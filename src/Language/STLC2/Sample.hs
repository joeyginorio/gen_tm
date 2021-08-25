{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC2.Sample where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Fresh (FreshT (..), runFreshT)
import Control.Monad.Reader (MonadReader (ask, local), MonadTrans (..), ReaderT (runReaderT), runReader)
import Control.Monad.State (MonadState (get, put), StateT, modify)
import Control.Monad.Trans.Writer.Lazy (WriterT (runWriterT))
import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Hedgehog (Gen, MonadGen (..))
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (GenT)
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import Language.STLC2 (Id, Term (..), Type (..), eval, ids, subst)

type GenM = ReaderT (Map Type [Term]) (FreshT Gen)

genTy :: forall m. MonadGen m => m Type
genTy =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      pure TyUnit,
      pure TyBool
    ]
    [ -- recursive generators
      TyFun <$> genTy <*> genTy
    ]

-- | Finalize generation by running the monad transformers for the environment
genWellTypedExp :: Type -> Gen Term
genWellTypedExp ty = runFreshT $ runReaderT (genWellTypedExp' ty) Map.empty

-- | Main recursive mechanism for genersating expressions for a given type.
genWellTypedExp' :: Type -> GenM Term
genWellTypedExp' ty =
  Gen.shrink shrinkExp $
    genWellTypedPath ty
      <|> Gen.recursive
        Gen.choice
        [ -- non-recursive generators
          genWellTypedExp'' ty
        ]
        [ -- recursive generators
          genWellTypedApp ty,
          genWellTypedExp' ty,
          genWellTypedExp''' ty
        ]

shrinkExp :: Term -> [Term]
shrinkExp (TmApp f a) = fst . flip runReader ids . runWriterT $ do
  f' <- eval f
  case f' of
    TmFun var _ b -> (: []) <$> (subst var a b >>= eval)
    _ -> pure []
shrinkExp _ = []

genWellTypedExp'' :: Type -> GenM Term
genWellTypedExp'' TyUnit = pure TmUnit
genWellTypedExp'' TyBool = Gen.bool <&> bool TmTrue TmFalse
genWellTypedExp'' (TyFun ty ty') = do
  var <- freshVar
  TmFun var ty <$> local (insertVar var ty) (genWellTypedExp' ty')

freshVar :: GenM Id
freshVar = lift . FreshT $ do
  i <- get
  modify succ
  pure (Text.pack $ '#' : show i)

insertVar :: Id -> Type -> Map Type [Term] -> Map Type [Term]
insertVar x ty = Map.insertWith (<>) ty [TmVar x] . fmap (List.filter (/= TmVar x))

genWellTypedExp''' :: Type -> GenM Term
genWellTypedExp''' ty =
  let tmIf = do
        tm <- genWellTypedExp'' TyBool
        tm' <- genWellTypedExp'' ty
        tm'' <- genWellTypedExp'' ty
        pure (TmIf tm tm' tm'')
   in tmIf

genWellTypedApp :: Type -> GenM Term
genWellTypedApp ty = do
  tg <- genKnownTypeMaybe
  eg <- genWellTypedExp' tg
  let tf = TyFun tg ty
  ef <- genWellTypedExp' tf
  pure (TmApp ef eg)

-- | Try to look up a variable of the desired type from the environment.
-- This does not always succceed, throwing `empty` when unavailable.
genWellTypedPath :: Type -> GenM Term
genWellTypedPath ty = do
  paths <- ask
  case fromMaybe [] (Map.lookup ty paths) of
    [] -> empty
    es -> Gen.element es

-- | Generate either known types from the environment or new types.
genKnownTypeMaybe :: GenM Type
genKnownTypeMaybe = do
  known <- ask
  if Map.null known
    then genTy
    else
      Gen.frequency
        [ (2, Gen.element $ Map.keys known),
          (1, genTy)
        ]

sample :: forall m a. Monad m => GenT m a -> StateT Seed.Seed m a
sample gen =
  let go :: StateT Seed.Seed m a
      go = do
        seed <- get
        let (seed', seed'') = Seed.split seed
        put seed''
        Tree.NodeT x _ <- lift . Tree.runTreeT . Gen.evalGenT 30 seed' $ gen
        maybe go pure x
   in go