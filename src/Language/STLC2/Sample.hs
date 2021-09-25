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
import qualified Language.STLC2 as STLC2

type GenM = ReaderT (Map STLC2.Type [STLC2.Term]) (FreshT Gen)

genTy :: forall m. MonadGen m => m STLC2.Type
genTy =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      pure STLC2.TyUnit,
      pure STLC2.TyBool
    ]
    [ -- recursive generators
      STLC2.TyFun <$> genTy <*> genTy
    ]

-- | Finalize generation by running the monad transformers for the environment
genWellTypedExp :: STLC2.Type -> Gen STLC2.Term
genWellTypedExp ty = runFreshT $ runReaderT (genWellTypedExp' ty) Map.empty

-- | Main recursive mechanism for genersating expressions for a given type.
genWellTypedExp' :: STLC2.Type -> GenM STLC2.Term
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

shrinkExp :: STLC2.Term -> [STLC2.Term]
shrinkExp (STLC2.TmApp f a) = fst . flip runReader STLC2.ids . runWriterT $ do
  f' <- STLC2.eval f
  case f' of
    STLC2.TmFun var _ b -> (: []) <$> (STLC2.subst var a b >>= STLC2.eval)
    _ -> pure []
shrinkExp _ = []

genWellTypedExp'' :: STLC2.Type -> GenM STLC2.Term
genWellTypedExp'' STLC2.TyUnit = pure STLC2.TmUnit
genWellTypedExp'' STLC2.TyBool = Gen.bool <&> bool STLC2.TmTrue STLC2.TmFalse
genWellTypedExp'' (STLC2.TyFun ty ty') = do
  var <- freshVar
  STLC2.TmFun var ty <$> local (insertVar var ty) (genWellTypedExp' ty')

freshVar :: GenM STLC2.Id
freshVar = lift . FreshT $ do
  i <- get
  modify succ
  pure (Text.pack $ 'x' : show i)

insertVar :: STLC2.Id -> STLC2.Type -> Map STLC2.Type [STLC2.Term] -> Map STLC2.Type [STLC2.Term]
insertVar x ty = Map.insertWith (<>) ty [STLC2.TmVar x] . fmap (List.filter (/= STLC2.TmVar x))

genWellTypedExp''' :: STLC2.Type -> GenM STLC2.Term
genWellTypedExp''' ty =
  let tmIf = do
        tm <- genWellTypedExp'' STLC2.TyBool
        tm' <- genWellTypedExp'' ty
        tm'' <- genWellTypedExp'' ty
        pure (STLC2.TmIf tm tm' tm'')
   in tmIf

genWellTypedApp :: STLC2.Type -> GenM STLC2.Term
genWellTypedApp ty = do
  tg <- genKnownTypeMaybe
  eg <- genWellTypedExp' tg
  let tf = STLC2.TyFun tg ty
  ef <- genWellTypedExp' tf
  pure (STLC2.TmApp ef eg)

-- | Try to look up a variable of the desired type from the environment.
-- This does not always succceed, throwing `empty` when unavailable.
genWellTypedPath :: STLC2.Type -> GenM STLC2.Term
genWellTypedPath ty = do
  paths <- ask
  case fromMaybe [] (Map.lookup ty paths) of
    [] -> empty
    es -> Gen.element es

-- | Generate either known types from the environment or new types.
genKnownTypeMaybe :: GenM STLC2.Type
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