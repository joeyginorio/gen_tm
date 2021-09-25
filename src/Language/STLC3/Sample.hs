{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC3.Sample where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Fresh (FreshT (..), runFreshT)
import Control.Monad.Reader (MonadReader (ask, local), MonadTrans (..), ReaderT (runReaderT), runReader)
import Control.Monad.State (MonadState (get, put), StateT, runStateT, modify)
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
import qualified Language.STLC3 as STLC3

type GenM = ReaderT (Map STLC3.Type [STLC3.Term]) (FreshT Gen)

genTy :: forall m. MonadGen m => m STLC3.Type
genTy =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      pure STLC3.TyUnit,
      pure STLC3.TyBool,
      pure STLC3.TyBList
    ]
    [ -- recursive generators
      STLC3.TyFun <$> genTy <*> genTy
    ]

-- | Finalize generation by running the monad transformers for the environment
genWellTypedExp :: STLC3.Type -> Gen STLC3.Term
genWellTypedExp ty = runFreshT $ runReaderT (genWellTypedExp' ty) Map.empty

-- | Main recursive mechanism for genersating expressions for a given type.
genWellTypedExp' :: STLC3.Type -> GenM STLC3.Term
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

shrinkExp :: STLC3.Term -> [STLC3.Term]
shrinkExp (STLC3.TmApp f a) = fst . flip runReader STLC3.ids . runWriterT $ do
  f' <- STLC3.eval f
  case f' of
    STLC3.TmFun var _ b -> (: []) <$> (STLC3.subst var a b >>= STLC3.eval)
    _ -> pure []
shrinkExp (STLC3.TmCons _ t) = [t]
shrinkExp _ = []

genWellTypedExp'' :: STLC3.Type -> GenM STLC3.Term
genWellTypedExp'' STLC3.TyUnit = pure STLC3.TmUnit
genWellTypedExp'' STLC3.TyBool = Gen.bool <&> bool STLC3.TmTrue STLC3.TmFalse
genWellTypedExp'' (STLC3.TyFun ty ty') = do
  var <- freshVar
  STLC3.TmFun var ty <$> local (insertVar var ty) (genWellTypedExp' ty')
genWellTypedExp'' STLC3.TyBList =
  Gen.shrink shrinkExp $
    Gen.recursive
      Gen.choice
      [ pure STLC3.TmNil
      ]
      [ STLC3.TmCons <$> genWellTypedExp' STLC3.TyBool <*> genWellTypedExp' STLC3.TyBList
      ]

freshVar :: GenM STLC3.Id
freshVar = lift . FreshT $ do
  i <- get
  modify succ
  pure (Text.pack $ 'x' : show i)

insertVar :: STLC3.Id -> STLC3.Type -> Map STLC3.Type [STLC3.Term] -> Map STLC3.Type [STLC3.Term]
insertVar x ty = Map.insertWith (<>) ty [STLC3.TmVar x] . fmap (List.filter (/= STLC3.TmVar x))

genWellTypedExp''' :: STLC3.Type -> GenM STLC3.Term
genWellTypedExp''' ty =
  let tmIf = do
        tm <- genWellTypedExp'' STLC3.TyBool
        tm' <- genWellTypedExp'' ty
        tm'' <- genWellTypedExp'' ty
        pure (STLC3.TmIf tm tm' tm'')
      tmFold = do
        tm <- genWellTypedExp'' (STLC3.TyFun STLC3.TyBool (STLC3.TyFun ty ty))
        tm' <- genWellTypedExp'' ty
        tm'' <- genWellTypedExp'' STLC3.TyBList
        pure (STLC3.TmFold tm tm' tm'')
   in Gen.choice [tmIf, tmFold]

genWellTypedApp :: STLC3.Type -> GenM STLC3.Term
genWellTypedApp ty = do
  tg <- genKnownTypeMaybe
  eg <- genWellTypedExp' tg
  let tf = STLC3.TyFun tg ty
  ef <- genWellTypedExp' tf
  pure (STLC3.TmApp ef eg)

-- | Try to look up a variable of the desired type from the environment.
-- This does not always succceed, throwing `empty` when unavailable.
genWellTypedPath :: STLC3.Type -> GenM STLC3.Term
genWellTypedPath ty = do
  paths <- ask
  case fromMaybe [] (Map.lookup ty paths) of
    [] -> empty
    es -> Gen.element es

-- | Generate either known types from the environment or new types.
genKnownTypeMaybe :: GenM STLC3.Type
genKnownTypeMaybe = do
  known <- ask
  if Map.null known
    then genTy
    else
      Gen.frequency
        [ (2, Gen.element $ Map.keys known),
          (1, genTy)
        ]

-- | Sample.
-- >>> flip runStateT (Seed.from 1) $ sample genTy
-- (TyBList,Seed 16204969531660614133 5610259966137620355)
--
-- >>> flip runStateT (Seed.from 6) $ sample $ genWellTypedExp STLC3.TyBList
-- Identity (TmFold (TmFun "x0" TyBool (TmApp (TmFun "x1" TyBool (TmApp (TmFun "x5" TyBList (TmApp (TmFold (TmFun "x6" TyBool (TmFun "x7" (TyFun TyBList (TyFun TyBList TyBList)) (TmVar "x7"))) (TmFun "x8" TyBList (TmFun "x9" TyBList (TmVar "x5"))) TmNil) (TmVar "x5"))) (TmCons (TmVar "x0") (TmFold (TmFun "x2" TyBool (TmIf TmFalse (TmFun "x3" TyBList (TmVar "x3")) (TmFun "x4" TyBList (TmVar "x4")))) TmNil (TmCons (TmVar "x0") TmNil))))) (TmVar "x0"))) (TmCons TmFalse (TmApp (TmApp (TmFun "x10" TyBList (TmApp (TmFold (TmFun "x11" TyBool (TmFun "x12" (TyFun TyBList (TyFun TyBool TyBList)) (TmVar "x12"))) (TmFun "x13" TyBList (TmFun "x14" TyBool (TmVar "x13"))) TmNil) (TmVar "x10"))) (TmCons TmFalse TmNil)) TmTrue)) (TmCons (TmApp (TmApp (TmFun "x18" TyBList (TmFun "x19" TyBList (TmApp (TmFun "x20" TyBList TmTrue) (TmVar "x19")))) (TmCons TmFalse TmNil)) (TmFold (TmFun "x15" TyBool (TmApp (TmFun "x16" TyBool (TmFun "x17" TyBList (TmVar "x17"))) (TmVar "x15"))) (TmCons TmTrue TmNil) TmNil)) TmNil),Seed 17676705464547183192 13115728398345486507)
--
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