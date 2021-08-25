{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC.Sample where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadPlus (..), MonadReader (ask, local), MonadTrans (..), ReaderT (runReaderT), runReader)
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify)
import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Hedgehog (Gen, MonadGen (..), distributeT)
import qualified Hedgehog.Gen as Gen
import Language.STLC (Id, Term (..), Type (..), eval, ids, subst)

newtype FreshT m a = FreshT {unFreshT :: (StateT Int m) a}
  deriving stock (Functor)

runFreshT :: forall m a. Monad m => FreshT m a -> m a
runFreshT = flip evalStateT 0 . unFreshT

instance Monad m => Monad (FreshT m) where
  return = FreshT . return
  (FreshT m) >>= f = FreshT $ m >>= unFreshT . f

instance (Functor f, Monad f) => Applicative (FreshT f) where
  pure = FreshT . pure
  (FreshT f) <*> (FreshT a) = FreshT $ f <*> a

instance (Monad m, Functor m, MonadPlus m) => Alternative (FreshT m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (FreshT m) where
  mzero = FreshT mzero
  mplus (FreshT m) (FreshT m') = FreshT $ mplus m m'

instance MFunctor FreshT where
  hoist nat m = FreshT $ hoist nat (unFreshT m)

instance MonadTrans FreshT where
  lift = FreshT . lift

instance MonadGen m => MonadGen (FreshT m) where
  type GenBase (FreshT m) = FreshT (GenBase m)
  toGenT = hoist FreshT . distributeT . unFreshT . hoist toGenT
  fromGenT = hoist fromGenT . distributeT

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
      TyProd <$> genTy <*> genTy,
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
shrinkExp (TmApp f a) = flip runReader ids $ do
  f' <- eval f
  case f' of
    TmFun var _ b -> (: []) <$> (subst var a b >>= eval)
    _ -> pure []
shrinkExp _ = []

genWellTypedExp'' :: Type -> GenM Term
genWellTypedExp'' TyUnit = pure TmUnit
genWellTypedExp'' TyBool = Gen.bool <&> bool TmTrue TmFalse
genWellTypedExp'' (TyProd ty ty') = TmProd <$> genWellTypedExp' ty <*> genWellTypedExp' ty'
genWellTypedExp'' (TyFun ty ty') = do
  var <- freshVar
  TmFun var ty <$> local (insertVar var ty) (genWellTypedExp' ty')

freshVar :: GenM Id
freshVar = lift . FreshT $ do
  i <- get
  modify succ
  pure ('#' : show i)

insertVar :: Id -> Type -> Map Type [Term] -> Map Type [Term]
insertVar x ty = Map.insertWith (<>) ty [TmVar x] . fmap (List.filter (/= TmVar x))

genWellTypedExp''' :: Type -> GenM Term
genWellTypedExp''' ty =
  let tmFst = do
        ty' <- genKnownTypeMaybe
        tm <- genWellTypedExp'' (TyProd ty ty')
        pure (TmFst tm)
      tmSnd = do
        ty' <- genKnownTypeMaybe
        tm <- genWellTypedExp'' (TyProd ty' ty)
        pure (TmSnd tm)
      tmIf = do
        tm <- genWellTypedExp'' TyBool
        tm' <- genWellTypedExp'' ty
        tm'' <- genWellTypedExp'' ty
        pure (TmIf tm tm' tm'')
   in tmFst <|> tmSnd <|> tmIf

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
