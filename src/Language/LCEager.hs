{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.LCEager where

import Bound (Bound ((>>>=)), Scope, Var, abstract1, fromScope, instantiate1, toScope, (>>>=))
import Control.Lens (makeLenses, over, scribe, (^.), _2)
import Control.Monad (ap)
import Control.Monad.Fresh (Fresh, MonadFresh (fresh), runFreshFrom)
import Control.Monad.Trans.Writer.Lazy (Writer, execWriter, runWriter)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes (compare1, eq1, readsPrec1, showsPrec1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import GHC.Generics (Generic, Generic1)
import Language.Eagerness (Eagerness (..))
import qualified Language.Haskell.TH as TH
import Prelude hiding (False, True, abs, foldr, head, tail)

type Exp :: Eagerness -> Type -> Type
data Exp k a
  = Var a
  | Exp k a :@ Exp k a
  | Lam (Scope () (Exp k) a)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

infixl 9 :@

instance Applicative (Exp k) where
  pure = Var
  (<*>) = ap

instance Monad (Exp k) where
  return = Var
  Var a >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e >>= f = Lam (e >>>= f)

instance FromJSON a => FromJSON (Var () (Exp k a))

instance FromJSON a => FromJSON (Scope () (Exp k) a)

instance FromJSON a => FromJSON (Exp k a)

instance ToJSON a => ToJSON (Var () (Exp k a))

instance ToJSON a => ToJSON (Scope () (Exp k) a)

instance ToJSON a => ToJSON (Exp k a)

deriveEq1 ''Exp
deriveOrd1 ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

instance Eq a => Eq (Exp k a) where (==) = eq1

instance Ord a => Ord (Exp k a) where compare = compare1

instance Show a => Show (Exp k a) where showsPrec = showsPrec1

instance Read a => Read (Exp k a) where readsPrec = readsPrec1

-- | A smart constructor for Lam
--
-- >>> lam "y" (lam "x" (Var "x" :@ Var "y"))
-- Lam (Scope (Lam (Scope (Var (B ()) :@ Var (F (Var (B ())))))))
lam :: forall k a. Eq a => a -> Exp k a -> Exp k a
lam v b = Lam (abstract1 v b)

newtype EvalStats a = EvalStats
  { _evalStatsNumSteps :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (EvalStats a) where
  EvalStats numSteps <> EvalStats numSteps' = EvalStats (numSteps <> numSteps')

instance Monoid a => Monoid (EvalStats a) where
  mempty = EvalStats mempty

makeLenses ''EvalStats
$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''EvalStats)

type EvalM k a = Writer (EvalStats (Sum Int)) (Exp k a)

-- | Compute the normal form of an expression.
-- Returned is the result obtained from applying eager evaluation.
nf :: forall a. Exp 'Eager a -> EvalM 'Eager a
nf e@Var {} = pure e
nf (Lam b) = Lam . toScope <$> nf (fromScope b)
nf (f :@ a) = do
  f' <- whnf f
  case f' of
    Lam b -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      nf (instantiate1 a b)
    _ -> (:@) <$> nf f' <*> nf a

-- | Run writer monad to compute the normal form and evaluation statistics of an expression.
nf' :: forall a. Exp 'Eager a -> (Exp 'Eager a, EvalStats Int)
nf' = over _2 (fmap getSum) . runWriter . nf

-- | Reduce a term to weak head normal form.
-- Returned is the result obtained from applying lazy evaluation.
whnf :: forall k a. Exp k a -> EvalM k a
whnf e@Var {} = pure e
whnf e@Lam {} = pure e
whnf (f :@ a) = do
  f' <- whnf f
  case f' of
    Lam b -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf (instantiate1 a b)
    _ -> pure $ f' :@ a

-- | Run writer monad to compute the weak head normal form and evaluation statistics of an expression.
whnf' :: forall k a. Exp k a -> (Exp k a, EvalStats Int)
whnf' = over _2 (fmap getSum) . runWriter . whnf

updateEvalHistogram :: EvalStats Int -> EvalStats (IntMap Int) -> EvalStats (IntMap Int)
updateEvalHistogram stats =
  over evalStatsNumSteps (IntMap.insertWith (+) (stats ^. evalStatsNumSteps) 1)

newtype TermStats a = TermStats
  { _termStatsNumNode :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (TermStats a) where
  TermStats numNode <> TermStats numNode' = TermStats (numNode <> numNode')

instance Monoid a => Monoid (TermStats a) where
  mempty = TermStats mempty

makeLenses ''TermStats
$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''TermStats)

countConstructors :: forall k a. Exp k a -> TermStats Int
countConstructors = fmap getSum . execWriter . go
  where
    go :: forall a'. Exp k a' -> Writer (TermStats (Sum Int)) ()
    go (Var _) = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go (Lam b) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go (fromScope b)
    go (f :@ a) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go f
      go a

updateTermHistogram :: TermStats Int -> TermStats (IntMap Int) -> TermStats (IntMap Int)
updateTermHistogram stats =
  over termStatsNumNode (IntMap.insertWith (+) (stats ^. termStatsNumNode) 1)

type TyTH a = Fresh a TH.Exp

-- | Convert a term to a Template Haskell expression
toTHExp :: forall k a. Enum a => a -> Exp k a -> TH.Exp
toTHExp a e = runFreshFrom a $ go e
  where
    toName a' = TH.mkName $ "x" <> (show . fromEnum $ a')

    go :: Exp k a -> TyTH a
    go (Var a') = pure . TH.VarE $ toName a'
    go (f :@ a') = TH.AppE <$> go f <*> go a'
    go (Lam bind) = do
      a' <- fresh
      bind' <- go (instantiate1 (Var a') bind)
      return $ TH.LamE [TH.VarP $ toName a'] bind'

-- | Pretty-print a term using Haskell syntax
pprintTerm :: forall k a. Enum a => Exp k a -> String
pprintTerm = unwords . words . TH.pprint . toTHExp (toEnum 0)
