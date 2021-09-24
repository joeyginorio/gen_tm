{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.LC2 where

import Control.Lens (makeLenses, over, scribe, (^.), _2)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Control.Monad.Trans.Writer.Lazy (WriterT (runWriterT), execWriter)
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as TH
import Data.Hashable (Hashable)

type Id = Text

-- | Syntax of lambda calculus
data Term
  = TmVar Id
  | TmFun Id Term
  | TmApp Term Term
  | TmUnit
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

$(deriveJSON defaultOptions ''Term)

ids :: [Id]
ids = (\n -> Text.pack $ '#' : show n) <$> [0 :: Integer ..]

-- | Fresh variables in a term
fvs :: Term -> Set Id
fvs (TmVar x) = Set.insert x Set.empty
fvs (TmFun x tm) = Set.delete x $ fvs tm
fvs (TmApp tm1 tm2) = fvs tm1 `Set.union` fvs tm2

-- | alpha conversion of terms (renaming of variables).
--
-- @aconv x y tm@ means change all @x@ to @y@ in @tm@
aconv :: Id -> Id -> Term -> Term
aconv x y (TmVar z)
  | x == z = TmVar y
  | otherwise = TmVar z
aconv x y (TmFun z tm)
  | x == z = TmFun y (aconv x y tm)
  | otherwise = TmFun z (aconv x y tm)
aconv x y (TmApp tm1 tm2) = TmApp (aconv x y tm1) (aconv x y tm2)

data EvalStats a = EvalStats
  { _evalStatsNumSteps :: a,
    _evalStatsNumStepsAppFun :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (EvalStats a) where
  EvalStats a b <> EvalStats a' b' = EvalStats (a <> a') (b <> b')

instance Monoid a => Monoid (EvalStats a) where
  mempty = EvalStats mempty mempty

makeLenses ''EvalStats
$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''EvalStats)

-- | Writer/reader term.
--
-- 'Reader' monad carries around fresh identifiers
-- 'Writer' monad tracks steps of computation
type WRTerm = WriterT (EvalStats (Sum Int)) (Reader [Id]) Term

-- | Capture-avoiding substitution.
--
-- @s[x/t]@ means a term @s@ where all @x@ are replaced with @t@
subst :: Id -> Term -> Term -> WRTerm
subst x t (TmVar y)
  | x == y = return t
  | otherwise = return $ TmVar y
subst x t s@(TmFun y tm)
  | x == y = return $ TmFun y tm
  | Set.member y (fvs t) = do
    ids' <- lift ask
    let z = head ids'
    let s' = aconv y z s
    tm' <- lift $ local tail (runWriterT $ subst x t s')
    return $ fst tm'
  | otherwise = do
    tm' <- subst x t tm
    return $ TmFun y tm'
subst x t (TmApp tm1 tm2) = do
  tm1' <- subst x t tm1
  tm2' <- subst x t tm2
  return $ TmApp tm1' tm2'

-- | The actual interpreter, using call-by-name evaluation order
eval :: Term -> WRTerm
eval (TmApp (TmFun x tm1) tm2) = do
  scribe evalStatsNumSteps (Sum 1)
  scribe evalStatsNumStepsAppFun (Sum 1)
  tm <- subst x tm2 tm1
  eval tm
eval (TmApp tm1 tm2) = do
  scribe evalStatsNumSteps (Sum 1)
  tm1' <- eval tm1
  eval $ TmApp tm1' tm2
eval tm = return tm

-- | Runs eval with a default list of fresh variable names
evalWR :: Term -> (Term, EvalStats Int)
evalWR = over _2 (fmap getSum) . flip runReader ids . runWriterT . eval

-- | Ignores writer output to just give output term
eval' :: Term -> Term
eval' = fst . evalWR

updateEvalHistogram :: EvalStats Int -> EvalStats (IntMap Int) -> EvalStats (IntMap Int)
updateEvalHistogram stats =
  over evalStatsNumSteps (IntMap.insertWith (+) (stats ^. evalStatsNumSteps) 1)
    . over evalStatsNumStepsAppFun (IntMap.insertWith (+) (stats ^. evalStatsNumStepsAppFun) 1)

data TermStats a = TermStats
  { _termStatsNumFun :: a,
    _termStatsNumApp :: a,
    _termStatsNumVar :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (TermStats a) where
  TermStats a b c <> TermStats a' b' c' = TermStats (a <> a') (b <> b') (c <> c')

instance Monoid a => Monoid (TermStats a) where
  mempty = TermStats mempty mempty mempty

makeLenses ''TermStats
$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''TermStats)

countConstructors :: Term -> TermStats Int
countConstructors = fmap getSum . execWriter . go
  where
    go (TmFun _ tm) = do
      scribe termStatsNumFun (Sum 1)
      go tm
    go (TmApp tm1 tm2) = do
      scribe termStatsNumApp (Sum 1)
      go tm1
      go tm2
    go (TmVar _) = scribe termStatsNumVar (Sum 1)

updateTermHistogram :: TermStats Int -> TermStats (IntMap Int) -> TermStats (IntMap Int)
updateTermHistogram stats =
  over termStatsNumFun (IntMap.insertWith (+) (stats ^. termStatsNumFun) 1)
    . over termStatsNumApp (IntMap.insertWith (+) (stats ^. termStatsNumApp) 1)
    . over termStatsNumVar (IntMap.insertWith (+) (stats ^. termStatsNumVar) 1)

-- | Convert a term to a Template Haskell expression
toTHExp :: Term -> TH.Exp
toTHExp (TmVar x) = TH.VarE $ TH.mkName $ Text.unpack x
toTHExp (TmFun x tm) = TH.LamE [TH.VarP $ TH.mkName $ Text.unpack x] $ toTHExp tm
toTHExp (TmApp tm1 tm2) = TH.AppE (toTHExp tm1) (toTHExp tm2)

-- | Pretty-print a term using Haskell syntax
--
-- >>> pprintTerm $ TmApp (TmFun "x" (TmVar "x")) (TmFun "y" (TmVar "y"))
-- "(\\x -> x) (\\y -> y)"
pprintTerm :: Term -> String
pprintTerm = TH.pprint . toTHExp
