{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.STLC3Eager where

import Bound (Scope, Var, abstract1, fromScope, instantiate1, toScope, (>>>=))
import Control.Lens (makeLenses, over, scribe, (^.), _2)
import Control.Monad (MonadPlus (mzero), ap, guard)
import Control.Monad.Fresh (Fresh, MonadFresh (fresh), runFresh, runFreshFrom)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.Lazy (Writer, execWriter, runWriter)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes (compare1, eq1, readsPrec1, showsPrec1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import GHC.Generics (Generic, Generic1)
import qualified Language.Haskell.TH as TH
import Prelude hiding (False, True, head, tail)

data Ty
  = -- | Arrow type (functions).
    TArr Ty Ty
  | -- | Unit type.
    TUnit
  | -- | Bool type.
    TBool
  | -- | List type.
    TList Ty
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Hashable)

data Exp a
  = -- | Variable.
    Var a
  | -- | Lambda abstraction.
    Lam {ty :: Ty, lamExp :: Scope () Exp a}
  | -- | Term application.
    (:@) {function :: Exp a, argument :: Exp a}
  | -- | Unit
    Unit
  | -- | True.
    True
  | -- | False.
    False
  | -- | If-then-else.
    If {condition :: Exp a, thenExp :: Exp a, elseExp :: Exp a}
  | -- | Nil.
    Nil {ty :: Ty}
  | -- | Cons.
    Cons {ty :: Ty, head :: Exp a, tail :: Exp a}
  | -- | Right fold.
    Foldr {step :: Exp a, initial :: Exp a, list :: Exp a}
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (Hashable, Hashable1)

infixl 9 :@

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var
  Var a >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam ty' e >>= f = Lam ty' (e >>>= f)
  Unit >>= _ = Unit
  True >>= _ = True
  False >>= _ = False
  If c t e >>= f = If (c >>= f) (t >>= f) (e >>= f)
  Nil ty' >>= _ = Nil ty'
  Cons ty' h t >>= f = Cons ty' (h >>= f) (t >>= f)
  Foldr s i l >>= f = Foldr (s >>= f) (i >>= f) (l >>= f)

instance FromJSON Ty

instance FromJSON a => FromJSON (Var () (Exp a))

instance FromJSON a => FromJSON (Scope () Exp a)

instance FromJSON a => FromJSON (Exp a)

instance ToJSON Ty

instance ToJSON a => ToJSON (Var () (Exp a))

instance ToJSON a => ToJSON (Scope () Exp a)

instance ToJSON a => ToJSON (Exp a)

deriveEq1 ''Exp
deriveOrd1 ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

instance Eq a => Eq (Exp a) where (==) = eq1

instance Ord a => Ord (Exp a) where compare = compare1

instance Show a => Show (Exp a) where showsPrec = showsPrec1

instance Read a => Read (Exp a) where readsPrec = readsPrec1

-- | Smart constructor for lambda terms
lam :: forall a. Eq a => Ty -> a -> Exp a -> Exp a
lam ty' uname bind = Lam ty' (abstract1 uname bind)

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

type EvalM a = Writer (EvalStats (Sum Int)) (Exp a)

-- | Compute the normal form of an expression.
-- Returned is the result obtained from applying eager evaluation.
nf :: Exp a -> EvalM a
nf e@Var {} = pure e
nf (Lam ty' b) = Lam ty' . toScope <$> nf (fromScope b)
nf (f :@ a) = do
  f' <- whnf f
  case f' of
    Lam _ty b -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      nf (instantiate1 a b)
    _ -> (:@) <$> nf f' <*> nf a
nf e@Unit = pure e
nf e@True = pure e
nf e@False = pure e
nf (If c t e) = do
  c' <- whnf c
  case c' of
    True -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      nf t
    False -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      nf e
    _ -> If <$> nf c' <*> nf t <*> nf e
nf e@Nil {} = pure e
nf (Cons ty' h t) = Cons ty' <$> nf h <*> nf t
nf (Foldr s i l) = do
  l' <- whnf l
  case l' of
    Nil {} -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      nf i
    Cons _ h t -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      h' <- nf h
      agg <- nf (Foldr s i t)
      nf (s :@ h' :@ agg)
    _ -> Foldr <$> nf s <*> nf i <*> nf l'

-- | Run writer monad to compute the normal form and evaluation statistics of an expression.
--
-- >>> nf' $ (:@) (lam TBool 'x' (Var 'x')) ((:@) (lam TBool 'y' (Var 'y')) ((:@) (lam TBool 'z' (Var 'z')) True))
-- (True,EvalStats {_evalStatsNumSteps = 3})
--
-- >>> nf' $ If ((:@) (lam TBool 'x' True) False) (lam TBool 'y' True) (lam TBool 'z' (Var 'z'))
-- (Lam {ty = TBool, lamExp = Scope True},EvalStats {_evalStatsNumSteps = 2})
--
-- >>> nf' $ Foldr (lam TBool 'a' ((:@) (lam TBool 'b' ((:@) (lam (TList TBool) 'f' ((:@) (Foldr (lam TBool 'g' (lam (TArr (TList TBool) (TArr (TList TBool) (TList TBool))) 'h' (Var 'h'))) (lam (TList TBool) 'i' (lam (TList TBool) 'j' (Var 'f'))) (Nil TBool)) (Var 'f'))) (Cons TBool (Var 'a') (Foldr (lam TBool 'c' (If False (lam (TList TBool) 'd' (Var 'd')) (lam (TList TBool) 'e' (Var 'e')))) (Nil TBool) (Cons TBool (Var 'a') (Nil TBool)))))) (Var 'a'))) (Cons TBool False ((:@) ((:@) (lam (TList TBool) 'k' ((:@) (Foldr (lam TBool 'l' (lam (TArr (TList TBool) (TArr TBool (TList TBool))) 'm' (Var 'm'))) (lam (TList TBool) 'n' (lam TBool 'o' (Var 'n'))) (Nil TBool)) (Var 'k'))) (Cons TBool False (Nil TBool))) True)) (Cons TBool ((:@) ((:@) (lam (TList TBool) 's' (lam (TList TBool) 't' ((:@) (lam (TList TBool) 'u' True) (Var 't')))) (Cons TBool False (Nil TBool))) (Foldr (lam TBool 'p' ((:@) (lam TBool 'q' (lam (TList TBool) 'r' (Var 'r'))) (Var 'p'))) (Cons TBool True (Nil TBool)) (Nil TBool))) (Nil TBool))
-- (Cons {ty = TBool, head = True, tail = Nil {ty = TBool}},EvalStats {_evalStatsNumSteps = 20})
--
-- >>> nf' $ Foldr (lam TBool 'x' (lam TBool 'y' (Var 'y'))) True (Cons TBool True (Cons TBool False (Nil TBool)))
-- (True,EvalStats {_evalStatsNumSteps = 7})
--
-- >>> let and = lam TBool 'x' (lam TBool 'y' (If (Var 'x') (Var 'y') (Var 'x'))) in nf' $ Foldr and True (Cons TBool True (Cons TBool True (Cons TBool False (Nil TBool))))
-- (False,EvalStats {_evalStatsNumSteps = 13})
--
-- >>> let or = lam TBool 'x' (lam TBool 'y' (If (Var 'x') (Var 'x') (Var 'y'))) in nf' $ Foldr or True (Cons TBool True (Cons TBool True (Cons TBool False (Nil TBool))))
-- (True,EvalStats {_evalStatsNumSteps = 13})
nf' :: forall a. Exp a -> (Exp a, EvalStats Int)
nf' = over _2 (fmap getSum) . runWriter . nf

-- | Reduce a term to weak head normal form.
-- Returned is the result obtained from applying lazy evaluation.
whnf :: forall a. Exp a -> EvalM a
whnf e@Var {} = pure e
whnf e@Lam {} = pure e
whnf (f :@ a) = do
  f' <- whnf f
  case f' of
    Lam _ty b -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf (instantiate1 a b)
    _ -> pure $ f' :@ a
whnf e@Unit = pure e
whnf e@True = pure e
whnf e@False = pure e
whnf (If c t e) = do
  c' <- whnf c
  case c' of
    True -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf t
    False -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf e
    _ -> pure $ If c' t e
whnf e@Nil {} = pure e
whnf e@Cons {} = pure e
whnf (Foldr s i l) = do
  l' <- whnf l
  case l' of
    Nil _ -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf i
    Cons _ h t -> do
      scribe (evalStatsNumSteps @(Sum Int)) (Sum 1)
      whnf $ s :@ h :@ Foldr s i t
    _ -> pure $ Foldr s i l'

-- | Run writer monad to compute the weak head normal form and evaluation statistics of an expression.
whnf' :: forall a. Exp a -> (Exp a, EvalStats Int)
whnf' = over _2 (fmap getSum) . runWriter . whnf

-- | Monad stack for type checking.
type TyM a = MaybeT (Fresh a)

-- | Guard against a type error.
assertTy :: Ord a => Map a Ty -> Exp a -> Ty -> TyM a ()
assertTy env e t = typeCheck env e >>= guard . (== t)

-- | Check the type of an expression.
typeCheck :: forall a. Ord a => Map a Ty -> Exp a -> TyM a Ty
typeCheck env (Var a) = MaybeT . return $ Map.lookup a env
typeCheck env (f :@ a) =
  typeCheck env f >>= \case
    TArr fTy tTy -> assertTy env a fTy >> return tTy
    _ -> mzero
typeCheck env (Lam ty' bind) = do
  uname <- fresh
  TArr ty' <$> typeCheck (Map.insert uname ty' env) (instantiate1 (Var uname) bind)
typeCheck _ Unit = return TUnit
typeCheck _ True = return TBool
typeCheck _ False = return TBool
typeCheck env (If c t e) = do
  assertTy env c TBool
  tTy <- typeCheck env t
  assertTy env e tTy
  return tTy
typeCheck _ (Nil ty') = return $ TList ty'
typeCheck env (Cons ty' h t) = do
  assertTy env h ty'
  assertTy env t (TList ty')
  return $ TList ty'
typeCheck env (Foldr s i l) =
  -- (ty' -> iTy -> iTy) -> iTy -> [ty'] -> iTy
  do
    iTy <- typeCheck env i
    TList ty' <- typeCheck env l
    assertTy env s (TArr ty' (TArr iTy iTy))
    return iTy

-- | Run the type checker monad to check the type of an expression.
--
-- >>> typeCheck' $ (:@) (lam TBool 'x' (Var 'x')) ((:@) (lam TBool 'y' (Var 'y')) ((:@) (lam TBool 'z' (Var 'z')) True))
-- Just TBool
--
-- >>> typeCheck' $ Foldr (lam TBool 'a' ((:@) (lam TBool 'b' ((:@) (lam (TList TBool) 'f' ((:@) (Foldr (lam TBool 'g' (lam (TArr (TList TBool) (TArr (TList TBool) (TList TBool))) 'h' (Var 'h'))) (lam (TList TBool) 'i' (lam (TList TBool) 'j' (Var 'f'))) (Nil TBool)) (Var 'f'))) (Cons TBool (Var 'a') (Foldr (lam TBool 'c' (If False (lam (TList TBool) 'd' (Var 'd')) (lam (TList TBool) 'e' (Var 'e')))) (Nil TBool) (Cons TBool (Var 'a') (Nil TBool)))))) (Var 'a'))) (Cons TBool False ((:@) ((:@) (lam (TList TBool) 'k' ((:@) (Foldr (lam TBool 'l' (lam (TArr (TList TBool) (TArr TBool (TList TBool))) 'm' (Var 'm'))) (lam (TList TBool) 'n' (lam TBool 'o' (Var 'n'))) (Nil TBool)) (Var 'k'))) (Cons TBool False (Nil TBool))) True)) (Cons TBool ((:@) ((:@) (lam (TList TBool) 's' (lam (TList TBool) 't' ((:@) (lam (TList TBool) 'u' True) (Var 't')))) (Cons TBool False (Nil TBool))) (Foldr (lam TBool 'p' ((:@) (lam TBool 'q' (lam (TList TBool) 'r' (Var 'r'))) (Var 'p'))) (Cons TBool True (Nil TBool)) (Nil TBool))) (Nil TBool))
-- Just (TList TBool)
typeCheck' :: forall a. (Ord a, Enum a) => Exp a -> Maybe Ty
typeCheck' = runFresh . runMaybeT . typeCheck Map.empty

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

countConstructors :: forall a. Exp a -> TermStats Int
countConstructors = fmap getSum . execWriter . go
  where
    go :: forall a'. Exp a' -> Writer (TermStats (Sum Int)) ()
    go Unit = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go True = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go False = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go (Var _) = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go (Lam _ b) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go (fromScope b)
    go (f :@ a) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go f
      go a
    go (If c t e) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go c
      go t
      go e
    go (Nil _) = scribe (termStatsNumNode @(Sum Int)) (Sum 1)
    go (Cons _ h t) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go h
      go t
    go (Foldr s i l) = do
      scribe (termStatsNumNode @(Sum Int)) (Sum 1)
      go s
      go i
      go l

updateTermHistogram :: TermStats Int -> TermStats (IntMap Int) -> TermStats (IntMap Int)
updateTermHistogram stats =
  over termStatsNumNode (IntMap.insertWith (+) (stats ^. termStatsNumNode) 1)

type TyTH a = Fresh a TH.Exp

-- | Convert a term to a Template Haskell expression
toTHExp :: forall a. Enum a => a -> Exp a -> TH.Exp
toTHExp = toTHExp' (\name _ -> TH.VarP name)

-- | Convert a term to a Template Haskell expression with type signature
toTHExpWithSig :: forall a. Enum a => a -> Exp a -> TH.Exp
toTHExpWithSig = toTHExp' (\name ty' -> TH.SigP (TH.VarP name) (toTHType ty'))

toTHExp' :: forall a. Enum a => (TH.Name -> Ty -> TH.Pat) -> a -> Exp a -> TH.Exp
toTHExp' varP a e = runFreshFrom a $ go e
  where
    toName a' = TH.mkName $ "x" <> (show . fromEnum $ a')

    unit = TH.mkName "()"
    true = TH.mkName "True"
    false = TH.mkName "False"
    ite = TH.mkName "ite"
    cons = TH.mkName ":"
    foldr' = TH.mkName "foldr"

    go :: Exp a -> TyTH a
    go (Var a') = pure . TH.VarE $ toName a'
    go (f :@ a') = TH.AppE <$> go f <*> go a'
    go (Lam ty' bind) = do
      a' <- fresh
      bind' <- go (instantiate1 (Var a') bind)
      return $ TH.LamE [varP (toName a') ty'] bind'
    go Unit = return $ TH.ConE unit
    go True = return $ TH.ConE true
    go False = return $ TH.ConE false
    go (If c t e') = do
      c' <- go c
      t' <- go t
      e'' <- go e'
      -- return $ TH.CondE c' t' e''
      return $ TH.AppE (TH.AppE (TH.AppE (TH.VarE ite) c') t') e''
    go (Nil _ty) = return $ TH.ListE []
    go (Cons _ty h t) = do
      t' <- go t
      case t' of
        TH.ListE exps -> do
          h' <- go h
          return $ TH.ListE $ (:) h' exps
        _ -> do
          h' <- go h
          return $ TH.AppE (TH.AppE (TH.ConE cons) h') t'
    go (Foldr s i l) = do
      s' <- go s
      i' <- go i
      l' <- go l
      return $ TH.AppE (TH.AppE (TH.AppE (TH.VarE foldr') s') i') l'

-- | Convert a type to a Template Haskell type
toTHType :: Ty -> TH.Type
toTHType TUnit = TH.TupleT 0
toTHType TBool = TH.ConT (TH.mkName "Bool")
toTHType (TArr ty' ty'') = TH.AppT (TH.AppT TH.ArrowT (toTHType ty')) (toTHType ty'')
toTHType (TList ty') = TH.AppT TH.ListT (toTHType ty')

-- | Pretty-print a term using Haskell syntax
--
-- >>> pprintTerm $ (:@) (lam TBool 'x' (Var 'x')) ((:@) (lam TBool 'y' (Var 'y')) ((:@) (lam TBool 'z' (Var 'z')) True))
-- "(\\x0 -> x0) ((\\x1 -> x1) ((\\x2 -> x2) True))"
--
-- >>> pprintTerm $ If ((:@) (lam TBool 'x' True) False) (lam TBool 'y' True) (lam TBool 'z' (Var 'z'))
-- "ite ((\\x0 -> True) False) (\\x1 -> True) (\\x2 -> x2)"
--
-- >>> pprintTerm $ Foldr (lam TBool 'x' (lam TBool 'y' (Var 'y'))) True (Cons TBool True (Cons TBool False (Nil TBool)))
-- "foldr (\\x0 -> \\x1 -> x1) True [True, False]"
--
-- >>> foldr (\x0 -> \x1 -> x1) True [True, False]
-- True
--
-- >>> pprintTerm $ Foldr (lam TBool 'a' ((:@) (lam TBool 'b' ((:@) (lam (TList TBool) 'f' ((:@) (Foldr (lam TBool 'g' (lam (TArr (TList TBool) (TArr (TList TBool) (TList TBool))) 'h' (Var 'h'))) (lam (TList TBool) 'i' (lam (TList TBool) 'j' (Var 'f'))) (Nil TBool)) (Var 'f'))) (Cons TBool (Var 'a') (Foldr (lam TBool 'c' (If False (lam (TList TBool) 'd' (Var 'd')) (lam (TList TBool) 'e' (Var 'e')))) (Nil TBool) (Cons TBool (Var 'a') (Nil TBool)))))) (Var 'a'))) (Cons TBool False ((:@) ((:@) (lam (TList TBool) 'k' ((:@) (Foldr (lam TBool 'l' (lam (TArr (TList TBool) (TArr TBool (TList TBool))) 'm' (Var 'm'))) (lam (TList TBool) 'n' (lam TBool 'o' (Var 'n'))) (Nil TBool)) (Var 'k'))) (Cons TBool False (Nil TBool))) True)) (Cons TBool ((:@) ((:@) (lam (TList TBool) 's' (lam (TList TBool) 't' ((:@) (lam (TList TBool) 'u' True) (Var 't')))) (Cons TBool False (Nil TBool))) (Foldr (lam TBool 'p' ((:@) (lam TBool 'q' (lam (TList TBool) 'r' (Var 'r'))) (Var 'p'))) (Cons TBool True (Nil TBool)) (Nil TBool))) (Nil TBool))
-- "foldr (\\x0 -> (\\x1 -> (\\x2 -> foldr (\\x3 -> \\x4 -> x4) (\\x5 -> \\x6 -> x2) [] x2) ((:) x0 (foldr (\\x7 -> ite False (\\x8 -> x8) (\\x9 -> x9)) [] [x0]))) x0) ((:) False ((\\x10 -> foldr (\\x11 -> \\x12 -> x12) (\\x13 -> \\x14 -> x13) [] x10) [False] True)) [(\\x15 -> \\x16 -> (\\x17 -> True) x16) [False] (foldr (\\x18 -> (\\x19 -> \\x20 -> x20) x18) [True] [])]"
--
-- >>> let ite p t f = if p then t else f in foldr (\x0 -> (\x1 -> (\x2 -> foldr (\x3 -> \x4 -> x4) (\x5 -> \x6 -> x2) [] x2) ((:) x0 (foldr (\x7 -> ite Prelude.False (\x8 -> x8) (\x9 -> x9)) [] [x0]))) x0) ((:) Prelude.False ((\x10 -> foldr (\x11 -> \x12 -> x12) (\x13 -> \x14 -> x13) [] x10) [Prelude.False] Prelude.True)) [(\x15 -> \x16 -> (\x17 -> Prelude.True) x16) [Prelude.False] (foldr (\x18 -> (\x19 -> \x20 -> x20) x18) [Prelude.True] [])]
-- [True]
--
-- >>> pprintTerm $ (:@) (lam (TList TBool) 'c' ((:@) (lam (TList TBool) 'd' Unit) (Var 'c'))) (Cons TBool (Foldr (lam TBool 'a' (lam TBool 'b' (Var 'b'))) True (Cons TBool True (Cons TBool True (Nil TBool)))) (Nil TBool))
-- "(\\x0 -> (\\x1 -> ()) x0) [foldr (\\x2 -> \\x3 -> x3) True [True, True]]"
pprintTerm :: forall a. Enum a => Exp a -> String
pprintTerm = unwords . words . TH.pprint . toTHExp (toEnum 0)

-- | Pretty-print a term with type signuatures using Haskell syntax
--
-- >>> pprintTermWithSig $ (:@) (lam TBool 'x' (Var 'x')) ((:@) (lam TBool 'y' (Var 'y')) ((:@) (lam TBool 'z' (Var 'z')) True))
-- "(\\(x0 :: Bool) -> x0) ((\\(x1 :: Bool) -> x1) ((\\(x2 :: Bool) -> x2) True))"
--
-- >>> pprintTermWithSig $ If ((:@) (lam TBool 'x' True) False) (lam TBool 'y' True) (lam TBool 'z' (Var 'z'))
-- "ite ((\\(x0 :: Bool) -> True) False) (\\(x1 :: Bool) -> True) (\\(x2 :: Bool) -> x2)"
--
-- >>> pprintTermWithSig $ Foldr (lam TBool 'x' (lam TBool 'y' (Var 'y'))) True (Cons TBool True (Cons TBool False (Nil TBool)))
-- "foldr (\\(x0 :: Bool) -> \\(x1 :: Bool) -> x1) True [True, False]"
--
-- >>> foldr (\x0 -> \x1 -> x1) True [True, False]
-- True
--
-- >>> pprintTermWithSig $ Foldr (lam TBool 'a' ((:@) (lam TBool 'b' ((:@) (lam (TList TBool) 'f' ((:@) (Foldr (lam TBool 'g' (lam (TArr (TList TBool) (TArr (TList TBool) (TList TBool))) 'h' (Var 'h'))) (lam (TList TBool) 'i' (lam (TList TBool) 'j' (Var 'f'))) (Nil TBool)) (Var 'f'))) (Cons TBool (Var 'a') (Foldr (lam TBool 'c' (If False (lam (TList TBool) 'd' (Var 'd')) (lam (TList TBool) 'e' (Var 'e')))) (Nil TBool) (Cons TBool (Var 'a') (Nil TBool)))))) (Var 'a'))) (Cons TBool False ((:@) ((:@) (lam (TList TBool) 'k' ((:@) (Foldr (lam TBool 'l' (lam (TArr (TList TBool) (TArr TBool (TList TBool))) 'm' (Var 'm'))) (lam (TList TBool) 'n' (lam TBool 'o' (Var 'n'))) (Nil TBool)) (Var 'k'))) (Cons TBool False (Nil TBool))) True)) (Cons TBool ((:@) ((:@) (lam (TList TBool) 's' (lam (TList TBool) 't' ((:@) (lam (TList TBool) 'u' True) (Var 't')))) (Cons TBool False (Nil TBool))) (Foldr (lam TBool 'p' ((:@) (lam TBool 'q' (lam (TList TBool) 'r' (Var 'r'))) (Var 'p'))) (Cons TBool True (Nil TBool)) (Nil TBool))) (Nil TBool))
-- "foldr (\\(x0 :: Bool) -> (\\(x1 :: Bool) -> (\\(x2 :: [Bool]) -> foldr (\\(x3 :: Bool) -> \\(x4 :: [Bool] -> [Bool] -> [Bool]) -> x4) (\\(x5 :: [Bool]) -> \\(x6 :: [Bool]) -> x2) [] x2) ((:) x0 (foldr (\\(x7 :: Bool) -> ite False (\\(x8 :: [Bool]) -> x8) (\\(x9 :: [Bool]) -> x9)) [] [x0]))) x0) ((:) False ((\\(x10 :: [Bool]) -> foldr (\\(x11 :: Bool) -> \\(x12 :: [Bool] -> Bool -> [Bool]) -> x12) (\\(x13 :: [Bool]) -> \\(x14 :: Bool) -> x13) [] x10) [False] True)) [(\\(x15 :: [Bool]) -> \\(x16 :: [Bool]) -> (\\(x17 :: [Bool]) -> True) x16) [False] (foldr (\\(x18 :: Bool) -> (\\(x19 :: Bool) -> \\(x20 :: [Bool]) -> x20) x18) [True] [])]"
--
-- >>> let ite p t f = if p then t else f in foldr (\(x0 :: Bool) -> (\(x1 :: Bool) -> (\(x2 :: [Bool]) -> foldr (\(x3 :: Bool) -> \(x4 :: [Bool] -> [Bool] -> [Bool]) -> x4) (\(x5 :: [Bool]) -> \(x6 :: [Bool]) -> x2) [] x2) ((:) x0 (foldr (\(x7 :: Bool) -> ite Prelude.False (\(x8 :: [Bool]) -> x8) (\(x9 :: [Bool]) -> x9)) [] [x0]))) x0) ((:) Prelude.False ((\(x10 :: [Bool]) -> foldr (\(x11 :: Bool) -> \(x12 :: [Bool] -> Bool -> [Bool]) -> x12) (\(x13 :: [Bool]) -> \(x14 :: Bool) -> x13) [] x10) [Prelude.False] Prelude.True)) [(\(x15 :: [Bool]) -> \(x16 :: [Bool]) -> (\(x17 :: [Bool]) -> Prelude.True) x16) [Prelude.False] (foldr (\(x18 :: Bool) -> (\(x19 :: Bool) -> \(x20 :: [Bool]) -> x20) x18) [Prelude.True] [])]
-- [True]
--
-- >>> pprintTermWithSig $ (:@) (lam (TList TBool) 'c' ((:@) (lam (TList TBool) 'd' Unit) (Var 'c'))) (Cons TBool (Foldr (lam TBool 'a' (lam TBool 'b' (Var 'b'))) True (Cons TBool True (Cons TBool True (Nil TBool)))) (Nil TBool))
-- "(\\(x0 :: [Bool]) -> (\\(x1 :: [Bool]) -> ()) x0) [foldr (\\(x2 :: Bool) -> \\(x3 :: Bool) -> x3) True [True, True]]"
pprintTermWithSig :: forall a. Enum a => Exp a -> String
pprintTermWithSig = unwords . words . TH.pprint . toTHExpWithSig (toEnum 0)

pprintType :: Ty -> String
pprintType = unwords . words . TH.pprint . toTHType
