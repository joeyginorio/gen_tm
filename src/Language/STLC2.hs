{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- STLC.hs
   =======
   Defines syntax and semantics of STLC. -}

module Language.STLC2 where

import Control.Lens (makeLenses, scribe, over, _2)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Monoid
import Data.Set (Set, delete, empty, insert, member, union)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH

{- ================================= Syntax ================================= -}

-- | Identifiers are strings
type Id = Text

-- | Lambda terms
data Term = TmUnit                          -- ^ Unit              {Intro.}
          | TmTrue                          -- ^ True
          | TmFalse                         -- ^ False
          | TmVar  Id                       -- ^ Variables
          | TmFun  Id Type Term             -- ^ Functions
          | TmIf   Term Term Term           -- ^ If statements     {Elim.}
          | TmApp  Term Term                -- ^ Application
          deriving stock (Show, Eq, Ord)

-- | Lambda types
data Type = TyUnit                          -- ^ Unit
          | TyBool                          -- ^ Booleans
          | TyFun  Type Type                -- ^ Functions
          deriving stock (Show, Eq, Ord)

-- | Binding,
-- e.g., @x :: Bool => (x,Bool)@
type Binding = (Id, Type)
type Context = [Binding]

$(deriveJSON defaultOptions ''Type)
$(deriveJSON defaultOptions ''Term)


{- ================================ Semantics =============================== -}

--                                {Typechecker}

-- | Typechecking errors
data Error = EVar  Id         -- ^ Variable not in context
           | EIf1  Term       -- ^ First term isn't Bool type
           | EIf2  Term Term  -- ^ Second and third term aren't the same type
           | EFun1 Term Term  -- ^ Second term not valid iput to first term
           | EFun2 Term       -- ^ First term isn't a funtion
           deriving (Show, Eq)

-- | Typecheck type = 'Reader' + 'Either' monad stack.
--
--   (1) 'Reader' passes around the context.
--   (2) 'Either' passes around informative typecheck errors.
type TcType = ReaderT Context (Either Error) Type

-- | Typecheck terms
tyCheck :: Term -> TcType
tyCheck TmUnit             = return TyUnit
tyCheck TmTrue             = return TyBool
tyCheck TmFalse            = return TyBool
tyCheck (TmVar x)          = find x
tyCheck (TmFun x ty1 tm)   = do ty2 <- local ((x,ty1):) $ tyCheck tm
                                return $ TyFun ty1 ty2
tyCheck (TmIf tm1 tm2 tm3) = do ty1 <- tyCheck tm1
                                lift $ if ty1 == TyBool
                                         then Left $ EIf1 tm1
                                         else Right ()
                                ty2 <- tyCheck tm2
                                ty3 <- tyCheck tm3
                                lift $ if ty2 == ty3
                                         then Left $ EIf2 tm2 tm3
                                         else Right ()
                                return ty3
tyCheck (TmApp tm1 tm2)    = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                lift $ case ty1 of
                                         (TyFun ty11 ty12)
                                           | ty11 == ty2 -> Right ty12
                                           | otherwise   -> Left $ EFun1 tm1 tm2
                                         _               -> Left $ EFun2 tm1

find :: Id -> TcType
find x = do ctx <- ask
            lift $ case lookup x ctx of
                     Nothing -> Left $ EVar x
                     Just ty -> Right ty


--                                {Interpreter}

-- | Infinite list of fresh variable names
--
-- >>> take 10 ids
-- ["#0","#1","#2","#3","#4","#5","#6","#7","#8","#9"]
ids :: [Id]
ids = (\n -> Text.pack $ '#' : show n) <$> [0 :: Integer ..]

-- | Fresh variables in a term
fvs :: Term -> Set Id
fvs TmUnit             = empty
fvs TmTrue             = empty
fvs TmFalse            = empty
fvs (TmVar x)          = insert x empty
fvs (TmFun x _ tm)     = delete x $ fvs tm
fvs (TmIf tm1 tm2 tm3) = fvs tm1 `union` fvs tm2 `union` fvs tm3
fvs (TmApp tm1 tm2)    = fvs tm1 `union` fvs tm2

-- | alpha conversion of terms (renaming of variables).
--
-- @aconv x y tm@ means change all @x@ to @y@ in @tm@
aconv :: Id -> Id -> Term -> Term
aconv _ _ TmUnit             = TmUnit
aconv _ _ TmTrue             = TmTrue
aconv _ _ TmFalse            = TmFalse
aconv x y (TmVar z)          | x == z    = TmVar y
                             | otherwise = TmVar z
aconv x y (TmFun z ty tm)    | x == z    = TmFun y ty (aconv x y tm)
                             | otherwise = TmFun z ty (aconv x y tm)
aconv x y (TmApp tm1 tm2)    = TmApp (aconv x y tm1) (aconv x y tm2)
aconv x y (TmIf tm1 tm2 tm3) = TmIf (aconv x y tm1) (aconv x y tm2) (aconv x y tm3)

data EvalStats a = EvalStats
  { _evalStatsNumSteps :: a,
    _evalStatsNumStepsAppFun :: a,
    _evalStatsNumStepsIfTrueFalse :: a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (EvalStats a) where
  EvalStats a b c <> EvalStats a' b' c' = EvalStats (a <> a') (b <> b') (c <> c')

instance Monoid a => Monoid (EvalStats a) where
  mempty = EvalStats mempty mempty mempty

makeLenses ''EvalStats
$(deriveJSON defaultOptions ''EvalStats)

-- | Writer/reader term.
--
-- 'Reader' monad carries around fresh identifiers
-- 'Writer' monad tracks steps of computation
type WRTerm = WriterT (EvalStats (Sum Integer)) (Reader [Id]) Term

-- | Capture-avoiding substitution.
--
-- @s[x/t]@ means a term @s@ where all @x@ are replaced with @t@
subst :: Id -> Term -> Term -> WRTerm
subst _ _ TmUnit             = return TmUnit
subst _ _ TmTrue             = return TmTrue
subst _ _ TmFalse            = return TmFalse
subst x t (TmVar y)          | x == y    = return t
                             | otherwise = return $ TmVar y
subst x t s@(TmFun y ty tm)  | x == y           = return $ TmFun y ty tm
                             | member y (fvs t) = do ids' <- lift ask
                                                     let z  = head ids'
                                                     let s' = aconv y z s
                                                     tm' <- lift $ local tail (runWriterT $ subst x t s')
                                                     return $ fst tm'
                             | otherwise        = do tm' <- subst x t tm
                                                     return $ TmFun y ty tm'
subst x t (TmIf tm1 tm2 tm3) = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  tm3' <- subst x t tm3
                                  return $ TmIf tm1' tm2' tm3'
subst x t (TmApp tm1 tm2)    = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  return $ TmApp tm1' tm2'

-- | The actual interpreter, using call-by-name evaluation order
eval :: Term -> WRTerm
eval (TmIf TmTrue tm2 _)         = do scribe evalStatsNumSteps (Sum 1)
                                      scribe evalStatsNumStepsIfTrueFalse (Sum 1)
                                      eval tm2
eval (TmIf TmFalse _ tm3)        = do scribe evalStatsNumSteps (Sum 1)
                                      scribe evalStatsNumStepsIfTrueFalse (Sum 1)
                                      eval tm3
eval (TmIf tm1 tm2 tm3)          = do scribe evalStatsNumSteps (Sum 1)
                                      tm1' <- eval tm1
                                      eval $ TmIf tm1' tm2 tm3
eval (TmApp (TmFun x _ tm1) tm2) = do scribe evalStatsNumSteps (Sum 1)
                                      scribe evalStatsNumStepsAppFun (Sum 1)
                                      tm <- subst x tm2 tm1
                                      eval tm
eval (TmApp tm1 tm2)             = do scribe evalStatsNumSteps (Sum 1)
                                      tm1' <- eval tm1
                                      eval $ TmApp tm1' tm2
eval tm = return tm

-- | Runs eval with a default list of fresh variable names
evalWR :: Term -> (Term, EvalStats Integer)
evalWR = over _2 (fmap getSum) . flip runReader ids . runWriterT . eval

-- | Ignores writer output to just give output term
eval' :: Term -> Term
eval' = fst . evalWR

data TermStats a = TermStats
  { _termStatsNumUnit :: a,
    _termStatsNumTrue :: a,
    _termStatsNumFalse :: a,
    _termStatsNumFun :: a,
    _termStatsNumApp :: a,
    _termStatsNumIf :: a,
    _termStatsNumVar :: a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (TermStats a) where
  TermStats a b c d e f g <> TermStats a' b' c' d' e' f' g' = TermStats (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance Monoid a => Monoid (TermStats a) where
  mempty = TermStats mempty mempty mempty mempty mempty mempty mempty

makeLenses ''TermStats
$(deriveJSON defaultOptions ''TermStats)

countConstructors :: Term -> TermStats Integer
countConstructors = fmap getSum . execWriter . go
  where 
    go TmUnit = scribe termStatsNumUnit (Sum 1)
    go TmTrue = scribe termStatsNumTrue (Sum 1)
    go TmFalse = scribe termStatsNumFalse (Sum 1)
    go (TmFun _ _ tm) = do scribe termStatsNumFun (Sum 1)
                           go tm
    go (TmApp tm1 tm2) = do scribe termStatsNumApp (Sum 1)
                            go tm1
                            go tm2
    go (TmIf tm1 tm2 tm3) = do scribe termStatsNumIf (Sum 1)
                               go tm1
                               go tm2
                               go tm3
    go (TmVar _) = scribe termStatsNumVar (Sum 1)

-- | Convert a term to a Template Haskell expression
toTHExp :: Term -> TH.Exp
toTHExp = toTHExp' False

-- | Convert a term to a Template Haskell expression with type signature
toTHExpWithSig :: Term -> TH.Exp
toTHExpWithSig = toTHExp' True

toTHExp' :: Bool -> Term -> TH.Exp
toTHExp' _ TmUnit = TH.TupE []
toTHExp' _ TmTrue = TH.ConE $ TH.mkName "True"
toTHExp' _ TmFalse = TH.ConE $ TH.mkName "False"
toTHExp' _ (TmVar x) = TH.VarE $ TH.mkName $ Text.unpack x
toTHExp' withSig (TmFun x ty tm) = 
  let varP False name = TH.VarP name
      varP True name = TH.SigP (TH.VarP name) (toTHType ty)
  in TH.LamE [varP withSig $ TH.mkName $ Text.unpack x] $ toTHExp' withSig tm
-- if-then-else in Haskell needs to be nested due to conflicts with do notation and that leads to additional whitespace
-- toTHExp' withSig (TmIf tm1 tm2 tm3) = TH.CondE (toTHExp' withSig tm1) (toTHExp' withSig tm2) (toTHExp' withSig tm3)
-- instead we use if-then-else as a function, ite
toTHExp' withSig (TmIf tm1 tm2 tm3) = TH.AppE (TH.AppE (TH.AppE (TH.VarE $ TH.mkName "ite") (toTHExp' withSig tm1)) (toTHExp' withSig tm2)) (toTHExp' withSig tm3)
toTHExp' withSig (TmApp tm1 tm2) = TH.AppE (toTHExp' withSig tm1) (toTHExp' withSig tm2)

-- | Convert a type to a Template Haskell type
toTHType :: Type -> TH.Type
toTHType TyUnit = TH.TupleT 0
toTHType TyBool = TH.ConT (TH.mkName "Bool")
toTHType (TyFun ty ty') = TH.AppT (TH.AppT TH.ArrowT (toTHType ty)) (toTHType ty')

-- | Pretty-print a term using Haskell syntax
--
-- >>> pprintTerm $ TmApp (TmFun "x" TyBool (TmVar "x")) (TmApp (TmFun "y" TyBool (TmVar "y")) (TmApp (TmFun "z" TyBool (TmVar "z")) TmTrue))
-- "(\\x -> x) ((\\y -> y) ((\\z -> z) True))"
--
-- >>> pprintTerm $ TmIf (TmApp (TmFun "x" TyBool TmTrue) TmFalse) (TmFun "y" TyBool TmTrue) (TmFun "z" TyBool (TmVar "z"))
-- "ite ((\\x -> True) False) (\\y -> True) (\\z -> z)"
pprintTerm :: Term -> String
pprintTerm = TH.pprint . toTHExp

-- | Pretty-print a term using Haskell syntax with type signature
--
-- >>> pprintTermWithSig $ TmApp (TmFun "x" TyBool (TmVar "x")) (TmApp (TmFun "y" TyBool (TmVar "y")) (TmApp (TmFun "z" TyBool (TmVar "z")) TmTrue))
-- "(\\(x :: Bool) -> x) ((\\(y :: Bool) -> y) ((\\(z :: Bool) -> z) True))"
--
-- >>> pprintTermWithSig $ TmIf (TmApp (TmFun "x" TyBool TmTrue) TmFalse) (TmFun "y" TyBool TmTrue) (TmFun "z" TyBool (TmVar "z"))
-- "ite ((\\(x :: Bool) -> True) False) (\\(y :: Bool) -> True) (\\(z :: Bool) -> z)"
pprintTermWithSig :: Term -> String
pprintTermWithSig = TH.pprint . toTHExpWithSig

-- | Pretty-print a type using Haskell syntax
--
-- >>> pprintType $ TyBool
-- "Bool"

-- >>> pprintType $ TyFun TyBool TyBool
-- "Bool -> Bool"
--
-- >>> pprintType $ TyFun (TyFun TyUnit TyBool) TyBool 
-- "(() -> Bool) -> Bool"
pprintType :: Type -> String
pprintType = TH.pprint . toTHType
