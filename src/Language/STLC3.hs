{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.STLC3 where

import Control.Lens (makeLenses, over, scribe, (^.), _2)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Reader (Reader, ReaderT (..), ask, local, runReader)
import Control.Monad.Trans.Writer.Lazy (WriterT (runWriterT), execWriter)
import Data.Aeson.TH (defaultOptions, deriveJSON, Options (fieldLabelModifier))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (intercalate)

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
          | TmNil                           -- ^ Nil
          | TmCons Term Term                -- ^ Cons
          | TmFold Term Term Term           -- ^ Fold
          deriving stock (Show, Eq, Ord, Generic)
          deriving anyclass (Hashable)

-- | Lambda types
data Type = TyUnit                          -- ^ Unit
          | TyBool                          -- ^ Booleans
          | TyFun   Type Type               -- ^ Functions
          | TyBList                         -- ^ List of Booleans
          deriving stock (Show, Eq, Ord, Generic)
          deriving anyclass (Hashable)

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
           | EFun1 Term Term  -- ^ Second term not valid input to first term
           | EFun2 Term       -- ^ First term isn't a function
           | ECons1 Term      -- ^ First term isn't a Boolean
           | ECons2 Term      -- ^ Second term isn't a Boolean list
           | EFold1 Term      -- ^ First term isn't a function from Bool
           | EFold2 Term Term -- ^ Second term isn't compatible with first
           | EFold3 Term      -- ^ Third term isn't a Boolean list
           deriving (Show, Eq)

-- | Typecheck type = 'Reader' + 'Either' monad stack.
--
--   (1) 'Reader' passes around the context.
--   (2) 'Either' passes around informative typecheck errors.
type TcType = ReaderT Context (Either Error) Type

-- | Typecheck terms
--
-- >>> flip runReaderT [] . tyCheck $ TmIf TmTrue TmUnit TmUnit
-- Right TyUnit
--
-- >>> flip runReaderT [] . tyCheck $ TmCons TmTrue (TmCons TmFalse TmNil)
-- Right TyBList
--
-- >>> flip runReaderT [] . tyCheck $ TmFun "x" TyBool (TmFun "y" TyUnit (TmVar "y"))
-- Right (TyFun TyBool (TyFun TyUnit TyUnit))
--
-- >>> flip runReaderT [] . tyCheck $ TmFold (TmFun "x" TyBool (TmFun "y" TyBool (TmVar "y"))) TmTrue (TmCons TmTrue (TmCons TmFalse TmNil))
-- Right TyBool
--
-- >>> flip runReaderT [] . tyCheck $ TmFold (TmFun "x0" TyBool (TmApp (TmFun "x1" TyBool (TmApp (TmFun "x5" TyBList (TmApp (TmFold (TmFun "x6" TyBool (TmFun "x7" (TyFun TyBList (TyFun TyBList TyBList)) (TmVar "x7"))) (TmFun "x8" TyBList (TmFun "x9" TyBList (TmVar "x5"))) TmNil) (TmVar "x5"))) (TmCons (TmVar "x0") (TmFold (TmFun "x2" TyBool (TmIf TmFalse (TmFun "x3" TyBList (TmVar "x3")) (TmFun "x4" TyBList (TmVar "x4")))) TmNil (TmCons (TmVar "x0") TmNil))))) (TmVar "x0"))) (TmCons TmFalse (TmApp (TmApp (TmFun "x10" TyBList (TmApp (TmFold (TmFun "x11" TyBool (TmFun "x12" (TyFun TyBList (TyFun TyBool TyBList)) (TmVar "x12"))) (TmFun "x13" TyBList (TmFun "x14" TyBool (TmVar "x13"))) TmNil) (TmVar "x10"))) (TmCons TmFalse TmNil)) TmTrue)) (TmCons (TmApp (TmApp (TmFun "x18" TyBList (TmFun "x19" TyBList (TmApp (TmFun "x20" TyBList TmTrue) (TmVar "x19")))) (TmCons TmFalse TmNil)) (TmFold (TmFun "x15" TyBool (TmApp (TmFun "x16" TyBool (TmFun "x17" TyBList (TmVar "x17"))) (TmVar "x15"))) (TmCons TmTrue TmNil) TmNil)) TmNil)
-- Right TyBList
tyCheck :: Term -> TcType
tyCheck TmUnit             = return TyUnit
tyCheck TmTrue             = return TyBool
tyCheck TmFalse            = return TyBool
tyCheck (TmVar x)          = find x
tyCheck (TmFun x ty1 tm)   = do ty2 <- local ((x,ty1):) $ tyCheck tm
                                return $ TyFun ty1 ty2
tyCheck (TmIf tm1 tm2 tm3) = do ty1 <- tyCheck tm1
                                lift $ if ty1 == TyBool
                                         then Right ()
                                         else Left $ EIf1 tm1
                                ty2 <- tyCheck tm2
                                ty3 <- tyCheck tm3
                                lift $ if ty2 == ty3
                                         then Right ()
                                         else Left $ EIf2 tm2 tm3
                                return ty3
tyCheck (TmApp tm1 tm2)    = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                lift $ case ty1 of
                                         (TyFun ty11 ty12)
                                           | ty11 == ty2 -> Right ty12
                                           | otherwise   -> Left $ EFun1 tm1 tm2
                                         _               -> Left $ EFun2 tm1
tyCheck TmNil              = return TyBList
tyCheck (TmCons tm1 tm2)   = do ty1 <- tyCheck tm1
                                lift $ if ty1 == TyBool
                                         then Right ()
                                         else Left $ ECons1 tm1
                                ty2 <- tyCheck tm2
                                lift $ if ty2 == TyBList
                                          then Right ()
                                          else Left $ ECons2 tm2
                                return TyBList
tyCheck (TmFold tm1 tm2 tm3) = -- (Bool -> ty2 -> ty2) -> ty2 -> [Bool] -> ty2
                               do ty1 <- tyCheck tm1
                                  ty2 <- tyCheck tm2
                                  ty3 <- tyCheck tm3
                                  lift $ if ty3 == TyBList
                                            then Right ()
                                            else Left $ EFold3 tm3
                                  lift $ case ty1 of
                                           (TyFun TyBool (TyFun ty21 ty22))
                                             | ty21 == ty2 && ty22 == ty2 -> Right ty2
                                             | otherwise   -> Left $ EFold2 tm1 tm2
                                           _ -> Left $ EFold1 tm1

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
fvs TmUnit             = Set.empty
fvs TmTrue             = Set.empty
fvs TmFalse            = Set.empty
fvs (TmVar x)          = Set.insert x Set.empty
fvs (TmFun x _ tm)     = Set.delete x $ fvs tm
fvs (TmIf tm1 tm2 tm3) = fvs tm1 `Set.union` fvs tm2 `Set.union` fvs tm3
fvs (TmApp tm1 tm2)    = fvs tm1 `Set.union` fvs tm2
fvs TmNil              = Set.empty
fvs (TmCons tm1 tm2)   = fvs tm1 `Set.union` fvs tm2
fvs (TmFold tm1 tm2 tm3) = fvs tm1 `Set.union` fvs tm2 `Set.union` fvs tm3

-- | alpha conversion of terms (renaming of variables).
--
-- @aconv x y tm@ means change all @x@ to @y@ in @tm@
aconv :: Id -> Id -> Term -> Term
aconv _ _ TmUnit               = TmUnit
aconv _ _ TmTrue               = TmTrue
aconv _ _ TmFalse              = TmFalse
aconv x y (TmVar z)            | x == z    = TmVar y
                               | otherwise = TmVar z
aconv x y (TmFun z ty tm)      | x == z    = TmFun y ty (aconv x y tm)
                               | otherwise = TmFun z ty (aconv x y tm)
aconv x y (TmApp tm1 tm2)      = TmApp (aconv x y tm1) (aconv x y tm2)
aconv x y (TmIf tm1 tm2 tm3)   = TmIf (aconv x y tm1) (aconv x y tm2) (aconv x y tm3)
aconv _ _ TmNil                = TmNil
aconv x y (TmCons tm1 tm2)     = TmCons (aconv x y tm1) (aconv x y tm2)
aconv x y (TmFold tm1 tm2 tm3) = TmFold (aconv x y tm1) (aconv x y tm2) (aconv x y tm3)

data EvalStats a = EvalStats
  { _evalStatsNumSteps :: a,
    _evalStatsNumStepsAppFun :: a,
    _evalStatsNumStepsIfTrueFalse :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (EvalStats a) where
  EvalStats a b c <> EvalStats a' b' c' = EvalStats (a <> a') (b <> b') (c <> c')

instance Monoid a => Monoid (EvalStats a) where
  mempty = EvalStats mempty mempty mempty

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
subst _ _ TmUnit             = return TmUnit
subst _ _ TmTrue             = return TmTrue
subst _ _ TmFalse            = return TmFalse
subst x t (TmVar y)          | x == y    = return t
                             | otherwise = return $ TmVar y
subst x t s@(TmFun y ty tm)  | x == y           = return $ TmFun y ty tm
                             | Set.member y (fvs t) = do ids' <- lift ask
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
subst _ _ TmNil              = return TmNil
subst x t (TmCons tm1 tm2)   = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  return $ TmCons tm1' tm2'
subst x t (TmFold tm1 tm2 tm3) = do tm1' <- subst x t tm1
                                    tm2' <- subst x t tm2
                                    tm3' <- subst x t tm3
                                    return $ TmFold tm1' tm2' tm3'

-- | The actual interpreter, using call-by-name evaluation order
eval :: Term -> WRTerm
eval (TmIf TmTrue tm2 _)               = do scribe evalStatsNumSteps (Sum 1)
                                            scribe evalStatsNumStepsIfTrueFalse (Sum 1)
                                            eval tm2
eval (TmIf TmFalse _ tm3)              = do scribe evalStatsNumSteps (Sum 1)
                                            scribe evalStatsNumStepsIfTrueFalse (Sum 1)
                                            eval tm3
eval (TmIf tm1 tm2 tm3)                = do scribe evalStatsNumSteps (Sum 1)
                                            tm1' <- eval tm1
                                            eval $ TmIf tm1' tm2 tm3
eval (TmApp (TmFun x _ tm1) tm2)       = do scribe evalStatsNumSteps (Sum 1)
                                            scribe evalStatsNumStepsAppFun (Sum 1)
                                            tm <- subst x tm2 tm1
                                            eval tm
eval (TmApp tm1 tm2)                   = do scribe evalStatsNumSteps (Sum 1)
                                            tm1' <- eval tm1
                                            eval $ TmApp tm1' tm2
eval (TmFold _ tm2 TmNil)              = do scribe evalStatsNumSteps (Sum 1)
                                            eval tm2
eval (TmFold tm1 tm2 (TmCons tm3 tm4)) = do scribe evalStatsNumSteps (Sum 1)
                                            eval $ TmApp (TmApp tm1 tm3) (TmFold tm1 tm2 tm4)
eval (TmFold tm1 tm2 tm3)              = do scribe evalStatsNumSteps (Sum 1)
                                            tm3' <- eval tm3
                                            eval $ TmFold tm1 tm2 tm3'
eval (TmCons tm1 tm2)                  = do scribe evalStatsNumSteps (Sum 1)
                                            TmCons <$> eval tm1 <*> eval tm2
eval tm = return tm

-- | Runs eval with a default list of fresh variable names
evalWR :: Term -> (Term, EvalStats Int)
evalWR = over _2 (fmap getSum) . flip runReader ids . runWriterT . eval

-- | Ignores writer output to just give output term
--
-- >>> eval' $ TmFold (TmFun "x0" TyBool (TmApp (TmFun "x1" TyBool (TmApp (TmFun "x5" TyBList (TmApp (TmFold (TmFun "x6" TyBool (TmFun "x7" (TyFun TyBList (TyFun TyBList TyBList)) (TmVar "x7"))) (TmFun "x8" TyBList (TmFun "x9" TyBList (TmVar "x5"))) TmNil) (TmVar "x5"))) (TmCons (TmVar "x0") (TmFold (TmFun "x2" TyBool (TmIf TmFalse (TmFun "x3" TyBList (TmVar "x3")) (TmFun "x4" TyBList (TmVar "x4")))) TmNil (TmCons (TmVar "x0") TmNil))))) (TmVar "x0"))) (TmCons TmFalse (TmApp (TmApp (TmFun "x10" TyBList (TmApp (TmFold (TmFun "x11" TyBool (TmFun "x12" (TyFun TyBList (TyFun TyBool TyBList)) (TmVar "x12"))) (TmFun "x13" TyBList (TmFun "x14" TyBool (TmVar "x13"))) TmNil) (TmVar "x10"))) (TmCons TmFalse TmNil)) TmTrue)) (TmCons (TmApp (TmApp (TmFun "x18" TyBList (TmFun "x19" TyBList (TmApp (TmFun "x20" TyBList TmTrue) (TmVar "x19")))) (TmCons TmFalse TmNil)) (TmFold (TmFun "x15" TyBool (TmApp (TmFun "x16" TyBool (TmFun "x17" TyBList (TmVar "x17"))) (TmVar "x15"))) (TmCons TmTrue TmNil) TmNil)) TmNil)
-- TmCons TmTrue TmNil
--
-- >>> eval' $ TmFold (TmFun "x" TyBool (TmFun "y" TyBool (TmVar "y"))) TmTrue (TmCons TmTrue (TmCons TmFalse TmNil))
-- TmTrue
--
-- >>> let and = TmFun "x" TyBool (TmFun "y" TyBool (TmIf (TmVar "x") (TmVar "y") (TmVar "x"))) in eval' $ TmFold and TmTrue (TmCons TmTrue (TmCons TmTrue (TmCons TmFalse TmNil)))
-- TmFalse
--
-- >>> let or = TmFun "x" TyBool (TmFun "y" TyBool (TmIf (TmVar "x") (TmVar "x") (TmVar "y"))) in eval' $ TmFold or TmFalse (TmCons TmTrue (TmCons TmTrue (TmCons TmFalse TmNil)))
-- TmTrue
eval' :: Term -> Term
eval' = fst . evalWR

updateEvalHistogram :: EvalStats Int -> EvalStats (IntMap Int) -> EvalStats (IntMap Int)
updateEvalHistogram stats =
  over evalStatsNumSteps (IntMap.insertWith (+) (stats ^. evalStatsNumSteps) 1)
    . over evalStatsNumStepsAppFun (IntMap.insertWith (+) (stats ^. evalStatsNumStepsAppFun) 1)
    . over evalStatsNumStepsIfTrueFalse (IntMap.insertWith (+) (stats ^. evalStatsNumStepsIfTrueFalse) 1)

data TermStats a = TermStats
  { _termStatsNumUnit :: a,
    _termStatsNumTrue :: a,
    _termStatsNumFalse :: a,
    _termStatsNumFun :: a,
    _termStatsNumApp :: a,
    _termStatsNumIf :: a,
    _termStatsNumVar :: a,
    _termStatsNumNil :: a,
    _termStatsNumCons :: a,
    _termStatsNumFold :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Hashable)

instance Semigroup a => Semigroup (TermStats a) where
  TermStats a b c d e f g h i j <> TermStats a' b' c' d' e' f' g' h' i' j' = TermStats (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j')

instance Monoid a => Monoid (TermStats a) where
  mempty = TermStats mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

makeLenses ''TermStats
$(deriveJSON defaultOptions {fieldLabelModifier = drop 10} ''TermStats)

countConstructors :: Term -> TermStats Int
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
    go TmNil = scribe termStatsNumNil (Sum 1)
    go (TmCons tm1 tm2) = do scribe termStatsNumCons (Sum 1)
                             go tm1
                             go tm2
    go (TmFold tm1 tm2 tm3) = do scribe termStatsNumFold (Sum 1)
                                 go tm1
                                 go tm2
                                 go tm3

updateTermHistogram :: TermStats Int -> TermStats (IntMap Int) -> TermStats (IntMap Int)
updateTermHistogram stats =
  over termStatsNumUnit (IntMap.insertWith (+) (stats ^. termStatsNumUnit) 1)
    . over termStatsNumTrue (IntMap.insertWith (+) (stats ^. termStatsNumTrue) 1)
    . over termStatsNumFalse (IntMap.insertWith (+) (stats ^. termStatsNumFalse) 1)
    . over termStatsNumFun (IntMap.insertWith (+) (stats ^. termStatsNumFun) 1)
    . over termStatsNumApp (IntMap.insertWith (+) (stats ^. termStatsNumApp) 1)
    . over termStatsNumIf (IntMap.insertWith (+) (stats ^. termStatsNumIf) 1)
    . over termStatsNumVar (IntMap.insertWith (+) (stats ^. termStatsNumVar) 1)
    . over termStatsNumNil (IntMap.insertWith (+) (stats ^. termStatsNumNil) 1)
    . over termStatsNumCons (IntMap.insertWith (+) (stats ^. termStatsNumCons) 1)
    . over termStatsNumFold (IntMap.insertWith (+) (stats ^. termStatsNumFold) 1)

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
toTHExp' withSig (TmIf tm1 tm2 tm3) =
  TH.AppE (TH.AppE (TH.AppE (TH.VarE $ TH.mkName "ite") (toTHExp' withSig tm1)) (toTHExp' withSig tm2)) (toTHExp' withSig tm3)
toTHExp' withSig (TmApp tm1 tm2) =
  TH.AppE (toTHExp' withSig tm1) (toTHExp' withSig tm2)
toTHExp' _ TmNil = TH.ListE []
toTHExp' withSig (TmCons tm1 tm2) =
  case toTHExp' withSig tm2 of
    TH.ListE exps -> TH.ListE $ (:) (toTHExp' withSig tm1) exps
    _ -> TH.AppE (TH.AppE (TH.ConE $ TH.mkName ":") (toTHExp' withSig tm1)) (toTHExp' withSig tm2)
toTHExp' withSig (TmFold tm1 tm2 tm3) =
  TH.AppE (TH.AppE (TH.AppE (TH.VarE $ TH.mkName "foldr") (toTHExp' withSig tm1)) (toTHExp' withSig tm2)) (toTHExp' withSig tm3)

-- | Convert a type to a Template Haskell type
toTHType :: Type -> TH.Type
toTHType TyUnit = TH.TupleT 0
toTHType TyBool = TH.ConT (TH.mkName "Bool")
toTHType (TyFun ty ty') = TH.AppT (TH.AppT TH.ArrowT (toTHType ty)) (toTHType ty')
toTHType TyBList = TH.AppT TH.ListT (toTHType TyBool)

-- | Pretty-print a term using Haskell syntax
--
-- >>> pprintTerm $ TmApp (TmFun "x" TyBool (TmVar "x")) (TmApp (TmFun "y" TyBool (TmVar "y")) (TmApp (TmFun "z" TyBool (TmVar "z")) TmTrue))
-- "(\\x -> x) ((\\y -> y) ((\\z -> z) True))"
--
-- >>> pprintTerm $ TmIf (TmApp (TmFun "x" TyBool TmTrue) TmFalse) (TmFun "y" TyBool TmTrue) (TmFun "z" TyBool (TmVar "z"))
-- "ite ((\\x -> True) False) (\\y -> True) (\\z -> z)"
--
-- >>> pprintTerm $ TmFold (TmFun "x" TyBool (TmFun "y" TyBool (TmVar "y"))) TmTrue (TmCons TmTrue (TmCons TmFalse TmNil))
-- "foldr (\\x -> \\y -> y) True [True, False]"
--
-- >>> foldr (\x -> \y -> y) True [True, False]
-- True
--
-- >>> pprintTerm $ TmFold (TmFun "x0" TyBool (TmApp (TmFun "x1" TyBool (TmApp (TmFun "x5" TyBList (TmApp (TmFold (TmFun "x6" TyBool (TmFun "x7" (TyFun TyBList (TyFun TyBList TyBList)) (TmVar "x7"))) (TmFun "x8" TyBList (TmFun "x9" TyBList (TmVar "x5"))) TmNil) (TmVar "x5"))) (TmCons (TmVar "x0") (TmFold (TmFun "x2" TyBool (TmIf TmFalse (TmFun "x3" TyBList (TmVar "x3")) (TmFun "x4" TyBList (TmVar "x4")))) TmNil (TmCons (TmVar "x0") TmNil))))) (TmVar "x0"))) (TmCons TmFalse (TmApp (TmApp (TmFun "x10" TyBList (TmApp (TmFold (TmFun "x11" TyBool (TmFun "x12" (TyFun TyBList (TyFun TyBool TyBList)) (TmVar "x12"))) (TmFun "x13" TyBList (TmFun "x14" TyBool (TmVar "x13"))) TmNil) (TmVar "x10"))) (TmCons TmFalse TmNil)) TmTrue)) (TmCons (TmApp (TmApp (TmFun "x18" TyBList (TmFun "x19" TyBList (TmApp (TmFun "x20" TyBList TmTrue) (TmVar "x19")))) (TmCons TmFalse TmNil)) (TmFold (TmFun "x15" TyBool (TmApp (TmFun "x16" TyBool (TmFun "x17" TyBList (TmVar "x17"))) (TmVar "x15"))) (TmCons TmTrue TmNil) TmNil)) TmNil)
-- "foldr (\\x0 -> (\\x1 -> (\\x5 -> foldr (\\x6 -> \\x7 -> x7) (\\x8 -> \\x9 -> x5) [] x5) ((:) x0 (foldr (\\x2 -> ite False (\\x3 -> x3) (\\x4 -> x4)) [] [x0]))) x0) ((:) False ((\\x10 -> foldr (\\x11 -> \\x12 -> x12) (\\x13 -> \\x14 -> x13) [] x10) [False] True)) [(\\x18 -> \\x19 -> (\\x20 -> True) x19) [False] (foldr (\\x15 -> (\\x16 -> \\x17 -> x17) x15) [True] [])]"
--
-- >>> let ite p t f = if p then t else f in foldr (\x0 -> (\x1 -> (\x5 -> foldr (\x6 -> \x7 -> x7) (\x8 -> \x9 -> x5) [] x5) ((:) x0 (foldr (\x2 -> ite False (\x3 -> x3) (\x4 -> x4)) [] [x0]))) x0) ((:) False ((\x10 -> foldr (\x11 -> \x12 -> x12) (\x13 -> \x14 -> x13) [] x10) [False] True)) [(\x18 -> \x19 -> (\x20 -> True) x19) [False] (foldr (\x15 -> (\x16 -> \x17 -> x17) x15) [True] [])]
-- [True]
--
-- >>> pprintTerm $ TmApp (TmFun "x2" TyBList (TmApp (TmFun "x3" TyBList TmUnit) (TmVar "x2"))) (TmCons (TmFold (TmFun "x0" TyBool (TmFun "x1" TyBool (TmVar "x1"))) TmTrue (TmCons TmTrue (TmCons TmTrue TmNil))) TmNil)
-- "(\\x2 -> (\\x3 -> ()) x2) [foldr (\\x0 -> \\x1 -> x1) True [True, True]]"
pprintTerm :: Term -> String
pprintTerm = unwords . words . TH.pprint . toTHExp

-- | Pretty-print a term using Haskell syntax with type signature
--
-- >>> pprintTermWithSig $ TmApp (TmFun "x" TyBool (TmVar "x")) (TmApp (TmFun "y" TyBool (TmVar "y")) (TmApp (TmFun "z" TyBool (TmVar "z")) TmTrue))
-- "(\\(x :: Bool) -> x) ((\\(y :: Bool) -> y) ((\\(z :: Bool) -> z) True))"
--
-- >>> pprintTermWithSig $ TmIf (TmApp (TmFun "x" TyBool TmTrue) TmFalse) (TmFun "y" TyBool TmTrue) (TmFun "z" TyBool (TmVar "z"))
-- "ite ((\\(x :: Bool) -> True) False) (\\(y :: Bool) -> True) (\\(z :: Bool) -> z)"
--
-- >>> pprintTermWithSig $ TmFold (TmFun "x" TyBool (TmFun "y" TyBool (TmVar "y"))) TmTrue (TmCons TmTrue (TmCons TmFalse TmNil))
-- "foldr (\\(x :: Bool) -> \\(y :: Bool) -> y) True [True, False]"
pprintTermWithSig :: Term -> String
pprintTermWithSig = unwords . words . TH.pprint . toTHExpWithSig

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
pprintType = unwords . words . TH.pprint . toTHType
