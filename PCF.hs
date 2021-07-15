{- PCF.hs
   ======
   Defines syntax and semantics of PCF. -}

module PCF where

import Data.Set (Set, empty, delete, insert, union, member)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

-- efw
{- ================================= Syntax ================================= -}

data Term = TmUnit                          -- Unit              {Intro.}
          | TmTrue                          -- True
          | TmFalse                         -- False
          | TmVar  Id                       -- Variables
          | TmProd Term Term                -- Products
          | TmFun  Id Type Term             -- Functions
          | TmIf   Term Term Term           -- If statements
          | TmFst  Term                     -- First projection
          | TmSnd  Term                     -- Second projection
          | TmApp  Term Term                -- Application
          deriving (Show, Eq)

data Type = TyUnit                          -- Unit
          | TyBool                          -- Booleans
          | TyProd Type Type                -- Products
          | TyFun  Type Type                -- Functions
          deriving (Show, Eq)

type Binding = (Id, Type)                    -- e.g. x :: Bool => (x,Bool)
type Context = [Binding]


{- ================================ Semantics =============================== -}

--                                {Typechecker}

data Error = EVar  Id         -- Variable not in context
           | EIf1  Term       -- First term isn't Bool type
           | EIf2  Term Term  -- Second and third term aren't the same type
           | EProd Term       -- Term isn't product type
           | EFun1 Term Term  -- Second term not valid iput to first term
           | EFun2 Term       -- First term isn't a funtion
           deriving (Show)

-- Typecheck type = Reader + Either monad stack
-- (i) Reader passes around the context
-- (ii) Either passes around informative typecheck errors

type TcType = ReaderT Context (Either Error) Type

tyCheck :: Term -> TcType
tyCheck (TmUnit)           = return TyUnit
tyCheck (TmTrue)           = return TyBool
tyCheck (TmFalse)          = return TyBool
tyCheck (TmVar x)          = do ctx <- ask
                                ty  <- find x
                                return ty
tyCheck (TmProd tm1 tm2)   = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                return $ TyProd ty1 ty2
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
tyCheck (TmFst tm)         = do ty <- tyCheck tm
                                lift $ case ty of
                                         (TyProd _ _) -> Right ()
                                         _            -> Left $ EProd tm
                                return ty
tyCheck (TmSnd tm)         = do ty <- tyCheck tm
                                lift $ case ty of
                                         (TyProd _ _) -> Right ()
                                         _            -> Left $ EProd tm
                                return ty
tyCheck (TmApp tm1 tm2)    = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                lift $ case ty1 of
                                         (TyFun ty11 ty12)
                                           | ty11 == ty2 -> Right ty12
                                           | otherwise   -> Left $ EFun1 tm1 tm2
                                         _                 -> Left $ EFun2 tm1

find :: Id -> TcType
find x = do ctx <- ask
            lift $ case lookup x ctx of
                     Nothing -> Left $ EVar x
                     Just ty -> Right ty


--                                {Interpreter}

-- Identifiers are strings
type Id = String

-- Infinite list of fresh variable names
ids :: [Id]
ids = zipWith (:) cs nums
      where cs   = repeat '#'
            nums = map show [0..]

-- Fresh variables in a term
fvs :: Term -> Set Id
fvs (TmUnit)           = empty
fvs (TmTrue)           = empty
fvs (TmFalse)          = empty
fvs (TmVar x)          = insert x empty
fvs (TmProd tm1 tm2)   = union (fvs tm1) (fvs tm2)
fvs (TmFun x _ tm)     = delete x $ fvs tm
fvs (TmIf tm1 tm2 tm3) = union (fvs tm1) $ union (fvs tm2) (fvs tm3)
fvs (TmFst tm)         = fvs tm
fvs (TmSnd tm)         = fvs tm
fvs (TmApp tm1 tm2)    = union (fvs tm1) (fvs tm2)

-- alpha conversion of terms (renaming of variables)
-- aconv x y tm means change all x to y in tm
aconv :: Id -> Id -> Term -> Term
aconv x y (TmUnit)         = TmUnit
aconv x y (TmTrue)         = TmTrue
aconv x y (TmFalse)        = TmFalse
aconv x y (TmVar z)        | x == z    = TmVar y
                           | otherwise = TmVar z
aconv x y (TmProd tm1 tm2) = TmProd (aconv x y tm1) (aconv x y tm2)
aconv x y (TmFun z ty tm)  | x == z    = TmFun y ty (aconv x y tm)
                           | otherwise = TmFun z ty (aconv x y tm)
aconv x y (TmFst tm)       = TmFst (aconv x y tm)
aconv x y (TmSnd tm)       = TmSnd (aconv x y tm)
aconv x y (TmApp tm1 tm2)  = TmApp (aconv x y tm1) (aconv x y tm2)

-- Substituted term
-- Reader monad carries around fresh identifiers
type STerm = Reader [Id] Term

-- Capture-avoiding substitution
-- s[x/t] means a term s where all x are replaced with t
subst :: Id -> Term -> Term -> STerm
subst x t (TmUnit)           = return TmUnit
subst x t (TmTrue)           = return TmTrue
subst x t (TmFalse)          = return TmFalse
subst x t (TmVar y)          | x == y    = return t
                             | otherwise = return $ TmVar y
subst x t (TmProd tm1 tm2)   = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  return $ TmProd tm1' tm2'
subst x t s@(TmFun y ty tm)  | x == y           = return $ TmFun y ty tm
                             | member y (fvs t) = do ids <- ask
                                                     let z  = head ids
                                                     let s' = aconv y z s
                                                     local tail (subst x t s')
                             | otherwise        = do tm' <- subst x t tm
                                                     return $ TmFun y ty tm'
subst x t (TmIf tm1 tm2 tm3) = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  tm3' <- subst x t tm3
                                  return $ TmIf tm1' tm2' tm3'
subst x t (TmFst tm)         = do tm' <- subst x t tm
                                  return $ TmFst tm
subst x t (TmSnd tm)         = do tm' <- subst x t tm
                                  return $ TmSnd tm
subst x t (TmApp tm1 tm2)    = do tm1' <- subst x t tm1
                                  tm2' <- subst x t tm2
                                  return $ TmApp tm1' tm2'

-- The actual interpreter, using call-by-name evaluation order
eval :: Term -> STerm
eval (TmIf TmTrue tm2 _)         = eval tm2
eval (TmIf TmFalse _ tm3)        = eval tm3
eval (TmIf tm1 tm2 tm3)          = do tm1' <- eval tm1
                                      eval $ TmIf tm1 tm2 tm3
eval (TmFst (TmProd tm1 tm2))    = eval tm1
eval (TmFst tm)                  = do tm' <- eval tm
                                      eval $ TmFst tm'
eval (TmSnd (TmProd tm1 tm2))    = eval tm2
eval (TmSnd tm)                  = do tm' <- eval tm
                                      eval $ TmSnd tm'
eval (TmApp (TmFun x _ tm1) tm2) = subst x tm2 tm1
eval (TmApp tm1 tm2)             = do tm1' <- eval tm1
                                      eval $ TmApp tm1' tm2
eval tm = return tm
