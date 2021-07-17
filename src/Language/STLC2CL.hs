{- STLC2CL.hs
   ==========
   Translate from simply-typed lambda calculus (STLC) to combinatory logic (CL).
   Two stages:
     (i) Apply desugaring and type erasure in translation from STLC to the
         untyped lambda calculus.
     (ii) Standard translation from untyped lambda calculus to CL. -}

module Language.STLC2CL where

import Data.Set (Set, empty, delete, insert, union, member)
import qualified Language.STLC as ST
import qualified Language.CL as CL


{- ====================== Stage 1: Erasure + Desugar ======================== -}

-- Here we exploit two properties of the simply typed lambda calculus, that
-- we can "erase" types without impacting run-time behavior of its programs,
-- known as type-erasure. The second property is its basic data (bool, pairs)
-- can be desugared into the pure untyped lambda calculus.

type Id = String

-- Syntax of lambda calculus
data Term = TmVar Id
          | TmFun Id Term
          | TmApp Term Term
          deriving Show

-- STLC to LC (applying type erasure and desugaring)
toLC :: ST.Term -> Term
toLC (ST.TmUnit)      = TmFun "x" (TmVar "x")
toLC (ST.TmTrue)      = TmFun "x" (TmFun "y" (TmVar "x"))
toLC (ST.TmFalse)     = TmFun "x" (TmFun "y" (TmVar "y"))
toLC (ST.TmVar x)     = TmVar x
toLC (ST.TmProd s t)  = TmFun "z" (TmApp (TmApp (TmVar "z") s') t')
                        where s' = toLC s
                              t' = toLC t
toLC (ST.TmFun x _ t) = TmFun x (toLC t)
toLC (ST.TmIf s t u)  = TmApp (TmApp s' t') u'
                        where s' = toLC s
                              t' = toLC t
                              u' = toLC u
toLC (ST.TmFst s)     = TmApp s' t
                        where s' = toLC s
                              t  = TmFun "x" (TmFun "y" (TmVar "x"))
toLC (ST.TmSnd s)     = TmApp s' t
                        where s' = toLC s
                              t  = TmFun "x" (TmFun "y" (TmVar "y"))
toLC (ST.TmApp s t)   = TmApp s' t'
                        where s' = toLC s
                              t' = toLC t


{- ===================== Stage 2: Lambda elimination ======================== -}

data Term' = S'
           | K'
           | TmVar' Id
           | TmFun' Id Term'
           | TmApp' Term' Term'
           deriving Show

-- Translate lambda terms into a mixed representation of lambda / CL terms
-- Makes it simpler to implement the term rewriting algorithm for translating
-- lambda to CL terms.
toTerm' :: Term -> Term'
toTerm' (TmVar x) = TmVar' x
toTerm' (TmFun x t) = TmFun' x (toTerm' t)
toTerm' (TmApp s t) = TmApp' (toTerm' s) (toTerm' t)

-- Fresh variables in a term
fvs :: Term' -> Set Id
fvs (S')             = empty
fvs (K')             = empty
fvs (TmVar' x)       = insert x empty
fvs (TmFun' x tm)    = delete x $ fvs tm
fvs (TmApp' tm1 tm2) = union (fvs tm1) (fvs tm2)

-- Convert lambda' terms to cl' terms
-- NOTE: Term' is a representation of BOTH lambda and cl terms because the
--       translation algorithm introduces pseudo-lambda and pseudo-cl terms
--       at intermediate stages. e.g. TmFun "x" K is neither a lambda or cl term.
toCL' :: Term' -> Term'
toCL' (S')                  = S'
toCL' (K')                  = K'
toCL' (TmVar' x)            = TmVar' x
toCL' (TmApp' s t)          = TmApp' s' t'
                              where s' = toCL' s
                                    t' = toCL' t
toCL' (TmFun' x (TmVar' y)) | x == y = TmApp' (TmApp' S' K') K'
toCL' (TmFun' x t)          | not $ member x (fvs t) = TmApp' K' (toCL' t)
                            | otherwise = case t of
                                t'@(TmFun' _ _) -> toCL' (TmFun' x (toCL' t'))
                                (TmApp' s' t')  -> TmApp'
                                                   (TmApp' S' s'') t''
                                  where s'' = toCL' $ TmFun' x s'
                                        t'' = toCL' $ TmFun' x t'

-- Transforms pseudo CL terms to legit CL terms (maybe!)
-- If for some reason the translation algorithm fails to convert things
-- to CL, then the Maybe monad will catch it (by returning Nothing).
toCL :: Term' -> Maybe CL.Term
toCL (S')         = return CL.S
toCL (K')         = return CL.K
toCL (TmApp' s t) = do s' <- toCL s
                       t' <- toCL t
                       return $ CL.App s' t'
toCL _            = Nothing

-- Compile STLC to CL
-- 4 stages:
-- (i) toLC, takes STLC terms to lambda calculus
-- (ii) toTerm', takes lambda terms to pseudo-lambda terms
-- (iii) toCL', takes pseudo lambda terms to pseudo cl terms
-- (iv) toCL, takes pseudo cl terms to cl terms (maybe!)
compile :: ST.Term -> Maybe CL.Term
compile = toCL . toCL' . toTerm' . toLC
