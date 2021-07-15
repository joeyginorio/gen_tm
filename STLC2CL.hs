{- STLC2CL.hs
   ==========
   Translate from simply-typed lambda calculus (STLC) to combinatory logic (CL).
   Two stages:
     (i) Apply desugaring and type erasure in translation from STLC to the
         untyped lambda calculus.
     (ii) Standard translation from untyped lambda calculus to CL. -}

module STLC2CL where

import Data.Set (Set, empty, delete, insert, union, member)
import qualified STLC as ST
import qualified CL as CL


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
