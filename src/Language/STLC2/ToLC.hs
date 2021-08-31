{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.STLC2.ToLC where

import qualified Language.LC as LC
import qualified Language.STLC2 as STLC2

-- | STLC to LC (applying type erasure and desugaring).
--
-- Here we exploit two properties of the simply typed lambda calculus, that
-- we can "erase" types without impacting run-time behavior of its programs,
-- known as type-erasure. The second property is its basic data (bool, pairs)
-- can be desugared into the pure untyped lambda calculus.
toLC :: STLC2.Term -> LC.Term
toLC STLC2.TmUnit = LC.TmFun "x" (LC.TmVar "x")
toLC STLC2.TmTrue = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "x"))
toLC STLC2.TmFalse = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "y"))
toLC (STLC2.TmVar x) = LC.TmVar x
-- toLC (STLC2.TmProd s t) = LC.TmFun "z" (LC.TmApp (LC.TmApp (LC.TmVar "z") s') t')
--   where
--     s' = toLC s
--     t' = toLC t
toLC (STLC2.TmFun x _ty t) = LC.TmFun x (toLC t)
toLC (STLC2.TmIf s t u) = LC.TmApp (LC.TmApp (toLC s) (toLC t)) (toLC u)
-- toLC (STLC2.TmFst s) = LC.TmApp s' t
--   where
--     s' = toLC s
--     t = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "x"))
-- toLC (STLC2.TmSnd s) = LC.TmApp s' t
--   where
--     s' = toLC s
--     t = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "y"))
toLC (STLC2.TmApp s t) = LC.TmApp (toLC s) (toLC t)