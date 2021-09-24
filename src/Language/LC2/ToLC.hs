{-# LANGUAGE OverloadedStrings #-}

module Language.LC2.ToLC where

import qualified Language.LC as LC
import qualified Language.LC2 as LC2

toLC :: LC2.Term -> LC.Term
toLC LC2.TmUnit = LC.TmFun "x" (LC.TmVar "x")
toLC LC2.TmTrue = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "x"))
toLC LC2.TmFalse = LC.TmFun "x" (LC.TmFun "y" (LC.TmVar "y"))
toLC (LC2.TmVar x) = LC.TmVar x
toLC (LC2.TmFun x t) = LC.TmFun x (toLC t)
toLC (LC2.TmIf s t u) = LC.TmApp (LC.TmApp (toLC s) (toLC t)) (toLC u)
toLC (LC2.TmApp s t) = LC.TmApp (toLC s) (toLC t)