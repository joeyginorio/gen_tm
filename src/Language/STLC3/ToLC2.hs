module Language.STLC3.ToLC2 where

import qualified Language.LC2 as LC2
import qualified Language.STLC3 as STLC3

toLC2 :: STLC3.Term -> LC2.Term
toLC2 STLC3.TmUnit = LC2.TmUnit
toLC2 STLC3.TmTrue = LC2.TmTrue
toLC2 STLC3.TmFalse = LC2.TmFalse
toLC2 (STLC3.TmVar x) = LC2.TmVar x
toLC2 (STLC3.TmFun x _ty t) = LC2.TmFun x (toLC2 t)
toLC2 (STLC3.TmIf s t u) = LC2.TmIf (toLC2 s) (toLC2 t) (toLC2 u)
toLC2 (STLC3.TmApp s t) = LC2.TmApp (toLC2 s) (toLC2 t)