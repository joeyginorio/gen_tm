{-# LANGUAGE RankNTypes #-}

module Language.LC.ToLCEager where

import Bound (abstract1, closed)
import Data.Maybe (fromJust)
import qualified Language.LC as LC
import Language.LCEager (Exp (..))

toLCEagerExp :: forall k a. LC.Term -> Exp k a
toLCEagerExp tm = fromJust . closed $ go tm
  where
    go :: LC.Term -> Exp k LC.Id
    go (LC.TmVar a) = Var a
    go (LC.TmFun a tm') =
      let b = abstract1 a (go tm')
       in Lam b
    go (LC.TmApp tm1 tm2) = go tm1 :@ go tm2