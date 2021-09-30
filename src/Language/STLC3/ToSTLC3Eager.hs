{-# LANGUAGE RankNTypes #-}

module Language.STLC3.ToSTLC3Eager where

import Bound (abstract1, closed)
import Data.Maybe (fromJust)
import qualified Language.STLC3 as STLC3
import Language.STLC3Eager (Exp (..), Ty (..))
import Prelude hiding (False, True)

toSTLC3EagerExp :: forall k a. STLC3.Term -> Exp k a
toSTLC3EagerExp tm = fromJust . closed $ go tm
  where
    go :: STLC3.Term -> Exp k STLC3.Id
    go (STLC3.TmVar a) = Var a
    go (STLC3.TmFun a ty' tm') =
      let b = abstract1 a (go tm')
       in Lam (toSTLC3EagerTy ty') b
    go (STLC3.TmApp tm1 tm2) = go tm1 :@ go tm2
    go STLC3.TmUnit = Unit
    go STLC3.TmTrue = True
    go STLC3.TmFalse = False
    go (STLC3.TmIf tm1 tm2 tm3) = If (go tm1) (go tm2) (go tm3)
    go (STLC3.TmNil ty') = Nil (toSTLC3EagerTy ty')
    go (STLC3.TmCons ty' tm1 tm2) = Cons (toSTLC3EagerTy ty') (go tm1) (go tm2)
    go (STLC3.TmFold tm1 tm2 tm3) = Foldr (go tm1) (go tm2) (go tm3)

toSTLC3EagerTy :: forall k. STLC3.Type -> Ty k
toSTLC3EagerTy STLC3.TyUnit = TUnit
toSTLC3EagerTy STLC3.TyBool = TBool
toSTLC3EagerTy (STLC3.TyFun ty' ty'') = TArr (toSTLC3EagerTy ty') (toSTLC3EagerTy ty'')
toSTLC3EagerTy (STLC3.TyList ty') = TList (toSTLC3EagerTy ty')