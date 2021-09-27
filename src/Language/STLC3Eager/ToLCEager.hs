{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.STLC3Eager.ToLCEager where

import Bound (closed, fromScope, toScope)
import Data.Maybe (fromJust)
import Language.LCEager (Exp (..), lam)
import qualified Language.STLC3Eager as STLC3Eager

toLCEager :: forall a. STLC3Eager.Exp a -> Exp a
toLCEager (STLC3Eager.Var a) = Var a
toLCEager (STLC3Eager.Lam _ b) = Lam . toScope . toLCEager . fromScope $ b
toLCEager (f STLC3Eager.:@ a) = toLCEager f :@ toLCEager a
toLCEager STLC3Eager.Unit = fromJust . closed $ lam "x" (Var "x")
toLCEager STLC3Eager.True = fromJust . closed $ lam "x" (lam "y" (Var "x"))
toLCEager STLC3Eager.False = fromJust . closed $ lam "x" (lam "y" (Var "y"))
toLCEager (STLC3Eager.If c t e) = toLCEager c :@ toLCEager t :@ toLCEager e
toLCEager STLC3Eager.Nil {} = fromJust . closed $ lam "c" (lam "n" (Var "n"))
toLCEager (STLC3Eager.Cons _ h t) =
  let h' = fromJust . closed $ toLCEager h
      t' = fromJust . closed $ toLCEager t
   in fromJust . closed $ lam "c" (lam "n" (Var "c" :@ h' :@ t' :@ Var "c" :@ Var "n"))
toLCEager (STLC3Eager.Foldr s i l) = toLCEager l :@ toLCEager s :@ toLCEager i
