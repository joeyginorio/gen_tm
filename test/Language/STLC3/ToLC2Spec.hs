{-# LANGUAGE TemplateHaskell #-}

module Language.STLC3.ToLC2Spec where

import Control.Monad (guard)
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import qualified Language.LC2 as LC2
import qualified Language.STLC3 as STLC3
import qualified Language.STLC3.Sample as STLC3.Sample
import qualified Language.STLC3.ToLC2 as STLC3

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll STLC3.Sample.genTy
    guard
      ( case ty of
          STLC3.TyUnit -> True
          STLC3.TyBool -> True
          STLC3.TyFun _ _ -> True
          STLC3.TyList _ -> True
      )
    tm <- forAll (STLC3.Sample.genWellTypedExp ty)
    let tm' = STLC3.eval' tm
    LC2.eval' (STLC3.toLC2 tm) === LC2.eval' (STLC3.toLC2 tm')

testSTLC3ToLC2 :: IO Bool
testSTLC3ToLC2 = checkParallel $$(discover)
