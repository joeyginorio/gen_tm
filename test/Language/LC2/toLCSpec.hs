{-# LANGUAGE TemplateHaskell #-}

module Language.LC2.ToLCSpec where

import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import qualified Language.LC as LC
import qualified Language.LC2.ToLC as LC2
import qualified Language.LC2 as LC2
import qualified Language.STLC3.Sample as STLC3.Sample
import qualified Language.STLC3.ToLC2 as STLC3
import qualified Language.STLC3 as STLC3

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll STLC3.Sample.genTy
    tm <- forAll (STLC3.Sample.genWellTypedExp ty)
    let tm' = LC2.toLC . LC2.eval' . STLC3.toLC2 . STLC3.eval' $ tm
    LC.eval' (LC2.toLC $ STLC3.toLC2 tm) === LC.eval' tm'

testSTLC3ToLC2ToLC :: IO Bool
testSTLC3ToLC2ToLC = checkParallel $$(discover)
