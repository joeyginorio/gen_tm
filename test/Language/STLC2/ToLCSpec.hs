{-# LANGUAGE TemplateHaskell #-}

module Language.STLC2.ToLCSpec where

import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import qualified Language.LC as LC
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.Sample as STLC2.Sample
import qualified Language.STLC2.ToLC as STLC2

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll STLC2.Sample.genTy
    tm <- forAll (STLC2.Sample.genWellTypedExp ty)
    let tm' = STLC2.eval' tm
    LC.eval' (STLC2.toLC tm) === LC.eval' (STLC2.toLC tm')

testToLC :: IO Bool
testToLC = checkParallel $$(discover)
