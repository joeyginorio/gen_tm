{-# LANGUAGE TemplateHaskell #-}

module Language.STLC2CLSpec where

import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.CL (reduce)
import Language.STLC (evalR)
import Language.STLC2CL (compile)
import Language.STLCSpec (genTy, genWellTypedExp)

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll genTy
    tm <- forAll (genWellTypedExp ty)
    let tm' = evalR tm
        mcl = compile tm
    (reduce <$> mcl) === compile tm'

testSTLC2CL :: IO Bool
testSTLC2CL = checkParallel $$(discover)
