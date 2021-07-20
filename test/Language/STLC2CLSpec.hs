{-# LANGUAGE TemplateHaskell #-}

module Language.STLC2CLSpec where

import Control.Monad (MonadPlus (mzero))
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.CL (Term (..), reduce)
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

prop_skk_identity :: Property
prop_skk_identity =
  property $ do
    ty <- forAll genTy
    tm <- forAll (genWellTypedExp ty)
    cl <- forAll (maybe mzero pure $ compile tm)
    reduce cl === reduce (App (App (App S K) K) cl)

testSTLC2CL :: IO Bool
testSTLC2CL = checkParallel $$(discover)
