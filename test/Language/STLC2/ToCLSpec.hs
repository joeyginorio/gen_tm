{-# LANGUAGE TemplateHaskell #-}

module Language.STLC2.ToCLSpec where

import Control.Monad (MonadPlus (mzero))
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import qualified Language.CL as CL
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.Sample as STLC2.Sample
import qualified Language.STLC2.ToCL as STLC2

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll STLC2.Sample.genTy
    tm <- forAll (STLC2.Sample.genWellTypedExp ty)
    let tm' = STLC2.eval' tm
    (CL.reduce' <$> STLC2.compile tm) === (CL.reduce' <$> STLC2.compile tm')

prop_skk_identity :: Property
prop_skk_identity =
  property $ do
    ty <- forAll STLC2.Sample.genTy
    tm <- forAll (STLC2.Sample.genWellTypedExp ty)
    cl <- forAll (maybe mzero pure $ STLC2.compile tm)
    CL.reduce' cl === CL.reduce' (CL.App (CL.App (CL.App CL.S CL.K) CL.K) cl)

testSTLC2ToCL :: IO Bool
testSTLC2ToCL = checkParallel $$(discover)
