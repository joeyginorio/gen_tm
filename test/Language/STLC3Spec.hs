{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC3Spec where

import Control.Monad.Reader
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.STLC3
import qualified Language.STLC3.Sample as Sample

prop_welltyped :: Property
prop_welltyped =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let ty' = runReaderT (tyCheck tm) []
    Right ty === ty'

prop_welltypedNormalForm :: Property
prop_welltypedNormalForm =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let tm' = eval' tm
    let ety' = runReaderT (tyCheck tm') []
    Right ty === ety'

testSTLC3 :: IO Bool
testSTLC3 = checkParallel $$(discover)
