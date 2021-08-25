{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC2Spec where

import Control.Monad.Reader
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.STLC2
import qualified Language.STLC2.Sample as Sample

prop_welltyped :: Property
prop_welltyped =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let (Right ty') = runReaderT (tyCheck tm) []
    ty === ty'

prop_welltypedNormalForm :: Property
prop_welltypedNormalForm =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let tm' = eval' tm
    let ety' = runReaderT (tyCheck tm') []
    Right ty === ety'

testSTLC :: IO Bool
testSTLC = checkParallel $$(discover)
