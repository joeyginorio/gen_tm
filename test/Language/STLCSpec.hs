{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLCSpec where

import Control.Monad.Reader (ReaderT (runReaderT))
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.STLC (evalR, tyCheck)
import Language.STLC.Sample (genTy, genWellTypedExp)

prop_welltyped :: Property
prop_welltyped =
  property $ do
    ty <- forAll genTy
    tm <- forAll (genWellTypedExp ty)
    let (Right ty') = runReaderT (tyCheck tm) []
    ty === ty'

prop_welltypedNormalForm :: Property
prop_welltypedNormalForm =
  property $ do
    ty <- forAll genTy
    tm <- forAll (genWellTypedExp ty)
    let tm' = evalR tm
    let ety' = runReaderT (tyCheck tm') []
    Right ty === ety'

testSTLC :: IO Bool
testSTLC = checkParallel $$(discover)
