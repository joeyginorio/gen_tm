{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC3Spec where

import Control.Monad.Reader (ReaderT (runReaderT))
import Hedgehog (Property, checkParallel, diff, discover, forAll, property, (===))
import Language.STLC3 (eval', pprintTerm, pprintType, tyCheck)
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

prop_prettyTyNoNewlines :: Property
prop_prettyTyNoNewlines =
  property $ do
    ty <- forAll Sample.genTy
    let prettyTy = pprintType ty
    diff prettyTy (flip notElem) '\n'

prop_prettyTmNoNewlines :: Property
prop_prettyTmNoNewlines =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let prettyTm = pprintTerm tm
    diff prettyTm (flip notElem) '\n'

testSTLC3 :: IO Bool
testSTLC3 = checkParallel $$(discover)
