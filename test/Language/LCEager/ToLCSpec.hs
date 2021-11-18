{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LCEager.ToLCSpec where

import Control.Lens ((^.))
import Hedgehog (Property, checkSequential, discover, forAll, property, withTests, (===))
import Language.Eagerness (Eagerness (..))
import qualified Language.LC as LC
import qualified Language.LC.ToLCEager as LC
import qualified Language.LCEager as LCEager
import qualified Language.LCEager.ToLC as LCEager
import qualified Language.STLC3Eager as STLC3Eager
import qualified Language.STLC3Eager.Sample as STLC3Eager.Sample
import qualified Language.STLC3Eager.ToLCEager as STLC3Eager

prop_there_and_back_again :: Property
prop_there_and_back_again =
  withTests 1000 . property $ do
    ty <- forAll STLC3Eager.Sample.genTy
    e :: STLC3Eager.Exp 'Lazy LC.Id <- forAll (STLC3Eager.Sample.genWellTypedExp ty)
    let e' = STLC3Eager.toLCEager e
        tm = LCEager.toLCTerm e'
        e'' = LC.toLCEagerExp tm
    e' === e''

prop_same_normal_forms :: Property
prop_same_normal_forms =
  withTests 1000 . property $ do
    ty <- forAll STLC3Eager.Sample.genTy
    e :: STLC3Eager.Exp 'Lazy LC.Id <- forAll (STLC3Eager.Sample.genWellTypedExp ty)
    let e' = STLC3Eager.toLCEager e
        tm = LCEager.toLCTerm e'
    let (tm', stats''') = LC.evalWR tm
        e''' = LC.toLCEagerExp tm'
        (e'', stats'') = LCEager.whnf' e'
    e'' === e'''
    stats'' ^. LCEager.evalStatsNumSteps === stats''' ^. LC.evalStatsNumSteps

testLCEagerToLC :: IO Bool
testLCEagerToLC = checkSequential $$(discover)
