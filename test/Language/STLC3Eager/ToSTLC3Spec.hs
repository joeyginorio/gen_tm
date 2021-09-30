{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.STLC3Eager.ToSTLC3Spec where

import Control.Lens ((^.))
import Hedgehog (Property, checkSequential, discover, forAll, property, withTests, (===))
import Language.Eagerness (Eagerness (..))
import qualified Language.STLC3 as STLC3
import qualified Language.STLC3.ToSTLC3Eager as STLC3
import qualified Language.STLC3Eager as STLC3Eager
import qualified Language.STLC3Eager.Sample as STLC3Eager.Sample
import qualified Language.STLC3Eager.ToSTLC3 as STLC3Eager

prop_there_and_back_again :: Property
prop_there_and_back_again =
  withTests 1000 . property $ do
    ty <- forAll STLC3Eager.Sample.genTy
    e :: STLC3Eager.Exp 'Lazy STLC3.Id <- forAll (STLC3Eager.Sample.genWellTypedExp ty)
    let tm = STLC3Eager.toSTLC3Term e
        e' = STLC3.toSTLC3EagerExp tm
    e === e'

prop_same_normal_forms :: Property
prop_same_normal_forms =
  withTests 1000 . property $ do
    ty <- forAll STLC3Eager.Sample.genTy
    e :: STLC3Eager.Exp 'Lazy STLC3.Id <- forAll (STLC3Eager.Sample.genWellTypedExp ty)
    let tm = STLC3Eager.toSTLC3Term e
        (tm', stats'') = STLC3.evalWR tm
        e'' = STLC3.toSTLC3EagerExp tm'
        (e', stats') = STLC3Eager.whnf' e
    e' === e''
    stats' ^. STLC3Eager.evalStatsNumSteps === stats'' ^. STLC3.evalStatsNumSteps

testSTLC3EagerToSTLC3 :: IO Bool
testSTLC3EagerToSTLC3 = checkSequential $$(discover)
