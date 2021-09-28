{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.STLC3Eager.ToLCEagerSpec where

import Hedgehog (Property, checkParallel, discover, forAll, property, (===), withTests, checkSequential)
import qualified Language.LCEager as LCEager
import qualified Language.STLC3Eager as STLC3Eager
import qualified Language.STLC3Eager.Sample as STLC3Eager.Sample
import qualified Language.STLC3Eager.ToLCEager as STLC3Eager

prop_commutative :: Property
prop_commutative =
  withTests 10000 . property $ do
    ty <- forAll STLC3Eager.Sample.genTy
    e :: STLC3Eager.Exp Int <- forAll (STLC3Eager.Sample.genWellTypedExp ty)
    let e' = fst . LCEager.nf' . STLC3Eager.toLCEager $ e
        e'' = fst . LCEager.nf' . STLC3Eager.toLCEager . fst . STLC3Eager.nf' $ e
    e' === e''

testSTLC3EagerToLCEager :: IO Bool
testSTLC3EagerToLCEager = checkSequential $$(discover)
