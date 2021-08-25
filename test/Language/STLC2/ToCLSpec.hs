{-# LANGUAGE TemplateHaskell #-}

module Language.STLC2.ToCLSpec where

import Control.Monad (MonadPlus (mzero))
import Hedgehog (Property, checkParallel, discover, forAll, property, (===))
import Language.CL (Term (..), reduce)
import Language.STLC2 (eval')
import qualified Language.STLC2.Sample as Sample
import Language.STLC2.ToCL (compile)

prop_commutative :: Property
prop_commutative =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    let tm' = eval' tm
    (reduce <$> compile tm) === (reduce <$> compile tm')

prop_skk_identity :: Property
prop_skk_identity =
  property $ do
    ty <- forAll Sample.genTy
    tm <- forAll (Sample.genWellTypedExp ty)
    cl <- forAll (maybe mzero pure $ compile tm)
    reduce cl === reduce (App (App (App S K) K) cl)

testToCL :: IO Bool
testToCL = checkParallel $$(discover)
