{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC3EagerSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (race)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Hedgehog (MonadTest, Property, checkSequential, diff, discover, forAll, property, withTests, (===))
import Language.STLC3Eager (Exp, nf', pprintTerm, pprintType, typeCheck')
import qualified Language.STLC3Eager.Sample as Sample

prop_welltyped :: Property
prop_welltyped =
  withTests 1000 . property $ do
    ty <- forAll Sample.genTy
    e :: Exp Int <- forAll (Sample.genWellTypedExp ty)
    let ty' = typeCheck' e
    Just ty === ty'

withTimeLimit :: (MonadTest m, MonadIO m, MonadBaseControl IO m, MonadFail m) => Int -> m a -> m a
withTimeLimit timeout v = do
  result <-
    race
      (liftIO $ threadDelay timeout)
      v
  case result of
    Left () -> fail "Timeout exceeded"
    Right x -> pure x

prop_welltypedNormalForm :: Property
prop_welltypedNormalForm =
  withTests 25000 . property $ do
    ty <- forAll Sample.genTy
    e :: Exp Int <- forAll (Sample.genWellTypedExp ty)
    let e' = fst $ nf' e
    let ty' = typeCheck' e'
    withTimeLimit 10000 $ Just ty === ty'

prop_prettyTyNoNewlines :: Property
prop_prettyTyNoNewlines =
  withTests 10000 . property $ do
    ty <- forAll Sample.genTy
    let prettyTy = pprintType ty
    diff prettyTy (flip notElem) '\n'

prop_prettyTmNoNewlines :: Property
prop_prettyTmNoNewlines =
  withTests 1000 . property $ do
    ty <- forAll Sample.genTy
    e :: Exp Int <- forAll (Sample.genWellTypedExp ty)
    let prettyTm = pprintTerm e
    diff prettyTm (flip notElem) '\n'

testSTLC3Eager :: IO Bool
testSTLC3Eager = checkSequential $$(discover)
