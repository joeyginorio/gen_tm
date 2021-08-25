{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dataset where

import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy (snoc, toStrict)
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen)
import Hedgehog.Internal.Gen (evalGen)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import qualified Language.STLC2 as ST
import qualified Language.STLC2.Sample as STLC2.Sample
import Pipes (Consumer, Pipe, Producer, cat, (>->))
import qualified Pipes as P
import qualified Pipes.ByteString as P hiding (map, take)
import Pipes.Lift (distribute)
import Pipes.Prelude (repeatM)
import qualified Pipes.Prelude as P hiding (toHandle)
import Pipes.Safe (MonadSafe)
import Pipes.Safe.Prelude (withFile)
import qualified System.IO as IO

-- | Examples in the dataset have this data type.
data Example where
  Example ::
    { -- | Example type
      exTy :: !ST.Type,
      -- | Pretty-printed example type
      exTyPretty :: !Text,
      -- | Example simply-typed lambda calculus term
      exSTLC :: !ST.Term,
      -- | Pretty-printed example simply-typed lambda calculus term
      exSTLCPretty :: !Text,
      -- | Reduced example simply-typed lambda calculus term
      exReducedSTLC :: !ST.Term,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      exReducedSTLCPretty :: !Text
    } ->
    Example
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''Example)

-- | Nondeterministic producer of types and simply-typed lambda calculus terms.
sampleStlc :: forall m. Monad m => Seed.Seed -> Producer (ST.Type, ST.Term) m ()
sampleStlc =
  evalStateT . distribute . repeatM . sample $ do
    ty <- STLC2.Sample.genTy
    tm <- STLC2.Sample.genWellTypedExp ty
    pure (ty, tm)
  where
    sample :: forall a. Gen a -> StateT Seed.Seed m a
    sample gen =
      let go = do
            seed <- get
            let (seed', seed'') = Seed.split seed
            put seed''
            case evalGen 30 seed' gen of
              Nothing ->
                go
              Just x ->
                pure $ Tree.treeValue x
       in go

-- | Pipe from the STLC terms to the 'Example' data type.
-- The input is a triple of maybe a cost, a type, and an STLC term, and the output is an
-- 'Example'.
toExample :: forall m. Monad m => Pipe (ST.Type, ST.Term) Example m ()
toExample = P.for cat $
  \(exTy, exSTLC) ->
    let exTyPretty = Text.pack . show $ exTy
        exSTLCPretty = Text.pack . show $ exSTLC
        exReducedSTLC = ST.eval' exSTLC
        exReducedSTLCPretty = Text.pack . show $ exReducedSTLC
     in P.yield Example {..}

-- | Write a JSON Lines text file with the examples.
writeJsonLines :: forall m a. (MonadSafe m, ToJSON a) => FilePath -> Consumer a m ()
writeJsonLines file = withFile file IO.WriteMode $ \h ->
  P.map (toStrict . flip snoc 0x0a . encodingToLazyByteString . toEncoding) >-> P.toHandle h
