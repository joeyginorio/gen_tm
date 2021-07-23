{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dataset where

import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy (toStrict, snoc)
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.CL as CL
import qualified Language.STLC as ST
import qualified Language.STLC.Gen as ST
import qualified Language.STLC2CL as ST2CL
import Pipes (Consumer, Pipe, Producer, cat, each, (>->))
import qualified Pipes as P
import qualified Pipes.ByteString as P hiding (map, take)
import qualified Pipes.Prelude as P hiding (toHandle)
import Pipes.Safe (MonadSafe)
import Pipes.Safe.Prelude (withFile)
import qualified System.IO as IO

-- | Examples in the dataset have this data type.
data Example where
  Example ::
    { -- | Example cost
      exCost :: !ST.Cost,
      -- | Example type
      exTy :: !ST.Type,
      -- | Pretty-printed example type
      exTyPretty :: !Text,
      -- | Example simply-typed lambda calculus term
      exSTLC :: !ST.Term,
      -- | Pretty-printed example simply-typed lambda calculus term
      exSTLCPretty :: !Text,
      -- | Example combinatory logic term
      exCL :: !CL.Term,
      -- | Pretty-printed example combinatory logic term
      exCLPretty :: !Text,
      -- | Reduced example simply-typed lambda calculus term
      exReducedSTLC :: !ST.Term,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      exReducedSTLCPretty :: !Text,
      -- | Reduced example combinatory logic term
      exReducedCL :: !CL.Term,
      -- | Pretty-printed reduced example combinatory logic term
      exReducedCLPretty :: !Text
    } ->
    Example
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''Example)

-- | Producer of costs, types, and simply-typed lambda calculus terms.
stlc :: forall m. Monad m => Producer (ST.Cost, ST.Type, ST.Term) m ()
stlc = each (ST.evalSearchS ST.gen) >-> P.map (\(Sum cost, (ty, tm)) -> (cost, ty, tm))

-- | Pipe from the STLC terms to the 'Example' data type.
toExample :: forall m. Monad m => Pipe (ST.Cost, ST.Type, ST.Term) Example m ()
toExample = P.for cat $
  \(exCost, exTy, exSTLC) ->
    let exTyPretty = Text.pack . show $ exTy
        exSTLCPretty = Text.pack . show $ exSTLC
        exReducedSTLC = ST.evalR exSTLC
        exReducedSTLCPretty = Text.pack . show $ exReducedSTLC
     in case ST2CL.compile exSTLC of
          Just exCL ->
            let exCLPretty = Text.pack . show $ exCL
                exReducedCL = CL.reduce exCL
                exReducedCLPretty = Text.pack . show $ exReducedCL
             in P.yield Example {..}
          Nothing -> pure ()

writeJsonLines :: MonadSafe m => FilePath -> Consumer Example m ()
writeJsonLines file = withFile file IO.WriteMode $ \h ->
  P.map (toStrict . flip snoc 0x0a . encodingToLazyByteString . toEncoding) >-> P.toHandle h
