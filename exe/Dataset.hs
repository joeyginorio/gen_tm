{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dataset where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get), evalStateT, modify)
import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (snoc, toStrict)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.Sample as STLC2.Sample
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Lift as P
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude as P
import qualified System.IO as IO

-- | Examples in the dataset have this data type.
data Example where
  Example ::
    { -- | Example type
      exSTLC2Type :: !STLC2.Type,
      -- | Pretty-printed example type
      exSTLC2TypePretty :: !Text,
      -- | Example simply-typed lambda calculus term
      exSTLC2Term :: !STLC2.Term,
      -- | Pretty-printed example simply-typed lambda calculus term
      exSTLC2TermPretty :: !Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      exSTLC2TermPrettyWithSig :: !Text,
      -- | Reduced example simply-typed lambda calculus term
      exReducedSTLC2Term :: !STLC2.Term,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      exReducedSTLC2TermPretty :: !Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      exReducedSTLC2TermPrettyWithSig :: !Text
    } ->
    Example
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''Example)

-- | Nondeterministic producer of types and simply-typed lambda calculus terms.
sampleStlc :: forall m. Monad m => Seed.Seed -> P.Producer (STLC2.Type, STLC2.Term) m ()
sampleStlc =
  evalStateT . P.distribute . P.repeatM . STLC2.Sample.sample $ do
    stlc2Type <- STLC2.Sample.genTy
    stlc2Term <- Gen.generalize $ STLC2.Sample.genWellTypedExp stlc2Type
    pure (stlc2Type, stlc2Term)

-- | Pipe from the STLC terms to the 'Example' data type.
-- The input is a triple of maybe a cost, a type, and an STLC term, and the output is an
-- 'Example'.
toExample :: forall m. Monad m => P.Pipe (STLC2.Type, STLC2.Term) Example m ()
toExample = P.for P.cat $
  \(exSTLC2Type, exSTLC2Term) ->
    let exSTLC2TypePretty = Text.pack . STLC2.pprintType $ exSTLC2Type
        exSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ exSTLC2Term
        exSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ exSTLC2Term
        exReducedSTLC2Term = STLC2.eval' exSTLC2Term
        exReducedSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ exReducedSTLC2Term
        exReducedSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ exReducedSTLC2Term
     in P.yield Example {..}

-- | Deduplicate the examples.
deduplicate :: forall m r a. (Monad m, Eq a, Hashable a) => (Example -> a) -> P.Pipe Example Example m r
deduplicate f = P.evalStateP HashSet.empty . P.for P.cat $ \ex -> do
  cache <- get
  let a = f ex
  if not (HashSet.member a cache)
    then do
      P.yield ex
      modify (HashSet.insert a)
    else pure ()

-- | Write a JSON Lines text file with the examples.
writeJsonLines :: forall m a. (P.MonadSafe m, ToJSON a) => FilePath -> P.Consumer a m ()
writeJsonLines file = P.withFile file IO.WriteMode $ \h ->
  P.map (toStrict . flip snoc 0x0a . encodingToLazyByteString . toEncoding)
    >-> P.for P.cat (\bs -> liftIO $ BS.hPut h bs >> IO.hFlush h)
