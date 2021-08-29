{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Dataset where

import Control.Lens (Zoom, Zoomed, makeLenses, zoom, (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get), evalStateT, modify)
import Data.Aeson (ToJSON (toEncoding), Options (fieldLabelModifier))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (snoc, toStrict)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
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
      _exSTLC2Type :: !STLC2.Type,
      -- | Pretty-printed example type
      _exSTLC2TypePretty :: !Text,
      -- | Example simply-typed lambda calculus term
      _exSTLC2Term :: !STLC2.Term,
      -- | Term statistics
      _exSTLC2TermStats :: !(STLC2.TermStats Int),
      -- | Pretty-printed example simply-typed lambda calculus term
      _exSTLC2TermPretty :: !Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      _exSTLC2TermPrettyWithSig :: !Text,
      -- | Evaluation statistics
      _exSTLC2EvalStats :: !(STLC2.EvalStats Int),
      -- | Reduced example simply-typed lambda calculus term
      _exReducedSTLC2Term :: !STLC2.Term,
      -- | Reduced term statistics
      _exReducedSTLC2TermStats :: !(STLC2.TermStats Int),
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      _exReducedSTLC2TermPretty :: !Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      _exReducedSTLC2TermPrettyWithSig :: !Text
    } ->
    Example
  deriving stock (Show, Eq)

makeLenses ''Example
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Example)

-- | Nondeterministic producer of types and simply-typed lambda calculus terms.
sampleStlc :: forall m r. Monad m => Seed.Seed -> P.Producer (STLC2.Type, STLC2.Term) m r
sampleStlc =
  evalStateT . P.distribute . P.repeatM . STLC2.Sample.sample $ do
    stlc2Type <- STLC2.Sample.genTy
    stlc2Term <- Gen.generalize $ STLC2.Sample.genWellTypedExp stlc2Type
    pure (stlc2Type, stlc2Term)

-- | Pipe from the STLC terms to the 'Example' data type.
-- The input is a triple of maybe a cost, a type, and an STLC term, and the output is an
-- 'Example'.
toExample :: forall m r. Monad m => P.Pipe (STLC2.Type, STLC2.Term) Example m r
toExample = P.for P.cat $
  \(_exSTLC2Type, _exSTLC2Term) ->
    let _exSTLC2TermStats = STLC2.countConstructors _exSTLC2Term
        _exSTLC2TypePretty = Text.pack . STLC2.pprintType $ _exSTLC2Type
        _exSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ _exSTLC2Term
        _exSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _exSTLC2Term
        (_exReducedSTLC2Term, _exSTLC2EvalStats) = STLC2.evalWR _exSTLC2Term
        _exReducedSTLC2TermStats = STLC2.countConstructors _exReducedSTLC2Term
        _exReducedSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ _exReducedSTLC2Term
        _exReducedSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _exReducedSTLC2Term
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

data Histogram = Histogram
  { _histSTLC2TermStats :: !(STLC2.TermStats (IntMap Int)),
    _histSTLC2EvalStats :: !(STLC2.EvalStats (IntMap Int)),
    _histReducedSTLC2TermStats :: !(STLC2.TermStats (IntMap Int))
  }
  deriving stock (Show, Eq)

instance Semigroup Histogram where
  Histogram a b c <> Histogram a' b' c' = Histogram (a <> a') (b <> b') (c <> c')

instance Monoid Histogram where
  mempty = Histogram mempty mempty mempty

makeLenses ''Histogram
$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Histogram)

histogram ::
  forall m m' m'' r.
  ( Zoom m' m (STLC2.EvalStats (IntMap Int)) Histogram,
    Functor (Zoomed m' ()),
    Zoom m'' m (STLC2.TermStats (IntMap Int)) Histogram,
    Functor (Zoomed m'' ())
  ) =>
  P.Consumer Example m r
histogram =
  P.for P.cat $ \ex -> P.lift $ do
    zoom histSTLC2TermStats . modify $ STLC2.updateTermHistogram (ex ^. exSTLC2TermStats)
    zoom histSTLC2EvalStats . modify $ STLC2.updateEvalHistogram (ex ^. exSTLC2EvalStats)
    zoom histReducedSTLC2TermStats . modify $ STLC2.updateTermHistogram (ex ^. exReducedSTLC2TermStats)

-- | Write a JSON Lines text file with the examples.
writeJsonLines :: forall m a r. (P.MonadSafe m, ToJSON a) => FilePath -> P.Consumer a m r
writeJsonLines file = P.withFile file IO.WriteMode $ \h ->
  P.map (toStrict . flip snoc 0x0a . encodingToLazyByteString . toEncoding)
    >-> P.for P.cat (\bs -> liftIO $ BS.hPut h bs >> IO.hFlush h)
