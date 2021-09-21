{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState (get), evalStateT, modify)
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON (toEncoding), eitherDecodeStrict')
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (snoc, toStrict)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Language.LC as LC
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.Sample as STLC2.Sample
import qualified Language.STLC2.ToLC as STLC2
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
      _exReducedSTLC2TermPrettyWithSig :: !Text,
      -- | Example term converted to untyped lambda calculus
      _exLCTerm :: !LC.Term,
      -- | Lambda calculus term statistics
      _exLCTermStats :: !(LC.TermStats Int),
      -- | Pretty-printed example lambda calculus term
      _exLCTermPretty :: !Text,
      -- | Lambda calculus evaluation statistics
      _exLCEvalStats :: !(LC.EvalStats Int),
      -- | Reduced untyped lambda calculus term
      _exReducedLCTerm :: !LC.Term,
      -- | Reduced lambda calculus term statistics
      _exReducedLCTermStats :: !(LC.TermStats Int),
      -- | Pretty-printed reduced untyped lambda calculus term
      _exReducedLCTermPretty :: !Text
    } ->
    Example
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

makeLenses ''Example
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Example)

-- | Nondeterministic producer of types and simply-typed lambda calculus terms.
sampleStlc :: forall m r. Monad m => Seed.Seed -> P.Producer (STLC2.Type, STLC2.Term) m r
sampleStlc =
  evalStateT . P.distribute . P.repeatM . STLC2.Sample.sample $ do
    stlc2Type <- STLC2.Sample.genTy
    stlc2Term <- Gen.generalize $ STLC2.Sample.genWellTypedExp stlc2Type
    pure (stlc2Type, stlc2Term)

toExample' :: STLC2.Type -> STLC2.Term -> Example
toExample' _exSTLC2Type _exSTLC2Term =
  let _exSTLC2TermStats = STLC2.countConstructors _exSTLC2Term
      _exSTLC2TypePretty = Text.pack . STLC2.pprintType $ _exSTLC2Type
      _exSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ _exSTLC2Term
      _exSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _exSTLC2Term
      (_exReducedSTLC2Term, _exSTLC2EvalStats) = STLC2.evalWR _exSTLC2Term
      _exReducedSTLC2TermStats = STLC2.countConstructors _exReducedSTLC2Term
      _exReducedSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ _exReducedSTLC2Term
      _exReducedSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _exReducedSTLC2Term
      _exLCTerm = STLC2.toLC _exSTLC2Term
      _exLCTermStats = LC.countConstructors _exLCTerm
      _exLCTermPretty = Text.pack . LC.pprintTerm $ _exLCTerm
      (_exReducedLCTerm, _exLCEvalStats) = LC.evalWR _exLCTerm
      _exReducedLCTermStats = LC.countConstructors _exReducedLCTerm
      _exReducedLCTermPretty = Text.pack . LC.pprintTerm $ _exReducedLCTerm
   in Example {..}

-- | Pipe from the STLC terms to the 'Example' data type.
-- The input is a triple of maybe a cost, a type, and an STLC term, and the output is an
-- 'Example'.
toExample :: forall m r. Monad m => P.Pipe (STLC2.Type, STLC2.Term) Example m r
toExample = P.for P.cat $ P.yield . uncurry toExample'

-- | Deduplicate the examples.
deduplicate :: forall m r a. (Monad m, Eq a, Hashable a) => HashSet a -> (Example -> a) -> P.Pipe Example Example m r
deduplicate s f = P.evalStateP s . P.for P.cat $ \ex -> do
  s' <- get
  let a = f ex
  if not (HashSet.member a s')
    then do
      P.yield ex
      modify (HashSet.insert a)
    else pure ()

-- | Cache the examples.
cache :: forall m r a. (Monad m, Eq a, Hashable a, MonadState (HashMap a Example) m) => (Example -> a) -> P.Pipe Example Example m r
cache f = P.for P.cat $ \ex -> do
  modify (HashMap.insert (f ex) ex)

-- | Produce compositional examples.
compositions :: (Monad m) => HashMap a Example -> P.Producer Example m ()
compositions m = P.for (P.each . HashMap.elems $ m) $ \ex -> do
  let tm = ex ^. exSTLC2Term
  P.for (P.each . HashMap.elems $ m) $ \ex' -> do
    let tm' = ex' ^. exSTLC2Term
        comp = STLC2.TmApp tm tm'
    case runReaderT (STLC2.tyCheck comp) [] of
      Left _err -> pure ()
      Right ty -> P.yield $ toExample' ty comp

data Histogram a = Histogram
  { _histSTLC2TermStats :: !(STLC2.TermStats a),
    _histSTLC2EvalStats :: !(STLC2.EvalStats a),
    _histReducedSTLC2TermStats :: !(STLC2.TermStats a),
    _histLCTermStats :: !(LC.TermStats a),
    _histLCEvalStats :: !(LC.EvalStats a),
    _histReducedLCTermStats :: !(LC.TermStats a)
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Histogram a) where
  Histogram a b c d e f <> Histogram a' b' c' d' e' f' = Histogram (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance Monoid a => Monoid (Histogram a) where
  mempty = Histogram mempty mempty mempty mempty mempty mempty

makeLenses ''Histogram
$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Histogram)

histogram ::
  forall m m' m'' m''' m'''' r.
  ( Zoom m' m (STLC2.EvalStats (IntMap Int)) (Histogram (IntMap Int)),
    Functor (Zoomed m' ()),
    Zoom m'' m (STLC2.TermStats (IntMap Int)) (Histogram (IntMap Int)),
    Functor (Zoomed m'' ()),
    Zoom m''' m (LC.EvalStats (IntMap Int)) (Histogram (IntMap Int)),
    Functor (Zoomed m''' ()),
    Zoom m'''' m (LC.TermStats (IntMap Int)) (Histogram (IntMap Int)),
    Functor (Zoomed m'''' ())
  ) =>
  P.Consumer Example m r
histogram =
  P.for P.cat $ \ex -> P.lift $ do
    zoom histSTLC2TermStats . modify $ STLC2.updateTermHistogram (ex ^. exSTLC2TermStats)
    zoom histSTLC2EvalStats . modify $ STLC2.updateEvalHistogram (ex ^. exSTLC2EvalStats)
    zoom histReducedSTLC2TermStats . modify $ STLC2.updateTermHistogram (ex ^. exReducedSTLC2TermStats)
    zoom histLCTermStats . modify $ LC.updateTermHistogram (ex ^. exLCTermStats)
    zoom histLCEvalStats . modify $ LC.updateEvalHistogram (ex ^. exLCEvalStats)
    zoom histReducedLCTermStats . modify $ LC.updateTermHistogram (ex ^. exReducedLCTermStats)

data HistogramRecord = HistogramRecord {_hrValue :: !Int, _hrCount :: !Int}
  deriving stock (Show, Eq, Ord)

makeLenses ''HistogramRecord
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''HistogramRecord)

toRecords :: IntMap Int -> [HistogramRecord]
toRecords im = uncurry HistogramRecord <$> IntMap.toList im

-- | Write a JSON Lines text file.
writeJsonLines :: forall m a r. (P.MonadSafe m, ToJSON a) => FilePath -> P.Consumer a m r
writeJsonLines file = P.withFile file IO.WriteMode $ \h ->
  P.map (toStrict . flip snoc 0x0a . encodingToLazyByteString . toEncoding)
    >-> P.for P.cat (\bs -> liftIO $ BS.hPut h bs >> IO.hFlush h)

-- | Read a JSON Lines text file.
readJsonLines :: forall m a. (P.MonadSafe m, FromJSON a) => FilePath -> P.Producer a m ()
readJsonLines file = P.withFile file IO.ReadMode $ \h ->
  go h
  where
    go h = do
      eof <- liftIO $ IO.hIsEOF h
      unless eof $ do
        bs <- liftIO $ BS.hGetLine h
        case eitherDecodeStrict' bs of
          Left err -> P.lift . P.throwM . userError $ "Failed to decode JSON: " <> err
          Right a -> P.yield a >> go h
