{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dataset where

import Control.Lens (Lens', makeLenses, zoom, (^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (runReaderT)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import qualified Data.Csv as CSV
import qualified Data.Csv.Parser as Csv
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
import Language.Eagerness (Eagerness (..))
import qualified Language.LC as LC
import qualified Language.LC2 as LC2
import qualified Language.LC2.ToLC as LC2
import qualified Language.LCEager as LCEager
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.Sample as STLC2.Sample
import qualified Language.STLC2.ToLC as STLC2
import qualified Language.STLC3 as STLC3
import qualified Language.STLC3.Sample as STLC3.Sample
import qualified Language.STLC3.ToLC2 as STLC3
import qualified Language.STLC3Eager as STLC3Eager
import qualified Language.STLC3Eager.Sample as STLC3Eager.Sample
import qualified Language.STLC3Eager.ToLCEager as STLC3Eager
import Opts (Language (..))
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as P hiding (map)
import qualified Pipes.Csv as P
import qualified Pipes.Lift as P
import qualified Pipes.Prelude as P hiding (fromHandle)
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude as P
import qualified System.IO as IO

type Tokenize = String -> IO [Int]

class
  ( Monoid (Dataset.Histogram l (IntMap Int)),
    Aeson.ToJSON (Dataset.Example l),
    Aeson.FromJSON (Dataset.Example l),
    Hashable (Type l),
    Eq (Type l)
  ) =>
  HasExamples (l :: Language)
  where
  type Type l = r | r -> l
  type Term l = r | r -> l
  type Example l = r | r -> l
  type Histogram l a = r | r -> l

  -- | Nondeterministic producer of types and simply-typed lambda calculus terms.
  sample' :: forall m. Monad m => SL.StateT Seed.Seed m (Type l, Term l)

  -- | Convert a type and a term to an Example.
  toExample' :: Type l -> Term l -> Example l

  -- | Extract a term from an Example.
  term :: Lens' (Example l) (Term l)

  -- | Extract a pretty-printed term from an Example.
  prettyTerm :: Lens' (Example l) Text

  -- | Extract a pretty-printed reduced term from an Example.
  prettyReducedTerm :: Lens' (Example l) Text

  -- | Extract all pretty-printed input terms from an Example.
  prettyTerms :: Example l -> [Text]

  -- | Extract all pretty-printed output terms from an Example.
  prettyReducedTerms :: Example l -> [Text]

  -- | Apply a function to a term.
  app :: Term l -> Term l -> Term l

  -- | type-check a term.
  typeCheck :: Term l -> Maybe (Type l)

  -- | Update histogram.
  histogram' :: forall m. (Monad m) => Example l -> SS.StateT (Histogram l (IntMap Int)) m ()

-- | Given a seed, produce a stream of sampled examples.
sample :: forall m r l. (Monad m, HasExamples l) => Seed.Seed -> P.Producer (Type l, Term l) m r
sample = SL.evalStateT . P.distribute . P.repeatM $ sample'

-- | Convert a stream of types and terms to a stream of Examples.
toExample :: forall m r l. (Monad m, HasExamples l) => P.Pipe (Type l, Term l) (Example l) m r
toExample = P.for P.cat $ P.yield . uncurry toExample'

-- | Examples in the STLC2 dataset have this data type.
data Example2 where
  Example2 ::
    { -- | Example type
      _ex2STLC2Type :: STLC2.Type,
      -- | Pretty-printed example type
      _ex2STLC2TypePretty :: Text,
      -- | Example simply-typed lambda calculus term
      _ex2STLC2Term :: STLC2.Term,
      -- | Term statistics
      _ex2STLC2TermStats :: STLC2.TermStats Int,
      -- | Pretty-printed example simply-typed lambda calculus term
      _ex2STLC2TermPretty :: Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      _ex2STLC2TermPrettyWithSig :: Text,
      -- | Evaluation statistics
      _ex2STLC2EvalStats :: STLC2.EvalStats Int,
      -- | Reduced example simply-typed lambda calculus term
      _ex2ReducedSTLC2Term :: STLC2.Term,
      -- | Reduced term statistics
      _ex2ReducedSTLC2TermStats :: STLC2.TermStats Int,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      _ex2ReducedSTLC2TermPretty :: Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      _ex2ReducedSTLC2TermPrettyWithSig :: Text,
      -- | Example term converted to untyped lambda calculus
      _ex2LCTerm :: LC.Term,
      -- | Lambda calculus term statistics
      _ex2LCTermStats :: LC.TermStats Int,
      -- | Pretty-printed example lambda calculus term
      _ex2LCTermPretty :: Text,
      -- | Lambda calculus evaluation statistics
      _ex2LCEvalStats :: LC.EvalStats Int,
      -- | Reduced untyped lambda calculus term
      _ex2ReducedLCTerm :: LC.Term,
      -- | Reduced lambda calculus term statistics
      _ex2ReducedLCTermStats :: LC.TermStats Int,
      -- | Pretty-printed reduced untyped lambda calculus term
      _ex2ReducedLCTermPretty :: Text
    } ->
    Example2
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

makeLenses ''Example2
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 4} ''Example2)

data Histogram2 a = Histogram2
  { _hist2STLC2TermStats :: STLC2.TermStats a,
    _hist2STLC2EvalStats :: STLC2.EvalStats a,
    _hist2ReducedSTLC2TermStats :: STLC2.TermStats a,
    _hist2LCTermStats :: LC.TermStats a,
    _hist2LCEvalStats :: LC.EvalStats a,
    _hist2ReducedLCTermStats :: LC.TermStats a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Histogram2 a) where
  Histogram2 a b c d e f <> Histogram2 a' b' c' d' e' f' = Histogram2 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance Monoid a => Monoid (Histogram2 a) where
  mempty = Histogram2 mempty mempty mempty mempty mempty mempty

makeLenses ''Histogram2
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 6} ''Histogram2)

instance HasExamples 'STLC2 where
  type Type 'STLC2 = STLC2.Type
  type Term 'STLC2 = STLC2.Term
  type Example 'STLC2 = Example2
  type Histogram 'STLC2 a = Histogram2 a
  sample' = STLC2.Sample.sample $ do
    stlc2Type <- STLC2.Sample.genTy
    stlc2Term <- Gen.generalize $ STLC2.Sample.genWellTypedExp stlc2Type
    pure (stlc2Type, stlc2Term)
  toExample' _ex2STLC2Type _ex2STLC2Term =
    let _ex2STLC2TermStats = STLC2.countConstructors _ex2STLC2Term
        _ex2STLC2TypePretty = Text.pack . STLC2.pprintType $ _ex2STLC2Type
        _ex2STLC2TermPretty = Text.pack . STLC2.pprintTerm $ _ex2STLC2Term
        _ex2STLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _ex2STLC2Term
        (_ex2ReducedSTLC2Term, _ex2STLC2EvalStats) = STLC2.evalWR _ex2STLC2Term
        _ex2ReducedSTLC2TermStats = STLC2.countConstructors _ex2ReducedSTLC2Term
        _ex2ReducedSTLC2TermPretty = Text.pack . STLC2.pprintTerm $ _ex2ReducedSTLC2Term
        _ex2ReducedSTLC2TermPrettyWithSig = Text.pack . STLC2.pprintTermWithSig $ _ex2ReducedSTLC2Term
        _ex2LCTerm = STLC2.toLC _ex2STLC2Term
        _ex2LCTermStats = LC.countConstructors _ex2LCTerm
        _ex2LCTermPretty = Text.pack . LC.pprintTerm $ _ex2LCTerm
        (_ex2ReducedLCTerm, _ex2LCEvalStats) = LC.evalWR _ex2LCTerm
        _ex2ReducedLCTermStats = LC.countConstructors _ex2ReducedLCTerm
        _ex2ReducedLCTermPretty = Text.pack . LC.pprintTerm $ _ex2ReducedLCTerm
     in Example2 {..}
  term = ex2STLC2Term
  prettyTerm = ex2STLC2TermPretty
  prettyReducedTerm = ex2ReducedSTLC2TermPretty
  prettyTerms = sequenceA [(^. ex2STLC2TermPretty), (^. ex2LCTermPretty)]
  prettyReducedTerms = sequenceA [(^. ex2ReducedSTLC2TermPretty), (^. ex2ReducedLCTermPretty)]
  app = STLC2.TmApp
  typeCheck tm = either (const Nothing) Just $ runReaderT (STLC2.tyCheck tm) []
  histogram' ex = do
    zoom hist2STLC2TermStats . SS.modify $ STLC2.updateTermHistogram (ex ^. ex2STLC2TermStats)
    zoom hist2STLC2EvalStats . SS.modify $ STLC2.updateEvalHistogram (ex ^. ex2STLC2EvalStats)
    zoom hist2ReducedSTLC2TermStats . SS.modify $ STLC2.updateTermHistogram (ex ^. ex2ReducedSTLC2TermStats)
    zoom hist2LCTermStats . SS.modify $ LC.updateTermHistogram (ex ^. ex2LCTermStats)
    zoom hist2LCEvalStats . SS.modify $ LC.updateEvalHistogram (ex ^. ex2LCEvalStats)
    zoom hist2ReducedLCTermStats . SS.modify $ LC.updateTermHistogram (ex ^. ex2ReducedLCTermStats)

data Example3 where
  Example3 ::
    { -- | Example type
      _ex3STLC3Type :: STLC3.Type,
      -- | Pretty-printed example type
      _ex3STLC3TypePretty :: Text,
      -- | Example simply-typed lambda calculus term
      _ex3STLC3Term :: STLC3.Term,
      -- | Term statistics
      _ex3STLC3TermStats :: STLC3.TermStats Int,
      -- | Pretty-printed example simply-typed lambda calculus term
      _ex3STLC3TermPretty :: Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      _ex3STLC3TermPrettyWithSig :: Text,
      -- | Evaluation statistics
      _ex3STLC3EvalStats :: STLC3.EvalStats Int,
      -- | Reduced example simply-typed lambda calculus term
      _ex3ReducedSTLC3Term :: STLC3.Term,
      -- | Reduced term statistics
      _ex3ReducedSTLC3TermStats :: STLC3.TermStats Int,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      _ex3ReducedSTLC3TermPretty :: Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      _ex3ReducedSTLC3TermPrettyWithSig :: Text,
      -- | Example term converted to untyped lambda calculus
      _ex3LC2Term :: LC2.Term,
      -- | Lambda calculus term statistics
      _ex3LC2TermStats :: LC2.TermStats Int,
      -- | Pretty-printed example lambda calculus term
      _ex3LC2TermPretty :: Text,
      -- | Lambda calculus evaluation statistics
      _ex3LC2EvalStats :: LC2.EvalStats Int,
      -- | Reduced untyped lambda calculus term
      _ex3ReducedLC2Term :: LC2.Term,
      -- | Reduced lambda calculus term statistics
      _ex3ReducedLC2TermStats :: LC2.TermStats Int,
      -- | Pretty-printed reduced untyped lambda calculus term
      _ex3ReducedLC2TermPretty :: Text,
      -- | Example term converted to untyped lambda calculus
      _ex3LCTerm :: LC.Term,
      -- | Lambda calculus term statistics
      _ex3LCTermStats :: LC.TermStats Int,
      -- | Pretty-printed example lambda calculus term
      _ex3LCTermPretty :: Text,
      -- | Lambda calculus evaluation statistics
      _ex3LCEvalStats :: LC.EvalStats Int,
      -- | Reduced untyped lambda calculus term
      _ex3ReducedLCTerm :: LC.Term,
      -- | Reduced lambda calculus term statistics
      _ex3ReducedLCTermStats :: LC.TermStats Int,
      -- | Pretty-printed reduced untyped lambda calculus term
      _ex3ReducedLCTermPretty :: Text
    } ->
    Example3
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

makeLenses ''Example3
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 4} ''Example3)

data Histogram3 a = Histogram3
  { _hist3STLC3TermStats :: STLC3.TermStats a,
    _hist3STLC3EvalStats :: STLC3.EvalStats a,
    _hist3ReducedSTLC3TermStats :: STLC3.TermStats a,
    _hist3LC2TermStats :: LC2.TermStats a,
    _hist3LC2EvalStats :: LC2.EvalStats a,
    _hist3ReducedLC2TermStats :: LC2.TermStats a,
    _hist3LCTermStats :: LC.TermStats a,
    _hist3LCEvalStats :: LC.EvalStats a,
    _hist3ReducedLCTermStats :: LC.TermStats a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Histogram3 a) where
  Histogram3 a b c d e f g h i <> Histogram3 a' b' c' d' e' f' g' h' i' = Histogram3 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i')

instance Monoid a => Monoid (Histogram3 a) where
  mempty = Histogram3 mempty mempty mempty mempty mempty mempty mempty mempty mempty

makeLenses ''Histogram3
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 6} ''Histogram3)

instance HasExamples 'STLC3 where
  type Type 'STLC3 = STLC3.Type
  type Term 'STLC3 = STLC3.Term
  type Example 'STLC3 = Example3
  type Histogram 'STLC3 a = Histogram3 a
  sample' = STLC3.Sample.sample $ do
    stlc3Type <- STLC3.Sample.genTy
    stlc3Term <- Gen.generalize $ STLC3.Sample.genWellTypedExp stlc3Type
    pure (stlc3Type, stlc3Term)
  toExample' _ex3STLC3Type _ex3STLC3Term =
    let _ex3STLC3TermStats = STLC3.countConstructors _ex3STLC3Term
        _ex3STLC3TypePretty = Text.pack . STLC3.pprintType $ _ex3STLC3Type
        _ex3STLC3TermPretty = Text.pack . STLC3.pprintTerm $ _ex3STLC3Term
        _ex3STLC3TermPrettyWithSig = Text.pack . STLC3.pprintTermWithSig $ _ex3STLC3Term
        (_ex3ReducedSTLC3Term, _ex3STLC3EvalStats) = STLC3.evalWR _ex3STLC3Term
        _ex3ReducedSTLC3TermStats = STLC3.countConstructors _ex3ReducedSTLC3Term
        _ex3ReducedSTLC3TermPretty = Text.pack . STLC3.pprintTerm $ _ex3ReducedSTLC3Term
        _ex3ReducedSTLC3TermPrettyWithSig = Text.pack . STLC3.pprintTermWithSig $ _ex3ReducedSTLC3Term
        _ex3LC2Term = STLC3.toLC2 _ex3STLC3Term
        _ex3LC2TermStats = LC2.countConstructors _ex3LC2Term
        _ex3LC2TermPretty = Text.pack . LC2.pprintTerm $ _ex3LC2Term
        (_ex3ReducedLC2Term, _ex3LC2EvalStats) = LC2.evalWR _ex3LC2Term
        _ex3ReducedLC2TermStats = LC2.countConstructors _ex3ReducedLC2Term
        _ex3ReducedLC2TermPretty = Text.pack . LC2.pprintTerm $ _ex3ReducedLC2Term
        _ex3LCTerm = LC2.toLC _ex3LC2Term
        _ex3LCTermStats = LC.countConstructors _ex3LCTerm
        _ex3LCTermPretty = Text.pack . LC.pprintTerm $ _ex3LCTerm
        (_ex3ReducedLCTerm, _ex3LCEvalStats) = LC.evalWR _ex3LCTerm
        _ex3ReducedLCTermStats = LC.countConstructors _ex3ReducedLCTerm
        _ex3ReducedLCTermPretty = Text.pack . LC.pprintTerm $ _ex3ReducedLCTerm
     in Example3 {..}
  term = ex3STLC3Term
  prettyTerm = ex3STLC3TermPretty
  prettyReducedTerm = ex3ReducedSTLC3TermPretty
  prettyTerms = sequenceA [(^. ex3STLC3TermPretty), (^. ex3LC2TermPretty), (^. ex3LCTermPretty)]
  prettyReducedTerms = sequenceA [(^. ex3ReducedSTLC3TermPretty), (^. ex3ReducedLC2TermPretty), (^. ex3ReducedLCTermPretty)]
  app = STLC3.TmApp
  typeCheck tm = either (const Nothing) Just $ runReaderT (STLC3.tyCheck tm) []
  histogram' ex = do
    zoom hist3STLC3TermStats . SS.modify $ STLC3.updateTermHistogram (ex ^. ex3STLC3TermStats)
    zoom hist3STLC3EvalStats . SS.modify $ STLC3.updateEvalHistogram (ex ^. ex3STLC3EvalStats)
    zoom hist3ReducedSTLC3TermStats . SS.modify $ STLC3.updateTermHistogram (ex ^. ex3ReducedSTLC3TermStats)
    zoom hist3LC2TermStats . SS.modify $ LC2.updateTermHistogram (ex ^. ex3LC2TermStats)
    zoom hist3LC2EvalStats . SS.modify $ LC2.updateEvalHistogram (ex ^. ex3LC2EvalStats)
    zoom hist3ReducedLC2TermStats . SS.modify $ LC2.updateTermHistogram (ex ^. ex3ReducedLC2TermStats)
    zoom hist3LCTermStats . SS.modify $ LC.updateTermHistogram (ex ^. ex3LCTermStats)
    zoom hist3LCEvalStats . SS.modify $ LC.updateEvalHistogram (ex ^. ex3LCEvalStats)
    zoom hist3ReducedLCTermStats . SS.modify $ LC.updateTermHistogram (ex ^. ex3ReducedLCTermStats)

data Example3Eager where
  Example3Eager ::
    { -- | Example type
      _ex3EagerSTLC3EagerType :: STLC3Eager.Ty 'Eager,
      -- | Pretty-printed example type
      _ex3EagerSTLC3EagerTypePretty :: Text,
      -- | Example simply-typed lambda calculus term
      _ex3EagerSTLC3EagerTerm :: STLC3Eager.Exp 'Eager Int,
      -- | Term statistics
      _ex3EagerSTLC3EagerTermStats :: STLC3Eager.TermStats Int,
      -- | Pretty-printed example simply-typed lambda calculus term
      _ex3EagerSTLC3EagerTermPretty :: Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      _ex3EagerSTLC3EagerTermPrettyWithSig :: Text,
      -- | Evaluation statistics
      _ex3EagerSTLC3EagerEvalStats :: STLC3Eager.EvalStats Int,
      -- | Reduced example simply-typed lambda calculus term
      _ex3EagerReducedSTLC3EagerTerm :: STLC3Eager.Exp 'Eager Int,
      -- | Reduced term statistics
      _ex3EagerReducedSTLC3EagerTermStats :: STLC3Eager.TermStats Int,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      _ex3EagerReducedSTLC3EagerTermPretty :: Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      _ex3EagerReducedSTLC3EagerTermPrettyWithSig :: Text,
      -- | Example term converted to untyped lambda calculus
      _ex3EagerLCEagerTerm :: LCEager.Exp 'Eager Int,
      -- | Lambda calculus term statistics
      _ex3EagerLCEagerTermStats :: LCEager.TermStats Int,
      -- | Pretty-printed example lambda calculus term
      _ex3EagerLCEagerTermPretty :: Text,
      -- | Lambda calculus evaluation statistics
      _ex3EagerLCEagerEvalStats :: LCEager.EvalStats Int,
      -- | Reduced untyped lambda calculus term
      _ex3EagerReducedLCEagerTerm :: LCEager.Exp 'Eager Int,
      -- | Reduced lambda calculus term statistics
      _ex3EagerReducedLCEagerTermStats :: LCEager.TermStats Int,
      -- | Pretty-printed reduced untyped lambda calculus term
      _ex3EagerReducedLCEagerTermPretty :: Text
    } ->
    Example3Eager
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

makeLenses ''Example3Eager
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 9} ''Example3Eager)

data Histogram3Eager a = Histogram3Eager
  { _hist3EagerSTLC3EagerTermStats :: STLC3Eager.TermStats a,
    _hist3EagerSTLC3EagerEvalStats :: STLC3Eager.EvalStats a,
    _hist3EagerReducedSTLC3EagerTermStats :: STLC3Eager.TermStats a,
    _hist3EagerLCEagerTermStats :: LCEager.TermStats a,
    _hist3EagerLCEagerEvalStats :: LCEager.EvalStats a,
    _hist3EagerReducedLCEagerTermStats :: LCEager.TermStats a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Histogram3Eager a) where
  Histogram3Eager a b c d e f <> Histogram3Eager a' b' c' d' e' f' = Histogram3Eager (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance Monoid a => Monoid (Histogram3Eager a) where
  mempty = Histogram3Eager mempty mempty mempty mempty mempty mempty

makeLenses ''Histogram3Eager
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 11} ''Histogram3Eager)

instance HasExamples 'STLC3Eager where
  type Type 'STLC3Eager = STLC3Eager.Ty 'Eager
  type Term 'STLC3Eager = STLC3Eager.Exp 'Eager Int
  type Example 'STLC3Eager = Example3Eager
  type Histogram 'STLC3Eager a = Histogram3Eager a
  sample' = STLC3Eager.Sample.sample $ do
    stlc3EagerType <- STLC3Eager.Sample.genTy
    stlc3EagerTerm <- Gen.generalize $ STLC3Eager.Sample.genWellTypedExp stlc3EagerType
    pure (stlc3EagerType, stlc3EagerTerm)
  toExample' _ex3EagerSTLC3EagerType _ex3EagerSTLC3EagerTerm =
    let _ex3EagerSTLC3EagerTermStats = STLC3Eager.countConstructors _ex3EagerSTLC3EagerTerm
        _ex3EagerSTLC3EagerTypePretty = Text.pack . STLC3Eager.pprintType $ _ex3EagerSTLC3EagerType
        _ex3EagerSTLC3EagerTermPretty = Text.pack . STLC3Eager.pprintTerm $ _ex3EagerSTLC3EagerTerm
        _ex3EagerSTLC3EagerTermPrettyWithSig = Text.pack . STLC3Eager.pprintTermWithSig $ _ex3EagerSTLC3EagerTerm
        (_ex3EagerReducedSTLC3EagerTerm, _ex3EagerSTLC3EagerEvalStats) = STLC3Eager.nf' _ex3EagerSTLC3EagerTerm
        _ex3EagerReducedSTLC3EagerTermStats = STLC3Eager.countConstructors _ex3EagerReducedSTLC3EagerTerm
        _ex3EagerReducedSTLC3EagerTermPretty = Text.pack . STLC3Eager.pprintTerm $ _ex3EagerReducedSTLC3EagerTerm
        _ex3EagerReducedSTLC3EagerTermPrettyWithSig = Text.pack . STLC3Eager.pprintTermWithSig $ _ex3EagerReducedSTLC3EagerTerm
        _ex3EagerLCEagerTerm = STLC3Eager.toLCEager _ex3EagerSTLC3EagerTerm
        _ex3EagerLCEagerTermStats = LCEager.countConstructors _ex3EagerLCEagerTerm
        _ex3EagerLCEagerTermPretty = Text.pack . LCEager.pprintTerm $ _ex3EagerLCEagerTerm
        (_ex3EagerReducedLCEagerTerm, _ex3EagerLCEagerEvalStats) = LCEager.nf' _ex3EagerLCEagerTerm
        _ex3EagerReducedLCEagerTermStats = LCEager.countConstructors _ex3EagerReducedLCEagerTerm
        _ex3EagerReducedLCEagerTermPretty = Text.pack . LCEager.pprintTerm $ _ex3EagerReducedLCEagerTerm
     in Example3Eager {..}
  term = ex3EagerSTLC3EagerTerm
  prettyTerm = ex3EagerSTLC3EagerTermPretty
  prettyReducedTerm = ex3EagerReducedSTLC3EagerTermPretty
  prettyTerms = sequenceA [(^. ex3EagerSTLC3EagerTermPretty), (^. ex3EagerLCEagerTermPretty)]
  prettyReducedTerms = sequenceA [(^. ex3EagerReducedSTLC3EagerTermPretty), (^. ex3EagerReducedLCEagerTermPretty)]
  app = (STLC3Eager.:@)
  typeCheck tm = STLC3Eager.typeCheck' tm
  histogram' ex = do
    zoom hist3EagerSTLC3EagerTermStats . SS.modify $ STLC3Eager.updateTermHistogram (ex ^. ex3EagerSTLC3EagerTermStats)
    zoom hist3EagerSTLC3EagerEvalStats . SS.modify $ STLC3Eager.updateEvalHistogram (ex ^. ex3EagerSTLC3EagerEvalStats)
    zoom hist3EagerReducedSTLC3EagerTermStats . SS.modify $ STLC3Eager.updateTermHistogram (ex ^. ex3EagerReducedSTLC3EagerTermStats)
    zoom hist3EagerLCEagerTermStats . SS.modify $ LCEager.updateTermHistogram (ex ^. ex3EagerLCEagerTermStats)
    zoom hist3EagerLCEagerEvalStats . SS.modify $ LCEager.updateEvalHistogram (ex ^. ex3EagerLCEagerEvalStats)
    zoom hist3EagerReducedLCEagerTermStats . SS.modify $ LCEager.updateTermHistogram (ex ^. ex3EagerReducedLCEagerTermStats)

data Example3Lazy where
  Example3Lazy ::
    { -- | Example type
      _ex3LazySTLC3LazyType :: STLC3Eager.Ty 'Lazy,
      -- | Pretty-printed example type
      _ex3LazySTLC3LazyTypePretty :: Text,
      -- | Example simply-typed lambda calculus term
      _ex3LazySTLC3LazyTerm :: STLC3Eager.Exp 'Lazy Int,
      -- | Term statistics
      _ex3LazySTLC3LazyTermStats :: STLC3Eager.TermStats Int,
      -- | Pretty-printed example simply-typed lambda calculus term
      _ex3LazySTLC3LazyTermPretty :: Text,
      -- | Pretty-printed example simply-typed lambda calculus term with type signatures
      _ex3LazySTLC3LazyTermPrettyWithSig :: Text,
      -- | Evaluation statistics
      _ex3LazySTLC3LazyEvalStats :: STLC3Eager.EvalStats Int,
      -- | Reduced example simply-typed lambda calculus term
      _ex3LazyReducedSTLC3LazyTerm :: STLC3Eager.Exp 'Lazy Int,
      -- | Reduced term statistics
      _ex3LazyReducedSTLC3LazyTermStats :: STLC3Eager.TermStats Int,
      -- | Pretty-printed reduced example simply-typed lambda calculus term
      _ex3LazyReducedSTLC3LazyTermPretty :: Text,
      -- | Pretty-printed reduced example simply-typed lambda calculus term with type signatures
      _ex3LazyReducedSTLC3LazyTermPrettyWithSig :: Text,
      -- | Example term converted to untyped lambda calculus
      _ex3LazyLCLazyTerm :: LCEager.Exp 'Lazy Int,
      -- | Lambda calculus term statistics
      _ex3LazyLCLazyTermStats :: LCEager.TermStats Int,
      -- | Pretty-printed example lambda calculus term
      _ex3LazyLCLazyTermPretty :: Text,
      -- | Lambda calculus evaluation statistics
      _ex3LazyLCLazyEvalStats :: LCEager.EvalStats Int,
      -- | Reduced untyped lambda calculus term
      _ex3LazyReducedLCLazyTerm :: LCEager.Exp 'Lazy Int,
      -- | Reduced lambda calculus term statistics
      _ex3LazyReducedLCLazyTermStats :: LCEager.TermStats Int,
      -- | Pretty-printed reduced untyped lambda calculus term
      _ex3LazyReducedLCLazyTermPretty :: Text
    } ->
    Example3Lazy
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

makeLenses ''Example3Lazy
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 8} ''Example3Lazy)

data Histogram3Lazy a = Histogram3Lazy
  { _hist3LazySTLC3LazyTermStats :: STLC3Eager.TermStats a,
    _hist3LazySTLC3LazyEvalStats :: STLC3Eager.EvalStats a,
    _hist3LazyReducedSTLC3LazyTermStats :: STLC3Eager.TermStats a,
    _hist3LazyLCLazyTermStats :: LCEager.TermStats a,
    _hist3LazyLCLazyEvalStats :: LCEager.EvalStats a,
    _hist3LazyReducedLCLazyTermStats :: LCEager.TermStats a
  }
  deriving stock (Show, Eq, Ord, Functor)

instance Semigroup a => Semigroup (Histogram3Lazy a) where
  Histogram3Lazy a b c d e f <> Histogram3Lazy a' b' c' d' e' f' = Histogram3Lazy (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance Monoid a => Monoid (Histogram3Lazy a) where
  mempty = Histogram3Lazy mempty mempty mempty mempty mempty mempty

makeLenses ''Histogram3Lazy
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 10} ''Histogram3Lazy)

instance HasExamples 'STLC3Lazy where
  type Type 'STLC3Lazy = STLC3Eager.Ty 'Lazy
  type Term 'STLC3Lazy = STLC3Eager.Exp 'Lazy Int
  type Example 'STLC3Lazy = Example3Lazy
  type Histogram 'STLC3Lazy a = Histogram3Lazy a
  sample' = STLC3Eager.Sample.sample $ do
    stlc3LazyType <- STLC3Eager.Sample.genTy
    stlc3LazyTerm <- Gen.generalize $ STLC3Eager.Sample.genWellTypedExp stlc3LazyType
    pure (stlc3LazyType, stlc3LazyTerm)
  toExample' _ex3LazySTLC3LazyType _ex3LazySTLC3LazyTerm =
    let _ex3LazySTLC3LazyTermStats = STLC3Eager.countConstructors _ex3LazySTLC3LazyTerm
        _ex3LazySTLC3LazyTypePretty = Text.pack . STLC3Eager.pprintType $ _ex3LazySTLC3LazyType
        _ex3LazySTLC3LazyTermPretty = Text.pack . STLC3Eager.pprintTerm $ _ex3LazySTLC3LazyTerm
        _ex3LazySTLC3LazyTermPrettyWithSig = Text.pack . STLC3Eager.pprintTermWithSig $ _ex3LazySTLC3LazyTerm
        (_ex3LazyReducedSTLC3LazyTerm, _ex3LazySTLC3LazyEvalStats) = STLC3Eager.whnf' _ex3LazySTLC3LazyTerm
        _ex3LazyReducedSTLC3LazyTermStats = STLC3Eager.countConstructors _ex3LazyReducedSTLC3LazyTerm
        _ex3LazyReducedSTLC3LazyTermPretty = Text.pack . STLC3Eager.pprintTerm $ _ex3LazyReducedSTLC3LazyTerm
        _ex3LazyReducedSTLC3LazyTermPrettyWithSig = Text.pack . STLC3Eager.pprintTermWithSig $ _ex3LazyReducedSTLC3LazyTerm
        _ex3LazyLCLazyTerm = STLC3Eager.toLCEager _ex3LazySTLC3LazyTerm
        _ex3LazyLCLazyTermStats = LCEager.countConstructors _ex3LazyLCLazyTerm
        _ex3LazyLCLazyTermPretty = Text.pack . LCEager.pprintTerm $ _ex3LazyLCLazyTerm
        (_ex3LazyReducedLCLazyTerm, _ex3LazyLCLazyEvalStats) = LCEager.whnf' _ex3LazyLCLazyTerm
        _ex3LazyReducedLCLazyTermStats = LCEager.countConstructors _ex3LazyReducedLCLazyTerm
        _ex3LazyReducedLCLazyTermPretty = Text.pack . LCEager.pprintTerm $ _ex3LazyReducedLCLazyTerm
     in Example3Lazy {..}
  term = ex3LazySTLC3LazyTerm
  prettyTerm = ex3LazySTLC3LazyTermPretty
  prettyReducedTerm = ex3LazyReducedSTLC3LazyTermPretty
  prettyTerms = sequenceA [(^. ex3LazySTLC3LazyTermPretty), (^. ex3LazyLCLazyTermPretty)]
  prettyReducedTerms = sequenceA [(^. ex3LazyReducedSTLC3LazyTermPretty), (^. ex3LazyReducedLCLazyTermPretty)]
  app = (STLC3Eager.:@)
  typeCheck tm = STLC3Eager.typeCheck' tm
  histogram' ex = do
    zoom hist3LazySTLC3LazyTermStats . SS.modify $ STLC3Eager.updateTermHistogram (ex ^. ex3LazySTLC3LazyTermStats)
    zoom hist3LazySTLC3LazyEvalStats . SS.modify $ STLC3Eager.updateEvalHistogram (ex ^. ex3LazySTLC3LazyEvalStats)
    zoom hist3LazyReducedSTLC3LazyTermStats . SS.modify $ STLC3Eager.updateTermHistogram (ex ^. ex3LazyReducedSTLC3LazyTermStats)
    zoom hist3LazyLCLazyTermStats . SS.modify $ LCEager.updateTermHistogram (ex ^. ex3LazyLCLazyTermStats)
    zoom hist3LazyLCLazyEvalStats . SS.modify $ LCEager.updateEvalHistogram (ex ^. ex3LazyLCLazyEvalStats)
    zoom hist3LazyReducedLCLazyTermStats . SS.modify $ LCEager.updateTermHistogram (ex ^. ex3LazyReducedLCLazyTermStats)

-- | Filter by number of tokens.
filterByMaxTokens :: forall m r v. MonadIO m => Tokenize -> Int -> (v -> [Text]) -> P.Pipe v v m r
filterByMaxTokens tokenize maxTokens f =
  let p v =
        allM
          ( \t -> do
              let s = Text.unpack t
              xs <- liftIO $ tokenize s
              return $ length xs < maxTokens
          )
          (f v)
   in P.filterM p
  where
    allM _ [] = return True
    allM p (x : xs) = do
      q <- p x
      if q
        then allM p xs
        else return False

-- | Deduplicate the examples.
deduplicate :: forall m r a v. (Monad m, Eq a, Hashable a) => HashSet a -> (v -> a) -> P.Pipe v v m r
deduplicate s f = P.evalStateP s . P.for P.cat $ \ex -> do
  s' <- SL.get
  let a = f ex
  if not (HashSet.member a s')
    then do
      P.yield ex
      SL.modify (HashSet.insert a)
    else pure ()

-- | Populate cache.
cache :: forall m r a v. (Monad m, Eq a, Hashable a, SL.MonadState (HashMap a v) m) => (v -> a) -> P.Pipe v v m r
cache f = P.for P.cat $ \ex -> do
  SL.modify (HashMap.insert (f ex) ex)

-- | Produce compositional examples.
compositions :: forall m a l. (Monad m, Eq a, Hashable a, HasExamples l) => HashMap a (Example l) -> Maybe (HashSet a) -> P.Producer (Type l, Term l) m ()
compositions examples keys =
  forEachExample $ \ex -> do
    let tm = ex ^. term
    forEachExample $ \ex' -> do
      let tm' = ex' ^. term
          comp = app tm tm'
      case typeCheck comp of
        Nothing -> pure ()
        Just ty -> P.yield (ty, comp)
  where
    filteredExamples = case keys of
      Nothing -> examples
      Just s -> HashMap.filterWithKey (\a _ex -> HashSet.member a s) examples
    forEachExample = P.for (P.each . HashMap.elems $ filteredExamples)

histogram :: forall m r l. (Monad m, HasExamples l) => P.Consumer (Example l) (SS.StateT (Histogram l (IntMap Int)) m) r
histogram = P.for P.cat $ P.lift . histogram'

data HistogramRecord = HistogramRecord {_hrValue :: !Int, _hrCount :: !Int}
  deriving stock (Show, Eq, Ord)

makeLenses ''HistogramRecord
$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 3} ''HistogramRecord)

toRecords :: IntMap Int -> [HistogramRecord]
toRecords im = uncurry HistogramRecord <$> IntMap.toList im

-- | Write a JSON Lines text file.
writeJsonLines :: forall m a r. (P.MonadSafe m, Aeson.ToJSON a) => FilePath -> P.Consumer a m r
writeJsonLines file = P.withFile file IO.WriteMode $ \h ->
  P.map (BSL.toStrict . flip BSL.snoc 0x0a . Aeson.encodingToLazyByteString . Aeson.toEncoding)
    >-> P.for P.cat (\bs -> liftIO $ BS.hPut h bs >> IO.hFlush h)

-- | Read a JSON Lines text file.
readJsonLines :: forall m a. (P.MonadSafe m, Aeson.FromJSON a) => FilePath -> P.Producer a m ()
readJsonLines file = P.withFile file IO.ReadMode $ \h ->
  go h
  where
    go h = do
      eof <- liftIO $ IO.hIsEOF h
      unless eof $ do
        bs <- liftIO $ BS.hGetLine h
        case Aeson.eitherDecodeStrict' bs of
          Left err -> P.lift . P.throwM . userError $ "Failed to decode JSON: " <> err
          Right a -> P.yield a >> go h

data TrainingExample = TrainingExample
  { _teTermPretty :: Text,
    _teReducedTermPretty :: Text,
    _teNumSteps :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance CSV.FromNamedRecord TrainingExample where
  parseNamedRecord r =
    -- let opts = CSV.defaultOptions {CSV.fieldLabelModifier = drop 3}
    --  in CSV.genericParseNamedRecord opts r
    TrainingExample
      <$> r CSV..: "inputs"
      <*> r CSV..: "outputs"
      <*> r CSV..: "num_steps"

makeLenses ''TrainingExample

-- | Read a CSV file.
readCsv ::
  forall m a.
  (P.MonadSafe m, CSV.FromNamedRecord a) =>
  FilePath ->
  P.Producer a m ()
readCsv file =
  let opts = Csv.defaultDecodeOptions {Csv.decDelimiter = fromIntegral (ord ',')}
   in P.withFile file IO.ReadMode $ \h ->
        P.decodeByNameWith opts (P.fromHandle h)
          >-> P.concat