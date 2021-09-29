{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Opts where

import Control.Applicative (Alternative (empty, (<|>)), optional, (<**>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS
import Data.Either.Validation (Validation)
import qualified Data.Either.Validation as Validation
import qualified Data.Functor.Barbie as Barbie
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import qualified Hedgehog as Gen
import qualified Hedgehog.Internal.Seed as Gen.Seed
import qualified Options.Applicative as Options

data Config f = Config
  { configCommand :: !(Command f)
  }
  deriving stock (Generic)
  deriving anyclass (Barbie.FunctorB, Barbie.TraversableB, Barbie.ConstraintsB)

deriving stock instance (Barbie.AllBF Show f Config) => Show (Config f)

deriving stock instance (Barbie.AllBF Eq f Config) => Eq (Config f)

configCustomJSONOptions :: Aeson.Options
configCustomJSONOptions = Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 6}

instance (Barbie.AllBF Aeson.FromJSON f Config) => Aeson.FromJSON (Config f) where
  parseJSON = Aeson.genericParseJSON configCustomJSONOptions

instance (Barbie.AllBF Aeson.ToJSON f Config) => Aeson.ToJSON (Config f) where
  toJSON = Aeson.genericToJSON configCustomJSONOptions
  toEncoding = Aeson.genericToEncoding configCustomJSONOptions

data Command f
  = Tm !(GenTmConfig f)
  | Comp !(GenCompConfig f)
  deriving stock (Generic)
  deriving anyclass (Barbie.FunctorB, Barbie.TraversableB, Barbie.ConstraintsB)

deriving stock instance (Barbie.AllBF Show f Command) => Show (Command f)

deriving stock instance (Barbie.AllBF Eq f Command) => Eq (Command f)

commandCustomJSONOptions :: Aeson.Options
commandCustomJSONOptions = Aeson.defaultOptions

instance (Barbie.AllBF Aeson.FromJSON f Command) => Aeson.FromJSON (Command f) where
  parseJSON = Aeson.genericParseJSON commandCustomJSONOptions

instance (Barbie.AllBF Aeson.ToJSON f Command) => Aeson.ToJSON (Command f) where
  toJSON = Aeson.genericToJSON commandCustomJSONOptions
  toEncoding = Aeson.genericToEncoding commandCustomJSONOptions

data Language = STLC2 | STLC3 | STLC3Eager | STLC3Lazy
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

language :: Options.ReadM Language
language =
  Options.str >>= \case
    "stlc2" -> return STLC2
    "stlc3" -> return STLC3
    "stlc3-eager" -> return STLC3Eager
    "stlc3-lazy" -> return STLC3Lazy
    (_ :: String) -> Options.readerError "Accepted languages are: stlc2, stlc3, stlc3-eager, and stlc3-lazy"

data GenTmConfig f = GenTmConfig
  { genTmConfigOutputFolder :: !(f FilePath),
    genTmConfigOutputDataFileName :: !(f FilePath),
    genTmConfigOutputHistogramFileName :: !(f FilePath),
    genTmConfigOutputConfigFileName :: !(f FilePath),
    genTmConfigNumberOfExampes :: !(f Int),
    genTmConfigSeed :: !(f Gen.Seed),
    genTmConfigLanguage :: !(f Language)
  }
  deriving stock (Generic)
  deriving anyclass (Barbie.FunctorB, Barbie.TraversableB, Barbie.ApplicativeB, Barbie.ConstraintsB)

deriving stock instance (Barbie.AllBF Show f GenTmConfig) => Show (GenTmConfig f)

deriving stock instance (Barbie.AllBF Eq f GenTmConfig) => Eq (GenTmConfig f)

instance (Alternative f) => Semigroup (GenTmConfig f) where
  (<>) = Barbie.bzipWith (<|>)

instance (Alternative f) => Monoid (GenTmConfig f) where
  mempty = Barbie.bpure empty

genTmConfigCustomJSONOptions :: Aeson.Options
genTmConfigCustomJSONOptions = Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 11}

instance (Barbie.AllBF Aeson.FromJSON f GenTmConfig) => Aeson.FromJSON (GenTmConfig f) where
  parseJSON = Aeson.genericParseJSON genTmConfigCustomJSONOptions

instance (Barbie.AllBF Aeson.ToJSON f GenTmConfig) => Aeson.ToJSON (GenTmConfig f) where
  toJSON = Aeson.genericToJSON genTmConfigCustomJSONOptions
  toEncoding = Aeson.genericToEncoding genTmConfigCustomJSONOptions

data GenCompConfig f = GenCompConfig
  { genCompConfigOutputFolder :: !(f FilePath),
    genCompConfigOutputConfigFileName :: !(f FilePath),
    genCompConfigOutputDataFileName :: !(f FilePath),
    genCompConfigInputFolder :: !(f FilePath),
    genCompConfigInputDataFileName :: !(f FilePath),
    genCompConfigInputTrainingDataCSVFile :: !(f (Maybe FilePath)),
    genCompConfigNumberOfExampes :: !(f Int),
    genCompConfigLanguage :: !(f Language)
  }
  deriving stock (Generic)
  deriving anyclass (Barbie.FunctorB, Barbie.TraversableB, Barbie.ApplicativeB, Barbie.ConstraintsB)

deriving stock instance (Barbie.AllBF Show f GenCompConfig) => Show (GenCompConfig f)

deriving stock instance (Barbie.AllBF Eq f GenCompConfig) => Eq (GenCompConfig f)

instance (Alternative f) => Semigroup (GenCompConfig f) where
  (<>) = Barbie.bzipWith (<|>)

instance (Alternative f) => Monoid (GenCompConfig f) where
  mempty = Barbie.bpure empty

genCompConfigCustomJSONOptions :: Aeson.Options
genCompConfigCustomJSONOptions = Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 13}

instance (Barbie.AllBF Aeson.FromJSON f GenCompConfig) => Aeson.FromJSON (GenCompConfig f) where
  parseJSON = Aeson.genericParseJSON genCompConfigCustomJSONOptions

instance (Barbie.AllBF Aeson.ToJSON f GenCompConfig) => Aeson.ToJSON (GenCompConfig f) where
  toJSON = Aeson.genericToJSON genCompConfigCustomJSONOptions
  toEncoding = Aeson.genericToEncoding genCompConfigCustomJSONOptions

$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 4} ''Gen.Seed)

readConfigFile :: FilePath -> IO Aeson.Value
readConfigFile filePath = do
  res <- Atto.parseOnly Aeson.jsonEOF' <$> BS.readFile filePath
  case res of
    Left _err -> error "Failed to parse config file"
    Right val -> pure val

jsonConfig :: forall b. (Aeson.FromJSON (b Maybe), Barbie.ApplicativeB b) => Aeson.Value -> b Maybe
jsonConfig = fromResult . Aeson.fromJSON
  where
    fromResult (Aeson.Success a) = a
    fromResult (Aeson.Error _) = Barbie.bpure Nothing

genTmConfigParser :: Config (Options.Parser `Compose` Maybe)
genTmConfigParser = Barbie.bmap (Compose . optional) parser
  where
    parser =
      Config . Tm $
        GenTmConfig
          { genTmConfigOutputFolder =
              Options.strOption $
                Options.long "output-folder"
                  <> Options.short 'o'
                  <> Options.metavar "OUTPUT_FOLDER"
                  <> Options.help "Output folder",
            genTmConfigOutputDataFileName =
              Options.strOption $
                Options.long "output-data-file-name"
                  <> Options.short 'd'
                  <> Options.metavar "OUTPUT_DATA_FILE_NAME"
                  <> Options.help "Output data file name",
            genTmConfigOutputHistogramFileName =
              Options.strOption $
                Options.long "output-histogram-file-name"
                  <> Options.short 'h'
                  <> Options.metavar "OUTPUT_HISTOGRAM_FILE_NAME"
                  <> Options.help "Output histogram file name",
            genTmConfigOutputConfigFileName =
              Options.strOption $
                Options.long "output-config-file-name"
                  <> Options.short 'c'
                  <> Options.metavar "OUTPUT_CONFIG_FILE_NAME"
                  <> Options.help "Output config file name",
            genTmConfigNumberOfExampes =
              Options.option Options.auto $
                Options.long "number-of-examples"
                  <> Options.short 'n'
                  <> Options.metavar "NUMBER_OF_EXAMPLES"
                  <> Options.help "Number of examples",
            genTmConfigSeed =
              Gen.Seed.from
                <$> Options.option
                  Options.auto
                  ( Options.long "seed"
                      <> Options.short 's'
                      <> Options.metavar "SEED"
                      <> Options.help "Seed"
                  ),
            genTmConfigLanguage =
              Options.option language $
                Options.long "language"
                  <> Options.short 'l'
                  <> Options.metavar "LANGUAGE"
                  <> Options.help "Language (stlc2, stlc3)"
          }

genCompConfigParser :: Config (Options.Parser `Compose` Maybe)
genCompConfigParser = Barbie.bmap (Compose . optional) parser
  where
    parser =
      Config . Comp $
        GenCompConfig
          { genCompConfigOutputFolder =
              Options.strOption $
                Options.long "output-folder"
                  <> Options.short 'o'
                  <> Options.metavar "OUTPUT_FOLDER"
                  <> Options.help "Output folder",
            genCompConfigOutputConfigFileName =
              Options.strOption $
                Options.long "output-config-file-name"
                  <> Options.short 'c'
                  <> Options.metavar "OUTPUT_CONFIG_FILE_NAME"
                  <> Options.help "Output config file name",
            genCompConfigOutputDataFileName =
              Options.strOption $
                Options.long "output-data-file-name"
                  <> Options.short 'd'
                  <> Options.metavar "OUTPUT_DATA_FILE_NAME"
                  <> Options.help "Output data file name",
            genCompConfigInputFolder =
              Options.strOption $
                Options.long "input-folder"
                  <> Options.short 'i'
                  <> Options.metavar "INPUT_FOLDER"
                  <> Options.help "Input folder",
            genCompConfigInputDataFileName =
              Options.strOption $
                Options.long "input-data-file-name"
                  <> Options.short 'a'
                  <> Options.metavar "INPUT_DATA_FILE_NAME"
                  <> Options.help "Input data file name",
            genCompConfigInputTrainingDataCSVFile =
              optional . Options.strOption $
                Options.long "input-training-data-csv-file"
                  <> Options.short 't'
                  <> Options.metavar "INPUT_TRAINING_DATA_CSV_FILE"
                  <> Options.help "Input training data CSV file",
            genCompConfigNumberOfExampes =
              Options.option Options.auto $
                Options.long "number-of-examples"
                  <> Options.short 'n'
                  <> Options.metavar "NUMBER_OF_EXAMPLES"
                  <> Options.help "Number of examples",
            genCompConfigLanguage =
              Options.option language $
                Options.long "language"
                  <> Options.short 'l'
                  <> Options.metavar "LANGUAGE"
                  <> Options.help "Language (stlc2, stlc3)"
          }

parserInfo ::
  forall b f.
  Barbie.TraversableB b =>
  b (Options.Parser `Compose` f) ->
  String ->
  Options.ParserInfo (b f, Maybe FilePath)
parserInfo b desc =
  let parser =
        (,) <$> Barbie.bsequence b
          <*> optional
            ( Options.option Options.str $
                Options.long "config-file"
                  <> Options.short 'f'
                  <> Options.metavar "CONFIG_FILE"
                  <> Options.help "Input config file name"
            )
   in Options.info parser $ Options.progDesc desc

command :: Options.Parser (Config Maybe, Maybe FilePath)
command =
  Options.hsubparser $
    commandOpts "tm" genTmConfigParser "Generate terms"
      <> commandOpts "comp" genCompConfigParser "Generate compositions of terms"
  where
    commandOpts cmd parser desc = Options.command cmd $ parserInfo parser desc

opts :: Options.ParserInfo (Config Maybe, Maybe FilePath)
opts =
  Options.info (command <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc "Generate and export datasets for neural interpretation"
      <> Options.header "gen-tm - a tool for generating and exporting datasets for neural interpretation"

genTmConfigErrors :: GenTmConfig (Const String)
genTmConfigErrors =
  GenTmConfig
    { genTmConfigOutputFolder = "output folder",
      genTmConfigOutputDataFileName = "output data file name",
      genTmConfigOutputHistogramFileName = "output histogram file name",
      genTmConfigOutputConfigFileName = "output config file name",
      genTmConfigNumberOfExampes = "number of examples",
      genTmConfigSeed = "seed",
      genTmConfigLanguage = "language"
    }

genCompConfigErrors :: GenCompConfig (Const String)
genCompConfigErrors =
  GenCompConfig
    { genCompConfigOutputFolder = "output folder",
      genCompConfigOutputConfigFileName = "output config file name",
      genCompConfigOutputDataFileName = "output data file name",
      genCompConfigInputFolder = "input folder",
      genCompConfigInputDataFileName = "input data file name",
      genCompConfigInputTrainingDataCSVFile = "input training data CSV file",
      genCompConfigNumberOfExampes = "number of examples",
      genCompConfigLanguage = "language"
    }

validate ::
  forall b.
  (Barbie.TraversableB b, Barbie.ApplicativeB b) =>
  b (Const String) ->
  b Maybe ->
  Validation [String] (b Identity)
validate errorMessages mb =
  Barbie.bsequence' $ Barbie.bzipWith go mb errorMessages
  where
    go :: forall a. Maybe a -> Const String a -> Validation [String] a
    go (Just a) _ = Validation.Success a
    go Nothing (Const errorMessage) = Validation.Failure [errorMessage]

config :: IO (Validation [String] (Config Identity))
config = do
  (Config cmd, mConfigFile) <- Options.execParser opts
  let go c =
        maybe
          (pure c)
          (fmap ((c <>) . jsonConfig) . readConfigFile)
          mConfigFile
  case cmd of
    Tm c ->
      (Config . Tm <$>) . validate genTmConfigErrors
        <$> go c
    Comp c ->
      (Config . Comp <$>) . validate genCompConfigErrors
        <$> go c
