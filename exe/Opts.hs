{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Opts where

import Control.Applicative (Alternative (empty, (<|>)), optional)
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
import qualified Options.Applicative as Opts

data Config f = Config
  { configOutputFolder :: f FilePath,
    configOutputDataFileName :: f FilePath,
    configOutputHistogramFileName :: f FilePath,
    configOutputConfigFileName :: f FilePath,
    configNumberOfExampes :: f Int,
    configSeed :: f Gen.Seed
  }
  deriving stock (Generic)
  deriving anyclass (Barbie.FunctorB, Barbie.TraversableB, Barbie.ApplicativeB, Barbie.ConstraintsB)

deriving stock instance (Barbie.AllBF Show f Config) => Show (Config f)

deriving stock instance (Barbie.AllBF Eq f Config) => Eq (Config f)

customOptions :: Aeson.Options
customOptions = Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 6}

instance (Barbie.AllBF Aeson.FromJSON f Config) => Aeson.FromJSON (Config f) where
  parseJSON = Aeson.genericParseJSON customOptions

instance (Barbie.AllBF Aeson.ToJSON f Config) => Aeson.ToJSON (Config f) where
  toJSON = Aeson.genericToJSON customOptions
  toEncoding = Aeson.genericToEncoding customOptions

$(Aeson.deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 4} ''Gen.Seed)

instance (Alternative f) => Semigroup (Config f) where
  (<>) = Barbie.bzipWith (<|>)

instance (Alternative f) => Monoid (Config f) where
  mempty = Barbie.bpure empty

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

cliConfigParser :: Config (Opts.Parser `Compose` Maybe)
cliConfigParser = Barbie.bmap (Compose . optional) parser
  where
    parser =
      Config
        { configOutputFolder =
            Opts.strOption $
              Opts.long "output-folder"
                <> Opts.short 'o'
                <> Opts.metavar "OUTPUT_FOLDER"
                <> Opts.help "Output folder",
          configOutputDataFileName =
            Opts.strOption $
              Opts.long "output-data-file-name"
                <> Opts.short 'd'
                <> Opts.metavar "OUTPUT_DATA_FILE_NAME"
                <> Opts.help "Output data file name",
          configOutputHistogramFileName =
            Opts.strOption $
              Opts.long "output-histogram-file-name"
                <> Opts.short 'h'
                <> Opts.metavar "OUTPUT_HISTOGRAM_FILE_NAME"
                <> Opts.help "Output histogram file name",
          configOutputConfigFileName =
            Opts.strOption $
              Opts.long "output-config-file-name"
                <> Opts.short 'c'
                <> Opts.metavar "OUTPUT_CONFIG_FILE_NAME"
                <> Opts.help "Output config file name",
          configNumberOfExampes =
            Opts.option Opts.auto $
              Opts.long "number-of-examples"
                <> Opts.short 'n'
                <> Opts.metavar "NUMBER_OF_EXAMPLES"
                <> Opts.help "Number of examples",
          configSeed =
            Gen.Seed.from
              <$> Opts.option
                Opts.auto
                ( Opts.long "seed"
                    <> Opts.short 's'
                    <> Opts.metavar "SEED"
                    <> Opts.help "Seed"
                )
        }

parserInfo ::
  forall b f.
  Barbie.TraversableB b =>
  b (Opts.Parser `Compose` f) ->
  Opts.ParserInfo (b f, Maybe FilePath)
parserInfo b =
  let parser =
        (,) <$> Barbie.bsequence b
          <*> optional
            ( Opts.option Opts.str $
                Opts.long "config-file"
                  <> Opts.short 'f'
                  <> Opts.metavar "CONFIG_FILE"
                  <> Opts.help "Input config file name"
            )
   in Opts.info parser $
        Opts.fullDesc
          <> Opts.progDesc "Generate and export datasets for neural interpretation"
          <> Opts.header "gen-tm - a tool for generating and exporting datasets for neural interpretation"

cliOpts :: IO (Config Maybe, Maybe FilePath)
cliOpts = Opts.execParser $ parserInfo cliConfigParser

configErrors :: Config (Const String)
configErrors =
  Config
    { configOutputFolder = "output folder",
      configOutputDataFileName = "output data file name",
      configOutputHistogramFileName = "output histogram file name",
      configOutputConfigFileName = "output config file name",
      configNumberOfExampes = "number of examples",
      configSeed = "seed"
    }

validateConfig ::
  forall b.
  (Barbie.TraversableB b, Barbie.ApplicativeB b) =>
  b (Const String) ->
  b Maybe ->
  Validation [String] (b Identity)
validateConfig errorMessages mb =
  Barbie.bsequence' $ Barbie.bzipWith validate mb errorMessages
  where
    validate :: forall a. Maybe a -> Const String a -> Validation [String] a
    validate (Just a) _ = Validation.Success a
    validate Nothing (Const errorMessage) = Validation.Failure [errorMessage]

config :: IO (Validation [String] (Config Identity))
config = do
  (c, mConfigFile) <- cliOpts
  validateConfig configErrors
    <$> maybe
      (pure c)
      (fmap ((c <>) . jsonConfig) . readConfigFile)
      mConfigFile
