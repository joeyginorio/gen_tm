{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Opts where

import Control.Applicative ((<**>))
import Data.Aeson.TH (defaultOptions, deriveJSON, Options (fieldLabelModifier))
import qualified Hedgehog as Gen
import qualified Hedgehog.Internal.Seed as Gen.Seed
import qualified Options.Applicative as Opts

data Config = Config
  { configOutputFolder :: String,
    configOutputDataFileName :: String,
    configOutputHistogramFileName :: String,
    configOutputConfigFileName :: String,
    configNumberOfExampes :: Int,
    configSeed :: Gen.Seed
  }
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Gen.Seed)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Config)

config :: Opts.Parser Config
config =
  Config
    <$> Opts.strOption
      ( Opts.long "output-folder"
          <> Opts.short 'o'
          <> Opts.metavar "OUTPUT_FOLDER"
          <> Opts.help "Output folder"
          <> Opts.showDefault
          <> Opts.value "."
      )
    <*> Opts.strOption
      ( Opts.long "output-data-file-name"
          <> Opts.short 'd'
          <> Opts.metavar "OUTPUT_DATA_FILE_NAME"
          <> Opts.help "Output data file name"
          <> Opts.showDefault
          <> Opts.value "data.jsonl"
      )
    <*> Opts.strOption
      ( Opts.long "output-histogram-file-name"
          <> Opts.short 'h'
          <> Opts.metavar "OUTPUT_HISTOGRAM_FILE_NAME"
          <> Opts.help "Output histogram file name"
          <> Opts.showDefault
          <> Opts.value "histogram.json"
      )
    <*> Opts.strOption
      ( Opts.long "output-config-file-name"
          <> Opts.short 'c'
          <> Opts.metavar "OUTPUT_CONFIG_FILE_NAME"
          <> Opts.help "Output config file name"
          <> Opts.showDefault
          <> Opts.value "config.json"
      )
    <*> Opts.option
      Opts.auto
      ( Opts.long "number-of-examples"
          <> Opts.short 'n'
          <> Opts.metavar "INT"
          <> Opts.help "Number of examples to generate"
          <> Opts.showDefault
          <> Opts.value 1
      )
    <*> ( Gen.Seed.from
            <$> Opts.option
              Opts.auto
              ( Opts.long "seed"
                  <> Opts.short 's'
                  <> Opts.metavar "INT"
                  <> Opts.help "Seed for the random number generator"
                  <> Opts.showDefault
                  <> Opts.value 43
              )
        )

opts :: Opts.ParserInfo Config
opts =
  Opts.info
    (Opts.config <**> Opts.helper)
    ( Opts.fullDesc
        <> Opts.progDesc "Generate and export datasets for STLC and CL"
        <> Opts.header "gen-tm - a tool for generating and exporting datasets for STLC and CL"
    )