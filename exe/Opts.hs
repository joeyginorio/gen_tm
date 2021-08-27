module Opts where

import Control.Applicative ((<**>))
import qualified Hedgehog as Gen
import qualified Hedgehog.Internal.Seed as Gen.Seed
import qualified Options.Applicative as Opts

data Config = Config
  { outputFileName :: String,
    numberOfExampes :: Int,
    seed :: Gen.Seed
  }

config :: Opts.Parser Config
config =
  Config
    <$> Opts.strOption
      ( Opts.long "output"
          <> Opts.short 'o'
          <> Opts.metavar "FILENAME"
          <> Opts.help "Output filename"
          <> Opts.showDefault
          <> Opts.value "examples.jsonl"
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