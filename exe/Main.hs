{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

{- Main.hs
   =======
   Provides a CLI for generating and exporting datasets for STLC and CL. -}

module Main where

import Dataset (sampleStlc, stlc, toExample, writeJsonLines)
import Hedgehog (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import Options.Applicative (Parser, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, showDefault, strOption, value, (<**>))
import Pipes (runEffect, (>->))
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import Control.Applicative
import Control.Monad.Trans.State
import Language.STLC(ids)

data GenTmOpts = GenTmOpts
  { outputFileName :: String,
    numberOfExampes :: Int,
    seed :: Seed
  }

genTmOpts :: Parser GenTmOpts
genTmOpts =
  GenTmOpts
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILENAME"
          <> help "Output filename"
          <> showDefault
          <> value "examples.jsonl"
      )
    <*> option
      auto
      ( long "number-of-examples"
          <> short 'n'
          <> metavar "INT"
          <> help "Number of examples to generate"
          <> showDefault
          <> value 1
      )
    <*> ( Seed.from
            <$> option
              auto
              ( long "seed"
                  <> short 's'
                  <> metavar "INT"
                  <> help "Seed for the random number generator"
                  <> showDefault
                  <> value 43
              )
        )


main :: IO ()
main = generateAndExport' =<< execParser opts
  where
    opts =
      info
        (genTmOpts <**> helper)
        ( fullDesc
            <> progDesc "Generate and export datasets for STLC and CL"
            <> header "gen-tm - a tool for generating and exporting datasets for STLC and CL"
        )

generateAndExport :: GenTmOpts -> IO ()
generateAndExport GenTmOpts {..} =
  runSafeT . runEffect $
    stlc
      >-> P.map (\(cost, ty, tm) -> (Just cost, ty, tm))
      >-> toExample
      >-> P.take numberOfExampes
      >-> writeJsonLines outputFileName

generateAndExport' :: GenTmOpts -> IO ()
generateAndExport' GenTmOpts {..} =
  runSafeT . runEffect $
    sampleStlc seed
      >-> P.map (\(ty, tm) -> (Nothing, ty, tm))
      >-> toExample
      >-> P.take numberOfExampes
      >-> writeJsonLines outputFileName
