{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

{- Main.hs
   =======
   Provides a CLI for generating and exporting datasets for STLC and CL. -}

module Main where

import qualified Dataset
import qualified Options.Applicative as Opts
import qualified Opts
import Pipes ((>->))
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import qualified Pipes as P

main :: IO ()
main = generateAndExport =<< Opts.execParser Opts.opts

generateAndExport :: Opts.Config -> IO ()
generateAndExport Opts.Config {..} =
  P.runSafeT . P.runEffect $
    Dataset.sampleStlc seed
      >-> Dataset.toExample
      >-> Dataset.deduplicate (\Dataset.Example {..} -> exSTLC2TermPretty)
      >-> P.take numberOfExampes
      >-> Dataset.writeJsonLines outputFileName
