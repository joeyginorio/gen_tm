{- Main.hs
   =======
   Provides a CLI for generating and exporting datasets for STLC and CL, where
   input  := unnormalized term
   output := normalized term . -}

module Main where

import Language.STLC.Dataset
import System.Environment

-- >>> :main 5 "data/stlc.json" "data/cl.json"
main :: IO ()
main = do args <- getArgs
          let n      = args !! 0
          let fname1 = args !! 1
          let fname2 = args !! 2
          let dsets  = genDatasets (read n)
          exportDataset fname1 (fst dsets)
          exportDataset fname2 (snd dsets)
