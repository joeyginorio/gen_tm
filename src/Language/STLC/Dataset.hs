{- Dataset.hs
   ==========
   Uses term generator to generate dataset in a JSON file, namely input-output
   pairs where:
      input  := unnormalized term
      output := normalized   term -}

module Language.STLC.Dataset where

import Language.STLC.Gen
import Data.Aeson
