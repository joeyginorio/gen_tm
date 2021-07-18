{- Dataset.hs
   ==========
   Uses term generator to generate dataset in a JSON file, namely input-output
   pairs where:
      input  := unnormalized term
      output := normalized   term -}

import Language.STLC.Gen
import Data.Aeson
