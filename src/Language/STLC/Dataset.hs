{- Dataset.hs
   ==========
   Uses term generator to generate dataset in a JSON file, namely input-output
   pairs where:
      input  := unnormalized term
      output := normalized   term -}

module Language.STLC.Dataset where

import Language.STLC.Gen
import Language.STLC
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B


{- =============================== JSON Format ============================== -}

-- | Representing an input/output pair
data IOPair a b = IOPair {input :: a, output :: b}
              deriving Show

-- | Encoding an input/output pair in JSON
instance (Show a, Show b) => ToJSON (IOPair a b) where
  toJSON (IOPair i o) = object [
    pack "input"  .=  show i,
    pack "output" .=  show o
                               ]

-- NOTE: Aeson already defines an encoding of lists to JSON, so our dataset
--       will be represented by [IOPair a b].


{- ============================= Data Generation ============================ -}

