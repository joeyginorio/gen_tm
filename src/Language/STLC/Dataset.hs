{- Dataset.hs
   ==========
   Uses term generator to generate dataset in a JSON file, namely input-output
   pairs where:
      input  := unnormalized term
      output := normalized   term -}

module Language.STLC.Dataset where

import Language.STLC.Gen
import Language.STLC
import Language.STLC2CL (compile)
import qualified Language.CL as CL
import Data.Aeson
import Data.Text (pack)
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

-- | Dataset is a list of IOPairs
type Dataset a = [IOPair a a]

-- | Helper for building datasets
buildDataset :: [a] -> [a] -> Dataset a
buildDataset = zipWith IOPair

-- | Generate two datasets, lhs of tuple is STLC, rhs of tuple is CL
-- | NOTE: The input/output pairs are unnormalized/normalized terms
genDatasets :: Int -> (Dataset Term, Dataset (Maybe CL.Term))
genDatasets n = (buildDataset ins outs, buildDataset ins' outs')
                where ins   = take n $ evalSearchS gen
                      outs  = map evalR ins
                      ins'  = map compile ins
                      outs' = map (fmap CL.reduce) ins'

-- | Exports dataset as a JSON
exportDataset :: (Show a) => FilePath -> Dataset a -> IO ()
exportDataset fname ds = B.writeFile fname (encode ds)

