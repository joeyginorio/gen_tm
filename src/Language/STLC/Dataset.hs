{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

{- Dataset.hs
   ==========
   Uses term generator to generate dataset in a JSON file, namely input-output
   pairs where:
      input  := unnormalized term
      output := normalized   term -}


module Language.STLC.Dataset where

import Language.STLC
-- import Language.STLC.Gen

import GHC.Generics
-- import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson

deriving instance Generic Type
deriving instance ToJSON Type

deriving instance Generic Term
deriving instance ToJSON Term

data IOPair = IOPair {input :: Term, output :: Term}
              deriving (Show, Generic, ToJSON)

iop = IOPair TmUnit TmUnit

main :: IO ()
main = I.writeFile "src/Language/STLC/myfile.json" (encodeToLazyText [iop,iop])
