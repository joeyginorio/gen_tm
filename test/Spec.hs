module Main where

import Control.Monad (void)
import Hedgehog.Main (defaultMain)
import Language.LC2.ToLCSpec (testSTLC3ToLC2ToLC)
import Language.STLC2.ToCLSpec (testSTLC2ToCL)
import Language.STLC2.ToLCSpec (testSTLC2ToLC)
import Language.STLC2Spec (testSTLC2)
import Language.STLC3.ToLC2Spec (testSTLC3ToLC2)
import Language.STLC3Eager.ToLCEagerSpec (testSTLC3EagerToLCEager)
import Language.STLC3EagerSpec (testSTLC3Eager)
import Language.STLC3Spec (testSTLC3)

main :: IO ()
main =
  defaultMain
    [ -- testSTLC2,
      -- testSTLC2ToCL,
      -- testSTLC2ToLC,
      -- testSTLC3,
      -- testSTLC3ToLC2,
      -- testSTLC3ToLC2ToLC,
      -- testSTLC3Eager,
      testSTLC3EagerToLCEager
    ]
