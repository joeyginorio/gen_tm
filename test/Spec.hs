module Main where

import Control.Monad (void)
import Language.STLC2.ToCLSpec (testSTLC2ToCL)
import Language.STLC2.ToLCSpec (testSTLC2ToLC)
import Language.STLC2Spec (testSTLC2)
import Language.STLC3.ToLC2Spec (testSTLC3ToLC2)
import Language.STLC3Spec (testSTLC3)

main :: IO ()
main = do
  void testSTLC2
  void testSTLC2ToCL
  void testSTLC2ToLC
  void testSTLC3
  void testSTLC3ToLC2