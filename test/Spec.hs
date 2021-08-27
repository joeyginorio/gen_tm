module Main where

import Control.Monad (void)
import Language.STLC2.ToCLSpec (testToCL)
import Language.STLC2Spec (testSTLC)

main :: IO ()
main = do
  void testSTLC
  void testToCL
