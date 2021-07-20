module Main where

import Control.Monad (void)
import Language.STLC2CLSpec (testSTLC2CL)
import Language.STLCSpec (testSTLC)

main :: IO ()
main = do
  void testSTLC
  void testSTLC2CL