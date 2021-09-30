module Main where

import Build_doctests (flags, module_sources, pkgs)
import Data.Foldable (traverse_)
import System.Environment (lookupEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
  libDir <- lookupEnv "NIX_GHC_LIBDIR"

  let args =
        concat
          [ flags,
            pkgs,
            maybe [] (\x -> ["-package-db " <> x <> "/package.conf.d"]) libDir,
            ["-XOverloadedStrings", "-XScopedTypeVariables", "-XDataKinds"],
            module_sources
          ]

  -- traverse_ putStrLn args
  doctest args