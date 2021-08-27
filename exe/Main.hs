{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- Main.hs
   =======
   Provides a CLI for generating and exporting datasets for STLC and CL. -}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text.Lazy as Text.Lazy
import qualified Dataset
import qualified Options.Applicative as Opts
import qualified Opts
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import qualified System.Console.ANSI.Codes as ANSI
import qualified System.ProgressBar as PB

main :: IO ()
main = generateAndExport =<< Opts.execParser Opts.opts

generateAndExport :: Opts.Config -> IO ()
generateAndExport Opts.Config {..} =
  P.runSafeT . P.runEffect $
    Dataset.sampleStlc configSeed
      >-> Dataset.toExample
      >-> Dataset.deduplicate (\Dataset.Example {..} -> exSTLC2TermPretty)
      >-> P.take configNumberOfExampes
      >-> P.tee (showProgress configNumberOfExampes)
      >-> Dataset.writeJsonLines configOutputFileName

showProgress :: forall a m r. MonadIO m => Int -> P.Consumer a m r
showProgress numberOfExamples = do
  let maxRefreshRate = 10
      setSGRCodeText = Text.Lazy.pack . ANSI.setSGRCode
      style =
        PB.defStyle
          { PB.styleDone = '▪',
            PB.styleCurrent = '▶',
            PB.styleTodo = ' ',
            PB.styleOpen = "",
            PB.styleClose = "",
            PB.styleEscapeDone = const $ setSGRCodeText [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green],
            PB.styleEscapePostfix = const $ setSGRCodeText [ANSI.Reset],
            PB.stylePrefix = PB.percentage,
            PB.stylePostfix = PB.exact <> " " <> PB.elapsedTime PB.renderDuration <> "/" <> PB.totalTime PB.renderDuration "...",
            PB.styleWidth = PB.TerminalWidth (13 + 60)
          }
  pb <-
    P.lift . liftIO $
      PB.newProgressBar
        style
        maxRefreshRate
        (PB.Progress 0 numberOfExamples ())
  P.for P.cat $
    \_ ->
      P.lift . liftIO $
        PB.updateProgress pb (\progress -> progress {PB.progressDone = PB.progressDone progress + 1})
