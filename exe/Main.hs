{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.Either.Validation as Validation
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Text.Lazy as Text.Lazy
import Data.Traversable (for)
import qualified Dataset
import qualified Opts
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Lift as P
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude as P
import qualified System.Console.ANSI.Codes as ANSI
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.ProgressBar as PB

main :: IO ()
main = do
  v <- Opts.config
  case v of
    Validation.Failure errorMessageList -> do
      error $
        "The following options are required but not provided: "
          <> List.intercalate ", " errorMessageList
    Validation.Success (Opts.Config (Opts.Tm config)) -> genTmAndExport config
    Validation.Success (Opts.Config (Opts.Comp config)) -> genCompAndExport config

genTmAndExport :: Opts.GenTmConfig Identity -> IO ()
genTmAndExport config@Opts.GenTmConfig {..} =
  P.runSafeT $ do
    saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputConfigFileName) (Opts.Config . Opts.Tm $ config)
    hist <-
      P.runEffect . P.execStateP mempty $
        Dataset.sampleStlc (runIdentity genTmConfigSeed)
          >-> Dataset.toExample
          >-> Dataset.deduplicate HashSet.empty (^. Dataset.exSTLC2TermPretty)
          >-> P.take (runIdentity genTmConfigNumberOfExampes)
          >-> P.tee (showProgress $ runIdentity genTmConfigNumberOfExampes)
          >-> P.tee (Dataset.writeJsonLines (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputDataFileName))
          >-> Dataset.histogram
    let hist' = fmap Dataset.toRecords hist
    saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputHistogramFileName) hist'

genCompAndExport :: Opts.GenCompConfig Identity -> IO ()
genCompAndExport config@Opts.GenCompConfig {..} =
  P.runSafeT $ do
    saveAsJson (runIdentity genCompConfigOutputFolder </> runIdentity genCompConfigOutputConfigFileName) (Opts.Config . Opts.Comp $ config)
    examples <-
      P.runEffect . P.execStateP mempty $
        Dataset.readJsonLines (runIdentity genCompConfigInputFolder </> runIdentity genCompConfigInputDataFileName)
          >-> Dataset.cache (^. Dataset.exSTLC2TermPretty)
          >-> P.drain
    liftIO . print $ "Read " <> show (length examples) <> " examples."
    keys <- for (runIdentity genCompConfigInputTrainingDataCSVFile) $ \fileName ->
      P.runEffect . P.execStateP mempty $
        Dataset.readCsv fileName
          >-> Dataset.cache (^. Dataset.teTermPretty)
          >-> P.drain
    liftIO . print $ case keys of
      Just ks -> "Read " <> show (length ks) <> " keys."
      Nothing -> "Skipped reading keys."
    P.runEffect $
      Dataset.compositions examples (HashMap.keysSet <$> keys)
        >-> Dataset.deduplicate (HashMap.keysSet examples) (^. Dataset.exSTLC2TermPretty)
        >-> P.take (runIdentity genCompConfigNumberOfExampes)
        >-> P.tee (showProgress $ runIdentity genCompConfigNumberOfExampes)
        >-> Dataset.writeJsonLines (runIdentity genCompConfigOutputFolder </> runIdentity genCompConfigOutputDataFileName)

saveAsJson :: forall m a. (P.MonadSafe m, ToJSON a) => FilePath -> a -> m ()
saveAsJson file a = P.withFile file IO.WriteMode $ \h ->
  let json = toStrict . encodingToLazyByteString . toEncoding $ a
   in liftIO $ BS.hPut h json

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
