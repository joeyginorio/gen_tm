{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Control.Concurrent.MSem as MSem
import Control.Lens ((^.), _1)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toEncoding))
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.Either.Validation as Validation
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IntMap (IntMap)
import qualified Data.List as List
import Data.Text (Text)
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
import qualified Tokenizers

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
  Tokenizers.withTokenizerFromConfigFile (runIdentity genTmConfigTokenizer) $ \tokenizer -> do
    tokenizerSem <- MSem.new (1 :: Int)
    let tokenize input = MSem.with tokenizerSem $ do
          encoding <- Tokenizers.encode tokenizer input
          Tokenizers.getIDs encoding
    P.runSafeT $ do
      saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputConfigFileName) (Opts.Config . Opts.Tm $ config)
      case runIdentity genTmConfigLanguage of
        Opts.STLC2 -> do
          hist :: Dataset.Histogram2 (IntMap Int) <- go tokenize
          let hist' = fmap Dataset.toRecords hist
          saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputHistogramFileName) hist'
        Opts.STLC3 -> do
          hist :: Dataset.Histogram3 (IntMap Int) <- go tokenize
          let hist' = fmap Dataset.toRecords hist
          saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputHistogramFileName) hist'
        Opts.STLC3Eager -> do
          hist :: Dataset.Histogram3Eager (IntMap Int) <- go tokenize
          let hist' = fmap Dataset.toRecords hist
          saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputHistogramFileName) hist'
        Opts.STLC3Lazy -> do
          hist :: Dataset.Histogram3Lazy (IntMap Int) <- go tokenize
          let hist' = fmap Dataset.toRecords hist
          saveAsJson (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputHistogramFileName) hist'
  where
    go :: forall l. Dataset.HasExamples l => Dataset.Tokenize -> P.SafeT IO (Dataset.Histogram l (IntMap Int))
    go tokenize =
      P.runEffect . P.execStateP mempty $
        Dataset.sample (runIdentity genTmConfigSeed)
          >-> Dataset.toExample
          >-> Dataset.filterByMaxTokens tokenize (runIdentity genTmConfigMaxInputTokens) Dataset.prettyTerms
          >-> Dataset.filterByMaxTokens tokenize (runIdentity genTmConfigMaxOutputTokens) Dataset.prettyReducedTerms
          >-> Dataset.deduplicate HashSet.empty (^. Dataset.prettyTerm)
          >-> P.take (runIdentity genTmConfigNumberOfExampes)
          >-> P.tee (showProgress $ runIdentity genTmConfigNumberOfExampes)
          >-> P.tee (Dataset.writeJsonLines (runIdentity genTmConfigOutputFolder </> runIdentity genTmConfigOutputDataFileName))
          >-> Dataset.histogram

genCompAndExport :: Opts.GenCompConfig Identity -> IO ()
genCompAndExport config@Opts.GenCompConfig {..} =
  P.runSafeT $ do
    saveAsJson (runIdentity genCompConfigOutputFolder </> runIdentity genCompConfigOutputConfigFileName) (Opts.Config . Opts.Comp $ config)
    case runIdentity genCompConfigLanguage of
      Opts.STLC2 -> go @'Opts.STLC2
      Opts.STLC3 -> go @'Opts.STLC3
      Opts.STLC3Eager -> go @'Opts.STLC3Eager
      Opts.STLC3Lazy -> go @'Opts.STLC3Lazy
  where
    go :: forall l. Dataset.HasExamples l => P.SafeT IO ()
    go = do
      examples :: HashMap Text (Dataset.Example l) <-
        P.runEffect . P.execStateP mempty $
          Dataset.readJsonLines (runIdentity genCompConfigInputFolder </> runIdentity genCompConfigInputDataFileName)
            >-> Dataset.cache (^. Dataset.prettyTerm)
            >-> P.drain
      liftIO . putStrLn $ "Read " <> show (length examples) <> " examples."
      keys <- for (runIdentity genCompConfigInputTrainingDataCSVFile) $ \fileName ->
        P.runEffect . P.execStateP mempty $
          Dataset.readCsv fileName
            >-> Dataset.cache (^. Dataset.teTermPretty)
            >-> P.drain
      liftIO . putStrLn $ case keys of
        Just ks -> "Read " <> show (length ks) <> " keys."
        Nothing -> "Skipped reading keys."
      P.runEffect $
        Dataset.compositions examples (HashMap.keysSet <$> keys)
          >-> Dataset.deduplicate HashSet.empty (^. _1)
          >-> Dataset.toExample
          >-> Dataset.deduplicate (HashMap.keysSet examples) (^. Dataset.prettyTerm)
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
