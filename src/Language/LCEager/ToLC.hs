{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LCEager.ToLC where

import Bound (instantiate1)
import Control.Monad.Fresh (Fresh, MonadFresh (fresh), runFreshFrom)
import qualified Data.Text as Text
import Language.Eagerness (Eagerness (Lazy))
import Language.LC (Id, Term (..))
import qualified Language.LCEager as LCEager

instance Enum Id where
  toEnum = Text.pack . show
  fromEnum = read . Text.unpack

-- | Convert an LCEager term to an LC term.
toLCTerm :: LCEager.Exp 'Lazy Id -> Term
toLCTerm e = runFreshFrom "0" $ go e
  where
    go :: LCEager.Exp 'Lazy Id -> Fresh Id Term
    go (LCEager.Var a') = pure $ TmVar a'
    go (LCEager.Lam b) = do
      a' <- ("x" <>) <$> fresh
      tm <- go (instantiate1 (LCEager.Var a') b)
      pure $ TmFun a' tm
    go (f LCEager.:@ a) = TmApp <$> go f <*> go a
