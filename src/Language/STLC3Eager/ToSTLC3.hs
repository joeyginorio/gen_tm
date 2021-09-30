{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.STLC3Eager.ToSTLC3 where

import Bound (instantiate1)
import Bound.Term (closed)
import Control.Monad.Fresh (Fresh, MonadFresh (fresh), runFreshFrom)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Language.Eagerness (Eagerness (Lazy))
import Language.STLC3 (Id, Term (..), Type (..))
import qualified Language.STLC3Eager as STLC3Eager

instance Enum Id where
  toEnum = Text.pack . show
  fromEnum = read . Text.unpack

-- | Convert an STLC3Eager term to an STLC3 term.
toSTLC3Term :: forall a. STLC3Eager.Exp 'Lazy a -> Term
toSTLC3Term = runFreshFrom "0" . go . fromJust . closed
  where
    go :: STLC3Eager.Exp 'Lazy Id -> Fresh Id Term
    go (STLC3Eager.Var a') = pure $ TmVar a'
    go (STLC3Eager.Lam ty b) = do
      a' <- ("x" <>) <$> fresh
      tm <- go (instantiate1 (STLC3Eager.Var a') b)
      let ty' = toSTLC3Type ty
      pure $ TmFun a' ty' tm
    go (f STLC3Eager.:@ a) = TmApp <$> go f <*> go a
    go STLC3Eager.Unit = pure TmUnit
    go STLC3Eager.True = pure TmTrue
    go STLC3Eager.False = pure TmFalse
    go (STLC3Eager.If c t e) = TmIf <$> go c <*> go t <*> go e
    go (STLC3Eager.Nil ty) = pure $ TmNil (toSTLC3Type ty)
    go (STLC3Eager.Cons ty h t) =
      TmCons (toSTLC3Type ty) <$> go h <*> go t
    go (STLC3Eager.Foldr s i l) = TmFold <$> go s <*> go i <*> go l

-- | Convert an STLC3Eager type to an STLC3 type.
toSTLC3Type :: STLC3Eager.Ty 'Lazy -> Type
toSTLC3Type STLC3Eager.TUnit = TyUnit
toSTLC3Type STLC3Eager.TBool = TyBool
toSTLC3Type (STLC3Eager.TArr ty ty') = TyFun (toSTLC3Type ty) (toSTLC3Type ty')
toSTLC3Type (STLC3Eager.TList ty) = TyList (toSTLC3Type ty)
