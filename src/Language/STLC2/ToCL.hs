{-# LANGUAGE OverloadedStrings #-}

{- STLC2CL.hs
   ==========
   Translate from simply-typed lambda calculus (STLC) to combinatory logic (CL).
   Two stages:
     (i) Apply desugaring and type erasure in translation from STLC to the
         untyped lambda calculus.
     (ii) Standard translation from untyped lambda calculus to CL. -}

module Language.STLC2.ToCL where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Language.CL as CL
import qualified Language.LC as LC
import qualified Language.STLC2 as STLC2
import qualified Language.STLC2.ToLC as STLC2

{- ====================== Stage 1: Erasure + Desugar ======================== -}

{- ===================== Stage 2: Lambda elimination ======================== -}

data Term'
  = S'
  | K'
  | TmVar' LC.Id
  | TmFun' LC.Id Term'
  | TmApp' Term' Term'
  deriving (Show)

-- | Translate lambda terms into a mixed representation of lambda / CL terms.
-- Makes it simpler to implement the term rewriting algorithm for translating
-- lambda to CL terms.
toTerm' :: LC.Term -> Term'
toTerm' (LC.TmVar x) = TmVar' x
toTerm' (LC.TmFun x t) = TmFun' x (toTerm' t)
toTerm' (LC.TmApp s t) = TmApp' (toTerm' s) (toTerm' t)

-- | Free variables in a term
fvs :: Term' -> Set LC.Id
fvs S' = Set.empty
fvs K' = Set.empty
fvs (TmVar' x) = Set.insert x Set.empty
fvs (TmFun' x tm) = Set.delete x $ fvs tm
fvs (TmApp' tm1 tm2) = fvs tm1 `Set.union` fvs tm2

-- | Convert lambda' terms to cl' terms.
--
-- NOTE: @Term'@ is a representation of BOTH lambda and cl terms because the
--       translation algorithm introduces pseudo-lambda and pseudo-cl terms
--       at intermediate stages. e.g. @TmFun "x" K@ is neither a lambda or cl term.
toCL' :: Term' -> Term'
toCL' S' = S'
toCL' K' = K'
toCL' (TmVar' x) = TmVar' x
toCL' (TmApp' s t) = TmApp' (toCL' s) (toCL' t)
toCL' (TmFun' x (TmVar' y)) | x == y = TmApp' (TmApp' S' K') K'
toCL' (TmFun' x t)
  | not $ Set.member x (fvs t) = TmApp' K' (toCL' t)
  | otherwise = case t of
    t'@(TmFun' _ _) -> toCL' (TmFun' x (toCL' t'))
    (TmApp' s' t') -> TmApp' (TmApp' S' s'') t''
      where
        s'' = toCL' $ TmFun' x s'
        t'' = toCL' $ TmFun' x t'
    _ -> error "toCL': not implemented"

-- | Transforms pseudo CL terms to legit CL terms (maybe!).
-- If for some reason the translation algorithm fails to convert things
-- to CL, then the Maybe monad will catch it (by returning Nothing).
toCL :: Term' -> Maybe CL.Term
toCL S' = return CL.S
toCL K' = return CL.K
toCL (TmApp' s t) = CL.App <$> toCL s <*> toCL t
toCL _ = Nothing

-- | Compile STLC to CL in 4 stages:
--
--   (1) @toLC@, takes STLC terms to lambda calculus
--   (2) @toTerm'@, takes lambda terms to pseudo-lambda terms
--   (3) @toCL'@, takes pseudo lambda terms to pseudo cl terms
--   (4) @toCL@, takes pseudo cl terms to cl terms (maybe!)
--
-- >>> compile $ STLC2.TmUnit
-- Just (App (App S K) K)
--
-- >>> compile $ STLC2.TmFun "x" STLC2.TyBool STLC2.TmUnit
-- Just (App K (App (App S K) K))
--
-- >>> compile $ STLC2.TmApp (STLC2.TmFun "x" STLC2.TyBool (STLC2.TmVar "x")) STLC2.TmTrue
-- Just (App (App (App S K) K) (App (App S (App K K)) (App (App S K) K)))
compile :: STLC2.Term -> Maybe CL.Term
compile = toCL . toCL' . toTerm' . STLC2.toLC
