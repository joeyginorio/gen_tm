{-# LANGUAGE TemplateHaskell #-}

{- CL.hs
   =====
   Defines syntax and semantics of SK-combinatory logic as both strings
   and graphs. -}

module Language.CL where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Control.Monad.Trans.Writer.Lazy
import Data.Monoid

{- ================================= Syntax ================================= -}

-- | Combinatory logic (CL) terms
data Term  = S | K | App Term Term
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''Term)


{- ================================ Semantics =============================== -}

-- | Writer term, writer monad tracks reduction steps
type WTerm = Writer (Sum Integer) Term

-- | Reduce (execute) a CL term
reduce :: Term -> WTerm
reduce (App (App K x) y)         = tell (Sum 1) >> reduce x
reduce (App (App (App S x) y) z) = do tell $ Sum 1
                                      xz <- reduce $ App x z
                                      yz <- reduce $ App y z
                                      reduce (App xz yz)
reduce (App x y)                 = do tell $ Sum 1
                                      x' <- reduce x
                                      y' <- reduce y
                                      if x /= x' || y /= y'
                                        then reduce $ App x' y'
                                        else return $ App x' y'
reduce tm                        = return tm

-- | Reduce, retaining writer output
reduceW :: Term -> (Term, Sum Integer)
reduceW = runWriter . reduce

-- | Reduce, ignoring writer output
reduce' :: Term -> Term
reduce' = fst . reduceW
