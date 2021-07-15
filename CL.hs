{- CL.hs
   =====
   Defines syntax and semantics of SK-combinatory logic as both strings
   and graphs. -}

module CL where

{- ================================= Syntax ================================= -}

-- Combinatory logic (CL) terms
data Term  = S | K | App Term Term deriving Show


{- ================================ Semantics =============================== -}

-- Reduce (execute) a CL term
reduce :: Term -> Term
reduce (App (App K x) y)         = x
reduce (App (App (App S x) y) z) = (App (App x z) (App y z))
reduce sk                        = sk

