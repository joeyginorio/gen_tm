{- CL.hs
   =====
   Defines syntax and semantics of SK-combinatory logic as both strings
   and graphs. -}

module Language.CL where

{- ================================= Syntax ================================= -}

-- | Combinatory logic (CL) terms
data Term  = S | K | App Term Term
  deriving (Show, Eq)


{- ================================ Semantics =============================== -}

-- | Reduce (execute) a CL term
reduce :: Term -> Term
reduce (App (App K x) y)         = reduce x
reduce (App (App (App S x) y) z) = reduce (App xz yz)
                                   where xz = reduce (App x z)
                                         yz = reduce (App y z)
reduce sk                        = sk
