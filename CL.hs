{- CL.hs
   =====
   Defines syntax and semantics of SK-combinatory logic as both strings
   and graphs. -}

module CL where

{- ================================= Syntax ================================= -}

-- Combinatory logic (CL) terms
data CL  = S | K | App CL CL deriving Show


{- ================================ Semantics =============================== -}

-- Reduce (execute) a CL term
reduce :: CL -> CL
reduce (App (App K x) y)         = x
reduce (App (App (App S x) y) z) = (App (App x z) (App y z))
reduce sk                        = sk


