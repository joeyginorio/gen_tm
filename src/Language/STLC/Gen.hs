{- Gen.hs
   ======
   Term generator for simply typed lambda calculus. With STLC2CL we can
   also use the generator for combinatory logic. Implementation has
   two stages.
     (i) Type generation
     (ii) Type-directed term generation -}

module Language.STLC.Gen where

import Language.STLC

import Control.Applicative
import Control.Monad.Search
import Control.Monad.Trans
import Data.Monoid (Sum(..))


{- ============================ Term Generator ============================== -}

-- Search monad lets us perform a weighted breadth first search
genTerm :: Context -> Type -> Search (Sum Integer) Term
genTerm ctx ty = genUnit ctx ty
             <|> genBool ctx ty
             <|> genVar ctx ty
             <|> genProd ctx ty

-- Generate unit
genUnit :: Context -> Type -> Search (Sum Integer) Term
genUnit _ (TyUnit) = do cost' (Sum 1)
                        return TmUnit
genUnit _ _        = abandon

-- Generate booleans
genBool :: Context -> Type -> Search (Sum Integer) Term
genBool _ (TyBool) = do cost' (Sum 1)
                        return TmTrue
                 <|> do cost' (Sum 1)
                        return TmFalse
genBool _ _        = abandon

-- Generate variables
genVar :: Context -> Type -> Search (Sum Integer) Term
genVar []           ty' = abandon
genVar ((x,ty):ctx) ty' | ty == ty' = do cost' (Sum 1)
                                         return (TmVar x)
                        | otherwise = genVar ctx ty'

genProd :: Context -> Type -> Search (Sum Integer) Term
genProd ctx (TyProd ty1 ty2) = do cost' (Sum 1)
                                  TmProd
                                    <$> genTerm ctx ty1
                                    <*> genTerm ctx ty2
genProd _ _                  = abandon
