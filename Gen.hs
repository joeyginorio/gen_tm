{- Gen.hs
   ======
   Term generator for simply typed lambda calculus. With STLC2CL we can
   also use the generator for combinatory logic. -}

import STLC

import Control.Applicative
import Control.Monad.Search
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Monoid (Sum(..))


{- ============================ Term Generator ============================== -}

-- Search + State monad stack.
-- Search monad handles weighted nondeterminism
-- State monad handles reading/writing of fresh variable names
type SearchS c = SearchT c (State [Id])

genTerm :: Context -> Type -> SearchS (Sum Integer) Term
genTerm ctx ty = genUnit ctx ty
             <|> genBool ctx ty
             <|> genVar ctx ty
             <|> genProd ctx ty

-- Generate unit
genUnit :: Context -> Type -> SearchS (Sum Integer) Term
genUnit _ (TyUnit) = do cost' (Sum 1)
                        return TmUnit
genUnit _ _        = abandon

-- Generate booleans
genBool :: Context -> Type -> SearchS (Sum Integer) Term
genBool _ (TyBool) = do cost' (Sum 1)
                        return TmTrue
                 <|> do cost' (Sum 1)
                        return TmFalse
genBool _ _        = abandon

-- Generate variables
genVar :: Context -> Type -> SearchS (Sum Integer) Term
genVar []           ty' = abandon
genVar ((x,ty):ctx) ty' | ty == ty' = do cost' (Sum 1)
                                         return (TmVar x)
                        | otherwise = genVar ctx ty'

genProd :: Context -> Type -> SearchS (Sum Integer) Term
genProd ctx (TyProd ty1 ty2) = do cost' (Sum 1)
                                  TmProd
                                    <$> genTerm ctx ty1
                                    <*> genTerm ctx ty2
genProd _ _                  = abandon


-- Generate functions
genFun :: Context -> Type -> SearchS (Sum Integer) Term
genFun ctx (TyFun ty1 ty2) = do cost' (Sum 1)
                                cost' (Sum (sizeTy ty1))
                                x <- lift $ gets head
                                lift $ modify tail
                                TmFun x ty1
                                  <$> genTerm ((x,ty1):ctx) ty2
genFun _ _                 = abandon


-- Helper function for generating functions
-- NOTE: Size of terms includes size of type annotations when using genFun
sizeTy :: Type -> Integer
sizeTy (TyUnit)         = 1
sizeTy (TyBool)         = 1
sizeTy (TyProd ty1 ty2) = 1 + (sizeTy ty1) + (sizeTy ty2)
sizeTy (TyFun ty1 ty2)  = 1 + (sizeTy ty1) + (sizeTy ty2)

