{- Gen.hs
   ======
   Term generator for simply typed lambda calculus. With STLC2CL we can
   also use the generator for combinatory logic. -}

module Language.STLC.Gen where

import Language.STLC
import Control.Applicative
import Control.Monad.Search
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Monoid (Sum(..))


{- ============================ Term Generator ============================== -}

-- Search + State monad stack.
-- Search monad handles weighted nondeterminism
-- State monad handles reading/writing of fresh variable names
-- NOTE: Without search monad, a naive approach to generation would never
--       terminate. For a simple program like t :: Unit, the lhs of an
--       application will get stuck in a depth-first traversal of an
--       infinitely large term. So what we want is to explore terms by
--       increasing size. The search monad lets us add costs to the
--       generative process. We provide the cost annotations and it handles
--       the enumeration in order of increasing cost.
type SearchS c = SearchT c (State [Id])


-- >>> take 3 . flip evalState ids . runSearchT $ genTm [] TyUnit
-- [(Sum {getSum = 1},TmUnit),(Sum {getSum = 5},TmSnd (TmProd TmUnit TmUnit)), ...]
--
-- >>> take 3 . flip evalState ids . runSearchT $ genTm [("x", TyBool)] (TyProd TyBool TyUnit)
-- [(Sum {getSum = 3},TmProd (TmVar "x") TmUnit),(Sum {getSum = 3}, ...]
genTm :: Context -> Type -> SearchS (Sum Integer) Term
genTm ctx ty = genTmUnit ctx ty
             <|> genTmBool ctx ty
             <|> genTmVar  ctx ty
             <|> genTmProd ctx ty
             <|> genTmFun  ctx ty
             <|> genTmFst  ctx ty
             <|> genTmSnd  ctx ty
             <|> genTmApp  ctx ty

-- Generate unit
genTmUnit :: Context -> Type -> SearchS (Sum Integer) Term
genTmUnit _ (TyUnit) = do cost' (Sum 1)
                          return TmUnit
genTmUnit _ _        = abandon

-- Generate booleans
genTmBool :: Context -> Type -> SearchS (Sum Integer) Term
genTmBool _ (TyBool) = do cost' (Sum 1)
                          return TmTrue
                 <|> do cost' (Sum 1)
                        return TmFalse
genTmBool _ _        = abandon

-- Generate variables
genTmVar :: Context -> Type -> SearchS (Sum Integer) Term
genTmVar []           ty' = abandon
genTmVar ((x,ty):ctx) ty' | ty == ty' = do cost' (Sum 1)
                                           return (TmVar x)
                          | otherwise = genTmVar ctx ty'

genTmProd :: Context -> Type -> SearchS (Sum Integer) Term
genTmProd ctx (TyProd ty1 ty2) = do cost' (Sum 1)
                                    TmProd
                                      <$> genTm ctx ty1
                                      <*> genTm ctx ty2
genTmProd _ _                  = abandon


-- Generate functions
genTmFun :: Context -> Type -> SearchS (Sum Integer) Term
genTmFun ctx (TyFun ty1 ty2) = do cost' (Sum 1)
                                  cost' (Sum (sizeTy ty1))
                                  x <- lift $ gets head
                                  lift $ modify tail
                                  TmFun x ty1
                                    <$> genTm ((x,ty1):ctx) ty2
genTmFun _ _                 = abandon

-- Helper function for generating functions
-- NOTE: Size of terms includes size of type annotations when using genFun
sizeTy :: Type -> Integer
sizeTy (TyUnit)         = 1
sizeTy (TyBool)         = 1
sizeTy (TyProd ty1 ty2) = 1 + (sizeTy ty1) + (sizeTy ty2)
sizeTy (TyFun ty1 ty2)  = 1 + (sizeTy ty1) + (sizeTy ty2)


genTmIf :: Context -> Type -> SearchS (Sum Integer) Term
genTmIf ctx ty = do cost' (Sum 1)
                    TmIf
                      <$> genTm ctx TyBool
                      <*> genTm ctx ty
                      <*> genTm ctx ty

genTmFst :: Context -> Type -> SearchS (Sum Integer) Term
genTmFst ctx ty = do cost' (Sum 1)
                     ty2 <- genTy
                     TmFst
                       <$> genTm ctx (TyProd ty ty2)

genTmSnd :: Context -> Type -> SearchS (Sum Integer) Term
genTmSnd ctx ty = do cost' (Sum 1)
                     ty1 <- genTy
                     TmSnd
                       <$> genTm ctx (TyProd ty1 ty)

genTmApp :: Context -> Type -> SearchS (Sum Integer) Term
genTmApp ctx ty = do cost' (Sum 1)
                     ty1 <- genTy
                     TmApp
                       <$> genTm ctx (TyFun ty1 ty)
                       <*> genTm ctx ty1


{- ============================ Type Generator ============================== -}

-- Generate types
--
-- >>> take 3 . flip evalState ids . runSearchT $ genTy
-- [(Sum {getSum = 1},TyUnit),(Sum {getSum = 1},TyBool),(Sum {getSum = 3},TyProd TyUnit TyUnit)]
genTy :: SearchS (Sum Integer) Type
genTy = genTyUnit
    <|> genTyBool
    <|> genTyProd

-- Generate unit
genTyUnit :: SearchS (Sum Integer) Type
genTyUnit = do cost' (Sum 1)
               return TyUnit

-- Generate booleans
genTyBool :: SearchS (Sum Integer) Type
genTyBool = do cost' (Sum 1)
               return TyBool

-- Generate product
genTyProd :: SearchS (Sum Integer) Type
genTyProd = do cost' (Sum 1)
               TyProd
                 <$> genTy
                 <*> genTy

-- Generate product
genTyFun :: SearchS (Sum Integer) Type
genTyFun = do cost' (Sum 1)
              TyFun
                <$> genTy
                <*> genTy
