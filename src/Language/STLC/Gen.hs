{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.MemoTrie
import Data.Function (fix)
-- import GHC.Generics (Generic)


-- | Costs are integers
type Cost = Integer

-- | Generates all closed terms in STLC
gen :: SearchS (Sum Cost) (Type, Term)
gen = do ty <- genTy
         tm <- (memoFix2 genTm) [] ty
         return (ty, tm)

-- | Generates all closed terms in STLC
gen' :: SearchS (Sum Cost) (Type, Term)
gen' = do ty <- genTy
          tm <- (fix genTm) [] ty
          return (ty, tm)


-- | Helper function to easily run monad stack.
--
-- NOTE: Returns an infinite list. Use 'take' to force finite amount
--       of computation.
evalSearchS :: SearchS (Sum Cost) a -> [(Sum Cost, a)]
evalSearchS = flip evalState ids . runSearchT


{- ============================ Term Generator ============================== -}

-- | 'Search' + 'State' monad stack.
--
-- 'Search' monad handles weighted nondeterminism.
-- 'State' monad handles reading/writing of fresh variable names.
--
-- NOTE: Without 'Search' monad, a naive approach to generation would never
--       terminate. For a simple program like @t :: Unit@, the lhs of an
--       application will get stuck in a depth-first traversal of an
--       infinitely large term. So what we want is to explore terms by
--       increasing size. The 'Search' monad lets us add costs to the
--       generative process. We provide the cost annotations and it handles
--       the enumeration in order of increasing cost.
type SearchS c = SearchT c (State [Id])

-- | Generate terms
--
-- >>> flip evalState ids . runSearchBestT $ genTm [] TyUnit
-- Just (Sum {getSum = 1},TmUnit)
genTm :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTm f ctx ty = genTmUnit ctx ty
             <|> genTmBool ctx ty
             <|> genTmVar  ctx ty
             <|> genTmProd f ctx ty
             <|> genTmIf   f ctx ty
             <|> genTmFun  f ctx ty
             <|> genTmFst  f ctx ty
             <|> genTmSnd  f ctx ty
             <|> genTmApp  f ctx ty

-- | Generate unit terms
genTmUnit :: Context -> Type -> SearchS (Sum Cost) Term
genTmUnit _ (TyUnit) = do cost' (Sum 1)
                          return TmUnit
genTmUnit _ _        = abandon

-- | Generate boolean terms
genTmBool :: Context -> Type -> SearchS (Sum Cost) Term
genTmBool _ (TyBool) = do cost' (Sum 1)
                          return TmTrue
                   <|> do cost' (Sum 1)
                          return TmFalse
genTmBool _ _        = abandon

-- | Generate variable terms
genTmVar :: Context -> Type -> SearchS (Sum Cost) Term
genTmVar []           ty' = abandon
genTmVar ((x,ty):ctx) ty' | ty == ty' = do cost' (Sum 1)
                                           return (TmVar x)
                          | otherwise = genTmVar ctx ty'

-- | Generate product terms
genTmProd :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmProd f ctx (TyProd ty1 ty2) = do cost' (Sum 1)
                                      TmProd
                                        <$> f ctx ty1
                                        <*> f ctx ty2
genTmProd _ _ _                  = abandon


-- | Generate function terms
genTmFun :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmFun f ctx (TyFun ty1 ty2) = do cost' (Sum 1)
                                    cost' (Sum (sizeTy ty1))
                                    x <- lift $ gets head
                                    lift $ modify tail
                                    TmFun x ty1
                                      <$> f ((x,ty1):ctx) ty2
genTmFun _ _ _                 = abandon

-- | Helper function for generating functions.
--
-- NOTE: Size of terms includes size of type annotations when using 'genFun'
sizeTy :: Type -> Cost
sizeTy (TyUnit)         = 1
sizeTy (TyBool)         = 1
sizeTy (TyProd ty1 ty2) = 1 + (sizeTy ty1) + (sizeTy ty2)
sizeTy (TyFun ty1 ty2)  = 1 + (sizeTy ty1) + (sizeTy ty2)

-- | Generate if-then-else terms
genTmIf :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmIf f ctx ty = do cost' (Sum 1)
                      TmIf
                        <$> f ctx TyBool
                        <*> f ctx ty
                        <*> f ctx ty

-- | Generate @TmFst@ terms
genTmFst :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmFst f ctx ty = do cost' (Sum 1)
                       ty2 <- genTy
                       TmFst
                         <$> f ctx (TyProd ty ty2)
-- | Generate @TmSnd@ terms
genTmSnd :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmSnd f ctx ty = do cost' (Sum 1)
                       ty1 <- genTy
                       TmSnd
                         <$> f ctx (TyProd ty1 ty)

-- | Generate @TmApp@ terms
genTmApp :: (Context -> Type -> SearchS (Sum Cost) Term) -> Context -> Type -> SearchS (Sum Cost) Term
genTmApp f ctx ty = do cost' (Sum 1)
                       ty1 <- genTy
                       TmApp
                         <$> f ctx (TyFun ty1 ty)
                         <*> f ctx ty1


{- ============================ Type Generator ============================== -}

-- | Generate types
--
-- >>> flip evalState ids . runSearchBestT $ genTy
-- Just (Sum {getSum = 1},TyUnit)
genTy :: SearchS (Sum Cost) Type
genTy = genTyUnit
    <|> genTyBool
    <|> genTyProd
    <|> genTyFun

-- | Generate unit type
genTyUnit :: SearchS (Sum Cost) Type
genTyUnit = do cost' (Sum 1)
               return TyUnit

-- | Generate bool type
genTyBool :: SearchS (Sum Cost) Type
genTyBool = do cost' (Sum 1)
               return TyBool

-- | Generate product type
genTyProd :: SearchS (Sum Cost) Type
genTyProd = do cost' (Sum 1)
               TyProd
                 <$> genTy
                 <*> genTy

-- | Generate function type
genTyFun :: SearchS (Sum Cost) Type
genTyFun = do cost' (Sum 1)
              TyFun
                <$> genTy
                <*> genTy

memoFix2 :: (HasTrie a, HasTrie b) => ((a -> b -> c) -> (a -> b -> c)) -> (a -> b -> c)
memoFix2 h = fix $ mup memo . h

data Nat = Z | S Nat deriving (Eq, Show)


type SearchN c = SearchT c (State Int)

z :: Int -> Search (Sum Cost) Nat
z 0 = abandon
z n = (cost' $ Sum 1) >> pure Z

suc :: Int -> Search (Sum Cost) Nat
suc 0 = abandon
suc n = (cost' $ Sum 1) >> S <$> nat (n-1)

nat :: Int -> Search (Sum Cost) Nat
nat n = z n <|> suc n

evalSearchN :: Int -> SearchN (Sum Cost) a -> [(Sum Cost, a)]
evalSearchN n = flip evalState n . runSearchT

nat' :: Search (Sum Cost) Nat
nat' = (cost' $ Sum 1) >> (pure Z <|> S <$> nat')
