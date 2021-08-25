{-# LANGUAGE BangPatterns #-}

{- Gen.hs
   ======
   Term generator for simply typed lambda calculus. With STLC2CL we can
   also use the generator for combinatory logic. -}

module Language.STLC.Gen2 where

import Language.STLC
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Logic


-- | Generates all closed terms in STLC
gen :: LogicS Term
gen = do ty <- genTy
         tm <- genTm [] ty
         return tm

-- | Helper function to easily run monad stack.
--
-- NOTE: Returns an infinite list. Use 'take' to force finite amount
--       of computation.
observeLogicS :: LogicS a -> Int -> [a]
observeLogicS l n = flip evalState ids . observeManyT n $ l


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
type LogicS a = LogicT (State [Id]) a

-- | Generate terms
--
-- >>> flip evalState ids . runSearchBestT $ genTm [] TyUnit
-- Just (Sum {getSum = 1},TmUnit)
genTm :: Context -> Type -> LogicS Term
genTm ctx ty = genTmUnit ctx ty
           <|> genTmBool ctx ty
           <|> genTmVar  ctx ty
           <|> genTmProd ctx ty
           <|> genTmFun  ctx ty
           <|> genTmIf   ctx ty
           <|> genTmFst  ctx ty
           <|> genTmSnd  ctx ty
           <|> genTmApp  ctx ty

-- | Generate unit terms
genTmUnit :: Context -> Type -> LogicS Term
genTmUnit _ (TyUnit) = return TmUnit
genTmUnit _ _        = empty

-- | Generate boolean terms
genTmBool :: Context -> Type -> LogicS Term
genTmBool _ (TyBool) = return TmTrue <|> return TmFalse
genTmBool _ _        = empty

-- | Generate variable terms
genTmVar :: Context -> Type -> LogicS Term
genTmVar []           ty' = empty
genTmVar ((x,ty):ctx) ty' | ty == ty' = return (TmVar x)
                          | otherwise = genTmVar ctx ty'

-- | Generate product terms
genTmProd :: Context -> Type -> LogicS Term
genTmProd ctx (TyProd ty1 ty2) = TmProd
                                 <$> genTm ctx ty1
                                 <*> genTm ctx ty2
genTmProd _ _                  = empty


-- | Generate function terms
genTmFun :: Context -> Type -> LogicS Term
genTmFun ctx (TyFun ty1 ty2) = do x <- lift $ gets Prelude.head
                                  lift $ modify Prelude.tail
                                  TmFun x ty1
                                    <$> genTm ((x,ty1):ctx) ty2
genTmFun _ _                 = empty

-- | Helper function for generating functions.
--
-- NOTE: Size of terms includes size of type annotations when using 'genFun'
sizeTy :: Type -> Int
sizeTy (TyUnit)         = 1
sizeTy (TyBool)         = 1
sizeTy (TyProd ty1 ty2) = 1 + (sizeTy ty1) + (sizeTy ty2)
sizeTy (TyFun ty1 ty2)  = 1 + (sizeTy ty1) + (sizeTy ty2)

-- | Generate if-then-else terms
genTmIf :: Context -> Type -> LogicS Term
genTmIf ctx ty = TmIf
                 <$> genTm ctx TyBool
                 <*> genTm ctx ty
                 <*> genTm ctx ty

-- | Generate @TmFst@ terms
genTmFst :: Context -> Type -> LogicS Term
genTmFst ctx ty = do ty2 <- genTy
                     TmFst
                       <$> genTm ctx (TyProd ty ty2)

-- | Generate @TmSnd@ terms
genTmSnd :: Context -> Type -> LogicS Term
genTmSnd ctx ty = do ty1 <- genTy
                     TmSnd
                       <$> genTm ctx (TyProd ty1 ty)

-- | Generate @TmApp@ terms
genTmApp :: Context -> Type -> LogicS Term
genTmApp ctx ty = do ty1 <- genTy
                     TmApp
                       <$> genTm ctx (TyFun ty1 ty)
                       <*> genTm ctx ty1


{- ============================ Type Generator ============================== -}

-- | Generate types
--
-- >>> flip evalState ids . runSearchBestT $ genTy
-- Just (Sum {getSum = 1},TyUnit)
genTy :: LogicS Type
genTy = genTyUnit
    <|> genTyBool
    <|> genTyProd
    <|> genTyFun

-- | Generate unit type
genTyUnit :: LogicS Type
genTyUnit = return TyUnit

-- | Generate bool type
genTyBool :: LogicS Type
genTyBool = return TyBool

-- | Generate product type
genTyProd :: LogicS Type
genTyProd = TyProd
            <$> genTy
            <*> genTy

-- | Generate function type
genTyFun :: LogicS Type
genTyFun = TyFun
           <$> genTy
           <*> genTy


-- data Nat = Z | S Nat deriving (Eq, Show)

-- type Gen a = StateT Int [] a

-- z :: Gen Nat
-- z = withFuel $ lift $ return Z

-- suc :: Gen Nat
-- suc = withFuel $ S <$> nats

-- nats = z <|> suc

-- withFuel :: Gen a -> Gen a
-- withFuel g = do fuel <- get
--                 if fuel <= 0
--                   then Control.Applicative.empty
--                   else (modify (+ (-1))) >> g

