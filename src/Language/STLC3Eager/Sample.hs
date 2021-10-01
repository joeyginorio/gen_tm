{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.STLC3Eager.Sample where

import Bound (instantiate1)
import Control.Applicative (Alternative (empty, (<|>)), (<|>))
import Control.Monad.Fresh (FreshT (..), MonadFresh (fresh), runFreshT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), MonadTrans (..), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT)
import Control.Monad.Trans.Writer.Lazy (runWriter)
import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Hedgehog (Gen, GenT, MonadGen (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import Language.Eagerness (Eagerness (..))
import Language.STLC3Eager (Exp (..), Ty (..), lam, nf', pprintTerm, whnf)
import Prelude hiding (False, True)

-- | Monad transformer stack for term and type generation.
-- Notably, contains the @FreshT@ transformer for generating fresh variable names
-- and a @ReaderT@ for the environment of scoped typed @Var@s.
type GTyM k a = ReaderT (Map (Ty k) [Exp k a]) (FreshT a Gen)

-- | Generate a type.
-- We cannot generate an expression without generating a type for it first.
genTy :: forall k m. MonadGen m => m (Ty k)
genTy =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      pure TUnit,
      pure TBool
    ]
    [ -- recursive generators
      TList <$> genTy,
      TArr <$> genTy <*> genTy
    ]

-- | Finalize generation by running the monad transformers for the environment
-- and the fresh variable name computation.
genWellTypedExp :: forall k a. (Eq a, Enum a) => Ty k -> Gen (Exp k a)
genWellTypedExp ty' = runFreshT $ runReaderT (genWellTypedExp' ty') mempty

-- | Main recursive mechanism for genersating expressions for a given type.
genWellTypedExp' :: forall k a. Eq a => Ty k -> GTyM k a (Exp k a)
genWellTypedExp' ty' =
  Gen.shrink shrinkExp $
    genWellTypedPath ty'
      <|> Gen.recursive
        Gen.choice
        [ -- non-recursive generators
          genWellTypedExp'' ty'
        ]
        [ -- recursive generators
          genWellTypedApp ty',
          genWellTypedExp' ty',
          genWellTypedExp''' ty'
        ]

shrinkExp :: forall k a. Exp k a -> [Exp k a]
shrinkExp (f :@ a) = fst . runWriter $ do
  r <- whnf f
  case r of
    Lam _ b -> pure <$> whnf (instantiate1 a b)
    _ -> pure []
shrinkExp (Cons _ _ t) = [t]
shrinkExp _ = []

-- | Pattern match on a given type and produce a corresponding term.
-- @Lam@ is generated from @Arr@ by first obtaining a fresh variable name for @Var@ and
-- then calling the @lam@ smart constructor on an expression that
-- was produced for an environment to which @Var@ was added.
-- A term of type @Nat@ is generated by converting a random integer through induction.
genWellTypedExp'' :: forall k a. Eq a => Ty k -> GTyM k a (Exp k a)
genWellTypedExp'' (TArr ty' ty'') = do
  uname <- fresh
  lam ty' uname <$> local (insertVar uname ty') (genWellTypedExp' ty'')
genWellTypedExp'' TUnit = pure Unit
genWellTypedExp'' TBool = Gen.bool <&> bool True False
genWellTypedExp'' (TList ty') =
  Gen.shrink shrinkExp $
    Gen.recursive
      Gen.choice
      [ pure (Nil ty')
      ]
      [ Cons ty' <$> genWellTypedExp' ty' <*> genWellTypedExp' (TList ty')
      ]

-- | Add @Var@ of given type to the given env so that it can be used for sampling later.
insertVar :: forall k a. Eq a => a -> Ty k -> Map (Ty k) [Exp k a] -> Map (Ty k) [Exp k a]
insertVar uname ty' =
  Map.insertWith (<>) ty' [Var uname] . fmap (List.filter (/= Var uname))

genWellTypedExp''' :: forall k a. Eq a => Ty k -> GTyM k a (Exp k a)
genWellTypedExp''' ty' =
  let genIf = do
        c <- genWellTypedExp' TBool
        t <- genWellTypedExp' ty'
        e <- genWellTypedExp' ty'
        pure (If c t e)
      genFoldr =
        -- (ty'' -> ty' -> ty') -> ty' -> [ty''] -> ty'
        do
          ty'' <- genTy
          s <- genWellTypedExp' (TArr ty'' (TArr ty' ty'))
          i <- genWellTypedExp' ty'
          l <- genWellTypedExp' (TList ty'')
          pure (Foldr s i l)
   in Gen.choice [genIf, genFoldr]

-- | Generate app by first producing type and value of the argument
-- and then generating a compatible @Lam@.
genWellTypedApp :: forall k a. Eq a => Ty k -> GTyM k a (Exp k a)
genWellTypedApp ty' = do
  tg <- genKnownTypeMaybe
  eg <- genWellTypedExp' tg
  let tf = TArr tg ty'
  ef <- genWellTypedExp' tf
  pure (ef :@ eg)

-- | Try to look up a known expression of the desired type from the environment.
-- This does not always succceed, throwing `empty` when unavailable.
genWellTypedPath :: forall k a. Ty k -> GTyM k a (Exp k a)
genWellTypedPath ty' = do
  paths <- ask
  case fromMaybe [] (Map.lookup ty' paths) of
    [] -> empty
    es -> Gen.element es

-- | Generate either known types from the environment or new types.
genKnownTypeMaybe :: forall k a. GTyM k a (Ty k)
genKnownTypeMaybe = do
  known <- ask
  if Map.null known
    then genTy
    else
      Gen.frequency
        [ (2, Gen.element $ Map.keys known),
          (1, genTy)
        ]

-- | Sample.
-- >>> flip evalStateT (Seed.from 1) $ sample genTy
-- TList TUnit
--
-- >>> e = runIdentity . flip evalStateT (Seed.from 6) $ sample $ genWellTypedExp (TList TBool) :: Exp 'Eager Char
-- >>> pprintTerm e
-- "(\\x0 -> x0) (ite (foldr (foldr (\\x1 -> \\x2 -> x2) (\\x3 -> \\x4 -> \\x5 -> x3) [] (ite True (\\x6 -> x6) (\\x7 -> x7)) (\\x8 -> (\\x9 -> \\x10 -> x9) x8)) True (foldr (\\x11 -> \\x12 -> x12) (ite True (\\x13 -> []) (\\x14 -> []) (ite True (\\x15 -> True) (\\x16 -> False))) (ite ((\\x17 -> True) ()) ((\\x18 -> []) ()) []))) (foldr (\\x19 -> ite x19 (foldr (foldr (\\x20 -> \\x21 -> x21) (\\x22 -> \\x23 -> x23) []) (\\x24 -> x24) []) (ite x19 (\\x25 -> x25) (\\x26 -> x26))) ((:) (foldr (\\x27 -> \\x28 -> x28) True []) ((\\x29 -> []) False)) (ite True ((\\x30 -> \\x31 -> []) () ((\\x32 -> x32) ())) (ite False (foldr (\\x33 -> \\x34 -> x34) [] []) ((\\x35 -> []) ())))) (ite (ite ((\\x36 -> x36) False) (foldr (\\x37 -> \\x38 -> x38) False []) ((\\x39 -> x39) False)) (foldr ((\\x40 -> \\x41 -> \\x42 -> x42) ()) ((\\x43 -> \\x44 -> []) False) (ite True [] [])) (\\x45 -> (\\x46 -> []) x45) (foldr (\\x47 -> (\\x48 -> \\x49 -> x49) x47) (\\x50 -> []) [] False)))"
--- >>> pprintTerm . fst $ nf' e
-- "[True]"
sample :: forall m a. Monad m => GenT m a -> StateT Seed.Seed m a
sample gen =
  let go :: StateT Seed.Seed m a
      go = do
        seed <- get
        let (seed', seed'') = Seed.split seed
        put seed''
        Tree.NodeT x _ <- lift . Tree.runTreeT . Gen.evalGenT 30 seed' $ gen
        maybe go pure x
   in go
