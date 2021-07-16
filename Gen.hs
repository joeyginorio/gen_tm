{- Gen.hs
   ======
   Term generator for simply typed lambda calculus. With STLC2CL we can
   also use the generator for combinatory logic. Implementation has
   two stages.
     (i) Type generation
     (ii) Type-directed term generation -}

import STLC

import Control.Applicative
import Control.Monad.Search
import Control.Monad.Trans
import Data.Monoid (Sum(..))


{- ============================ Term Generator ============================== -}

-- Search + Maybe monad
-- Our type-directed search might not always return something
type SearchM c = SearchT c Maybe

-- Generate unit
genUnit :: Context -> Type -> SearchM (Sum Integer) Term
genUnit _ (TyUnit) = do cost' (Sum 1)
                        return TmUnit
genUnit _ _        = lift Nothing

-- Generate booleans
genBool :: Context -> Type -> SearchM (Sum Integer) Term
genBool _ (TyBool) = do cost' (Sum 1)
                        return TmTrue
                 <|> do cost' (Sum 1)
                        return TmFalse
genBool _ _        = lift Nothing

-- Generate variables


-- genVar :: Context -> Type -> Search (Sum Integer) Term
-- genVar ((x,ty):ctx) = do cost' (Sum 1)
                         -- return $ TmVar x

-- data BTree = Nil | Node BTree Bool BTree deriving Show

-- btrees :: Search (Sum Integer) BTree
-- btrees = do cost' (Sum 0)
--             return Nil
--      <|> do cost' (Sum 1)
--             Node <$> btrees <*> (pure True) <*> btrees
