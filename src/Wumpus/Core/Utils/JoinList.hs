{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils.JoinList
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- A \"join list\" datatype and operations. 
--
-- A join list is implemented a binary tree, so joining two 
-- lists (catenation, aka (++)) is a cheap operation. 
--
-- This constrasts with the regular list datatype which is a 
-- cons list: while consing on a regular list is by nature cheap, 
-- joining (++) is expensive. 
--
-- This version has no Empty constructor, so empty join lists 
-- cannot be built.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Utils.JoinList 
  ( 
  -- * Join list datatype, opaque.
    JoinList

  -- * Left view as per Data.Sequence  
  , ViewL(..)

  -- * Conversion between join lists and regular lists
  , fromList
  , toList
  , toListF_rl

  -- * Construction
  , one
  , cons
  , snoc
  , join

  -- * Basic functions  
  , head

  , accumMapL
  , isOne
  , isMany

  -- * Left view
  , viewl


  
  ) where


import Control.Applicative hiding ( empty )

import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Monoid ( mappend )
import Data.Traversable ( Traversable(..) )

import Prelude hiding ( head )

data JoinList a = One a 
                | Join (JoinList a) (JoinList a)
  deriving (Eq)

data ViewL a = OneL a | a :< (JoinList a)
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Show a => Show (JoinList a) where
  showsPrec _ xs = showString "fromList " . shows (toList xs) 


instance Functor JoinList where
  fmap f (One a)    = One (f a)
  fmap f (Join t u) = Join (fmap f t) (fmap f u)



instance Foldable JoinList where
  foldMap f (One a)    = f a
  foldMap f (Join t u) = F.foldMap f t `mappend` F.foldMap f u

  foldr                = joinfoldr
  foldl                = joinfoldl


instance Traversable JoinList where
  traverse f (One a)    = One <$> f a
  traverse f (Join t u) = Join <$> traverse f t <*> traverse f u


-- Views

instance Functor ViewL where
  fmap f (OneL a)       = OneL $ f a
  fmap f (a :< as)      = f a :< fmap f as


--------------------------------------------------------------------------------
-- Conversion

-- | Convert a join list to a regular list.
--
toList :: JoinList a -> [a]
toList = joinfoldr (:) []

-- | Build a join list from a regular list.
--
-- This builds a tall skinny list.
--
-- WARNING - throws an error on empty list.
--

fromList :: [a] -> JoinList a
fromList []     = error "Wumpus.Core - internal error empty JoinList"
fromList [x]    = One x
fromList (x:xs) = Join (One x) (fromList xs)


-- Note -- this works from Right to Left...
--
toListF_rl :: (a -> b) -> JoinList a -> [b]
toListF_rl f = step []
  where
    step acc (One x)     = f x : acc
    step acc (Join t u)  = let acc' = step acc u in step acc' t



--------------------------------------------------------------------------------

isOne :: JoinList a -> Bool
isOne (One _)       = True
isOne _             = False

isMany :: JoinList a -> Bool
isMany (Join _ _)   = True
isMany _            = False




-- | Create a singleton join list.
--
one :: a -> JoinList a
one = One


infixr 5 `cons`

-- | Cons an element to the front of the join list.
--
cons :: a -> JoinList a -> JoinList a
cons a xs = Join (One a) xs  

-- | Snoc an element to the tail of the join list.
--
snoc :: JoinList a -> a -> JoinList a
snoc xs a = Join xs (One a)




infixr 5 `join`

-- | Because there is no empty join list, join is Join.
--
join :: JoinList a -> JoinList a -> JoinList a
join = Join

--------------------------------------------------------------------------------
-- Basic functions

-- | Extract the first element of a join list - i.e. the leftmost
-- element of the left spine. An error is thrown if the list is 
-- empty. 
-- 
-- This function performs a traversal down the left spine, so 
-- unlike @head@ on regular lists this function is not performed 
-- in constant time.
--
head :: JoinList a -> a
head (One a)    = a
head (Join t _) = head t




accumMapL :: (x -> st -> (y,st)) -> JoinList x -> st -> (JoinList y,st)
accumMapL f xs st0 = go xs st0 
  where
    go (One x)     st = let (y,st') = f x st in (One y,st')
    go (Join t u)  st = (Join v w, st'')
                             where (v,st')  = go t st
                                   (w,st'') = go u st'




-- | Right-associative fold of a JoinList.
--
joinfoldr :: (a -> b -> b) -> b -> JoinList a -> b
joinfoldr f = go
  where
    go e (One a)    = f a e
    go e (Join t u) = go (go e u) t


-- | Left-associative fold of a JoinList.
--
joinfoldl :: (b -> a -> b) -> b -> JoinList a -> b
joinfoldl f = go 
  where
    go e (One a)    = f e a
    go e (Join t u) = go (go e t) u

--------------------------------------------------------------------------------
-- Views

-- | Access the left end of a sequence.
--
-- Unlike the corresponing operation on Data.Sequence this is 
-- not a cheap operation, the joinlist must be traversed down 
-- the left spine to find the leftmost node.
--
-- Also the traversal may involve changing the shape of the 
-- underlying binary tree.
--
viewl :: JoinList a -> ViewL a
viewl (One a)    = OneL a
viewl (Join t u) = step t u
  where
    step (One a)      r = a :< r
    step (Join t' u') r = step t' (Join u' r)


