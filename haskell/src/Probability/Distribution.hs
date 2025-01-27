{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Probability.Distribution 
    ( Distribution(..)
    , singleton
    , empty
    , uniform
    , prune
    , bind
    , merge
    , normalize
    , scale
    , d6
    , d3
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ratio
import Data.List (sortBy)
import GHC.Generics (Generic)
import Control.Monad (join)

-- | Core Distribution type representing a probability distribution over values
newtype Distribution a = Distribution { probabilities :: Map a Rational }
    deriving (Eq, Show, Generic)

epsilon :: Rational
epsilon = 1 % 100000000000000

pruneFactor :: Rational
pruneFactor = 1 % 100000

-- | Create a singleton distribution with probability 1 for the given value
singleton :: Ord a => a -> Distribution a
singleton x = Distribution $ Map.singleton x 1

-- | Create an empty distribution
empty :: Distribution a
empty = Distribution Map.empty

-- | Create a uniform distribution over a list of values
uniform :: Ord a => [a] -> Distribution a
uniform xs = Distribution $ Map.fromList [(x, 1 % fromIntegral (length xs)) | x <- xs]

-- | Map a function over the values in a distribution
instance Functor Distribution where
    fmap f (Distribution ps) = Distribution $ Map.mapKeysWith (+) f ps

-- | Monadic bind for distributions
instance Monad Distribution where
    return = singleton
    (>>=) = bind

instance Applicative Distribution where
    pure = return
    (<*>) = ap

-- | Bind operation for distributions
bind :: Ord b => Distribution a -> (a -> Distribution b) -> Distribution b
bind (Distribution ps) f = Distribution $ Map.fromListWith (+)
    [ (y, p * q)
    | (x, p) <- Map.toList ps
    , (y, q) <- Map.toList $ probabilities (f x)
    ]

-- | Merge two distributions by adding their probabilities
merge :: Ord a => Distribution a -> Distribution a -> Distribution a
merge (Distribution p1) (Distribution p2) = 
    Distribution $ Map.unionWith (+) p1 p2

-- | Normalize a distribution so probabilities sum to 1
normalize :: Distribution a -> Distribution a
normalize (Distribution ps) = Distribution $ Map.map (/ total) ps
  where
    total = sum $ Map.elems ps

-- | Scale all probabilities by a factor
scale :: Rational -> Distribution a -> Distribution a
scale factor (Distribution ps) = Distribution $ Map.map (* factor) ps

-- | Prune small probabilities from a distribution
prune :: Ord a => Distribution a -> Distribution a
prune (Distribution ps) = 
    let maxProb = maximum $ Map.elems ps
        threshold = maxProb * pruneFactor
    in Distribution $ Map.filter (> threshold) ps

-- Common distributions
d6 :: Distribution Int
d6 = uniform [1..6]

d3 :: Distribution Int
d3 = uniform [1..3]

-- Helper instances
instance Semigroup (Distribution a) where
    (<>) = merge

instance Monoid (Distribution a) where
    mempty = empty
    mappend = (<>)

instance Show (Distribution a) where
    show (Distribution ps) = "Distribution " ++ show ps

instance Eq (Distribution a) where
    (Distribution ps1) == (Distribution ps2) = ps1 == ps2

instance Ord (Distribution a) where
    compare (Distribution ps1) (Distribution ps2) = compare ps1 ps2


