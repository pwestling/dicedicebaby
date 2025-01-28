{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}

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
    , Probability.Distribution.map
    , distmap
    , repeated
    , (>>==)
    , liftM
    , combine
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ratio
import Data.List (sortBy)
import GHC.Generics (Generic)
import Control.Monad (join, ap)
import Data.Aeson
import Data.List (intercalate)
import Probability.MemoUgly (memo)
import Data.Function (on)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)

-- import Debug.Trace (trace, traceShow, traceShowId)

-- labelTraceId :: (Show a) => String -> a -> a
-- labelTraceId label x = trace (label <> ": " <> show x) x
-- labelTraceId label x = x


-- | Core Distribution type representing a probability distribution over values
newtype Distribution a = Distribution { probabilities :: Map a Rational }
    deriving ( Generic)

instance ToJSONKey a => ToJSON (Distribution a) where
    toJSON = toJSON . probabilities

instance (FromJSONKey a, Ord a) => FromJSON (Distribution a) where
    parseJSON = fmap Distribution . parseJSON


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


distmap :: (Ord a, Ord b) => (a -> b) -> Distribution a -> Distribution b
distmap f (Distribution ps) = Distribution $ Map.mapKeysWith (+) f ps

map :: (Ord a, Ord b) => (a -> b) -> Distribution a -> Distribution b
map = distmap

-- | Combine two distributions by applying the semigroup operation to their values
combineRaw :: (Ord a, Semigroup a, Show a, NFData a) => Distribution a -> Distribution a -> Distribution a
combineRaw (Distribution p1) (Distribution p2) = Distribution $ Map.fromListWith (+)
    [  force (x <> y, p1' * p2')
    | (x, p1') <- Map.toList p1
    , (y, p2') <- Map.toList p2
    ]

combine :: (Ord a, Semigroup a, Show a, NFData a) => Distribution a -> Distribution a -> Distribution a
combine = memo combineRaw

repeated :: (Ord a, Semigroup a, Show a, NFData a) => Distribution a -> Int -> Distribution a
repeated d 0 = empty
repeated d 1 = d
repeated d n = combine d (repeated d (n - 1))

-- | Bind operation for distributions
bind :: (Ord b, Show b, Show a, NFData b) => Distribution a -> (a -> Distribution b) -> Distribution b
bind (Distribution ps) f = Distribution $ Map.fromListWith (+)
    [ force (y, p * q)
    | (x, p) <- Map.toList ps
    , (y, q) <- Map.toList $ probabilities (f x)
    ]


(>>==) :: (Ord b, Show b, Show a, NFData b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>==) = bind
infixl 1 >>==

liftM :: (Ord a, Ord b, Show b, Show a, NFData b) => (a -> Distribution b) -> Distribution a -> Distribution b
liftM f d = bind d f
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

instance Ord a => Ord (Distribution a) where
    compare = compare `on` probabilities

-- Helper instances
instance Ord a => Semigroup (Distribution a) where
    (<>) = merge

instance Ord a => Monoid (Distribution a) where
    mempty = empty
    mappend = (<>)

instance Show a => Show (Distribution a) where
    show (Distribution ps) = 
        let entries = [ show k ++ ": " ++ showDecimal p 
                     | (k, p) <- Map.toList ps ]
        in "Distribution [\n  " ++ 
           intercalate "\n  " entries ++ 
           "\n]"
         where
           showDecimal r = show (fromRational r :: Double)

instance Eq a => Eq (Distribution a) where
    (Distribution ps1) == (Distribution ps2) = ps1 == ps2

instance (NFData a) => NFData (Distribution a) 

