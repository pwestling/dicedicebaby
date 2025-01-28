{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Probability.Dice 
    ( DiceFormula(..)
    , constant
    , roll
    , isVariable
    , d6
    , d3
    , twoD6
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import Probability.Distribution (Distribution)
import qualified Probability.Distribution as D

import Data.Aeson.Types (withObject, withText, (.:), (.=), object)
import Data.Text (Text)

-- | Represents a dice roll formula like "2d6+1"
data DiceFormula = DiceFormula
    { dice :: Int     -- ^ Number of dice
    , sides :: Int    -- ^ Number of sides per die
    , modifier :: Int -- ^ Fixed modifier to add
    } deriving (Eq, Generic)

instance ToJSON DiceFormula where
    toJSON DiceFormula{..} = object
        [ "dice" .= dice
        , "sides" .= sides
        , "modifier" .= modifier
        ]

instance FromJSON DiceFormula where
    parseJSON = withObject "DiceFormula" $ \v -> DiceFormula
        <$> v .: "dice"
        <*> v .: "sides"
        <*> v .: "modifier"

-- | Create a constant value (0 dice)
constant :: Int -> DiceFormula
constant value = DiceFormula 
    { dice = 0
    , sides = 1
    , modifier = value 
    }

mkDiceFormula :: Int -> Int -> Int -> DiceFormula
mkDiceFormula dice sides modifier = DiceFormula
    { dice = dice
    , sides = sides
    , modifier = modifier
    }

-- | Roll the dice and return a distribution of results
roll :: DiceFormula -> Distribution Int
roll DiceFormula{..} 
    | dice == 0 = D.singleton modifier
    | otherwise = addModifier $ combineDice dice singleDie
  where
    -- Distribution for a single die
    singleDie = D.uniform [1..sides]
    
    -- Combine multiple dice
    combineDice 1 dist = dist
    combineDice n dist = D.bind dist $ \x ->
        D.map (+ x) (combineDice (n-1) dist)
    
    -- Add the modifier if non-zero
    addModifier dist
        | modifier == 0 = dist
        | otherwise = D.map (+ modifier) dist

-- | Check if this formula involves dice rolls
isVariable :: DiceFormula -> Bool
isVariable DiceFormula{..} = dice > 0

-- | Show dice formula in standard notation (e.g. "2D6+1")
instance Show DiceFormula where
    show DiceFormula{..} = 
        let base = if dice > 1 
                  then show dice ++ "D" ++ show sides
                  else "D" ++ show sides
        in case modifier of
            0 -> base
            m | m > 0 -> base ++ "+" ++ show m
            m -> base ++ show m

-- Common formulas
d6 :: DiceFormula
d6 = DiceFormula 1 6 0

d3 :: DiceFormula
d3 = DiceFormula 1 3 0

twoD6 :: DiceFormula
twoD6 = DiceFormula 2 6 0 