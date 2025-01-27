{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Probability.Warhammer.Profile where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Probability.Distribution (Distribution)
import Probability.Dice (DiceFormula)
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Hashable as Hash
import Data.Set (Set)
import Data.List (intercalate)
import Data.Hashable (Hashable, hashWithSalt)

data AttackStage 
    = Start
    | Attacks
    | Hits
    | Wounds
    | FailedSaves
    | Damage
    | FeltDmg
    | ModelsSlain
    | Complete
    deriving (Eq, Ord, Show, Generic)

instance ToJSON AttackStage
instance FromJSON AttackStage

data AttackSequence = AttackSequence
    { values :: Map.Map AttackStage Int
    , frozen :: Map.Map AttackStage Int
    } deriving (Eq, Show, Generic)

instance ToJSON AttackSequence
instance FromJSON AttackSequence

instance Ord AttackSequence where
    compare a1 a2 = compare (Map.toList $ values a1) (Map.toList $ values a2)

-- Helper functions for AttackSequence
getValue :: AttackStage -> AttackSequence -> Int
getValue stage AttackSequence{..} = Map.findWithDefault 0 stage values

getFrozen :: AttackStage -> AttackSequence -> Int
getFrozen stage AttackSequence{..} = Map.findWithDefault 0 stage frozen

-- | Modifier typeclass for things that can modify attack sequences
class Modifier m where
    modifyAttacker :: m -> AttackProfile -> Defender -> AttackProfile
    modifyDefender :: m -> AttackProfile -> Defender -> Defender
    modifyRoll :: m -> Distribution DieResult -> AttackStage -> AttackProfile -> Defender -> Distribution DieResult

data DieResult = DieResult
    { value :: Int
    , sequence :: Maybe AttackSequence
    , passesCheck :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToJSON DieResult
instance FromJSON DieResult

data AttackProfile = AttackProfile
    { apName :: Text
    , models :: Int
    , gunsPerModel :: Int
    , attacks :: DiceFormula
    , ballisticSkill :: Int
    , strength :: Int
    , armorPen :: Int
    , damage :: DiceFormula
    , keywords :: [Text]
    , modifiers :: [ModifierBox]
    } deriving (Show, Generic)

instance ToJSON AttackProfile
instance FromJSON AttackProfile

data DefenderProfile = DefenderProfile
    { dpName :: Text
    , models :: Int
    , toughness :: Int
    , armorSave :: Int
    , invulnSave :: Maybe Int
    , wounds :: Int
    , feelNoPain :: Maybe Int
    , keywords :: [Text]
    , isLeader :: Bool
    , modifiers :: [ModifierBox]
    } deriving (Show, Generic)

instance ToJSON DefenderProfile
instance FromJSON DefenderProfile

data Defender = Defender
    { defenderName :: Text
    , profiles :: [DefenderProfile]
    } deriving (Show, Generic)

instance ToJSON Defender
instance FromJSON Defender

-- | Get all modifiers from a defender's profiles
getDefenderModifiers :: Defender -> [ModifierBox]
getDefenderModifiers = concatMap modifiers . profiles

-- | Check if defender has multiple save profiles
multipleSaveProfiles :: Defender -> Bool
multipleSaveProfiles Defender{..} = case profiles of
    [] -> False
    (p:ps) -> any (\prof -> armorSave prof /= armorSave p || 
                           invulnSave prof /= invulnSave p) ps

-- | Check if defender has any feel no pain saves
hasAnyFnp :: Defender -> Bool
hasAnyFnp = any (isJust . feelNoPain) . profiles

-- | Check if defender has multiple wound values
multipleWoundProfiles :: Defender -> Bool
multipleWoundProfiles Defender{..} = case profiles of
    [] -> False
    (p:ps) -> any (\prof -> wounds prof /= wounds p) ps

-- | Get highest toughness in unit, prioritizing non-leader models
getHighestToughness :: Defender -> Int
getHighestToughness Defender{..} = 
    let nonLeaderToughness = [t | DefenderProfile{..} <- profiles, not isLeader, let t = toughness]
        leaderToughness = [t | DefenderProfile{..} <- profiles, isLeader, let t = toughness]
    in case nonLeaderToughness of
        [] -> maximum (leaderToughness ++ [0])
        ts -> maximum ts

-- | Get next profile to allocate wounds to
getNextProfile :: Defender -> Int -> Maybe DefenderProfile
getNextProfile Defender{..} modelsSlain = 
    let remainingProfiles = dropWhile (\p -> models p <= modelsSlain) profiles
    in case remainingProfiles of
        [] -> Nothing
        (p:_) -> Just p

-- | Existential type to box up different modifiers
data ModifierBox = forall m. (Modifier m, ToJSON m, FromJSON m) => ModifierBox m

instance ToJSON ModifierBox where
    toJSON (ModifierBox m) = toJSON m

instance FromJSON ModifierBox where
    parseJSON = error "ModifierBox FromJSON not implemented" -- Needs custom implementation

-- Common modifiers
data RerollAllFails = RerollAllFails AttackStage
    deriving (Show, Generic)

instance Modifier RerollAllFails where
    modifyAttacker m p d = p
    modifyDefender m p d = d
    modifyRoll (RerollAllFails stage) dist currentStage profile defender
        | stage /= currentStage = dist
        | otherwise = rerollFails dist
      where
        rerollFails d = d >>= \result -> 
            if not (passesCheck result == Just False)
                then return result
                else dist

instance ToJSON RerollAllFails
instance FromJSON RerollAllFails

-- | Lethal Hits: Automatically wound on hit rolls of 6
data LethalHits = LethalHits
    deriving (Show, Generic)

instance Modifier LethalHits where
    modifyAttacker _ p _ = p
    modifyDefender _ p _ = p
    modifyRoll _ dist stage profile defender
        | stage /= Hits = dist
        | otherwise = dist >>= \result ->
            if value result == 6
                then return $ result { sequence = Just $ createAttackSequence Wounds 1 }
                else return result

instance ToJSON LethalHits
instance FromJSON LethalHits

-- | Devastating Wounds: On wound rolls of 6, ignore armor saves
data DevastatingWounds = DevastatingWounds
    deriving (Show, Generic)

instance Modifier DevastatingWounds where
    modifyAttacker _ p _ = p
    modifyDefender _ p _ = p
    modifyRoll _ dist stage profile defender
        | stage /= Wounds = dist
        | otherwise = dist >>= \result ->
            if value result == 6
                then return $ result { sequence = Just $ createAttackSequence FailedSaves 1 }
                else return result

instance ToJSON DevastatingWounds
instance FromJSON DevastatingWounds

-- Helper function to create attack sequences
createAttackSequence :: AttackStage -> Int -> AttackSequence
createAttackSequence stage val = AttackSequence 
    { values = Map.singleton stage val
    , frozen = Map.empty
    }

-- Example profiles
spaceMarine :: DefenderProfile
spaceMarine = DefenderProfile
    { dpName = "Space Marine"
    , models = 1
    , toughness = 4
    , armorSave = 3
    , invulnSave = Nothing
    , wounds = 2
    , feelNoPain = Nothing
    , keywords = []
    , isLeader = False
    , modifiers = []
    }

bolter :: AttackProfile
bolter = AttackProfile
    { apName = "Bolter"
    , models = 1
    , gunsPerModel = 1
    , attacks = DiceFormula 0 1 2  -- Fixed 2 shots
    , ballisticSkill = 3
    , strength = 4
    , armorPen = 0
    , damage = DiceFormula 0 1 1  -- Fixed 1 damage
    , keywords = []
    , modifiers = []
    }

-- Add after the existing AttackSequence functions:

-- | Create a new sequence with a single value at the given stage
createSequence :: AttackStage -> Int -> AttackSequence
createSequence stage val = AttackSequence
    { values = Map.singleton stage val
    , frozen = Map.empty
    }

-- | Clear all frozen values
clearFrozen :: AttackSequence -> AttackSequence
clearFrozen seq = seq { frozen = Map.empty }

-- | Freeze all values at a stage
freezeAll :: AttackStage -> AttackSequence -> AttackSequence
freezeAll stage seq = AttackSequence
    { values = Map.delete stage (values seq)
    , frozen = Map.insert stage (getValue stage seq + getFrozen stage seq) (frozen seq)
    }

-- | Freeze one value at a stage
freezeOne :: AttackStage -> AttackSequence -> AttackSequence
freezeOne stage seq = AttackSequence
    { values = Map.adjust (subtract 1) stage (values seq)
    , frozen = Map.insertWith (+) stage 1 (frozen seq)
    }

-- | Add or update a value for a stage
withValue :: AttackStage -> Int -> AttackSequence -> AttackSequence
withValue stage val seq = seq { values = Map.insert stage val (values seq) }

-- | Add or update a frozen value for a stage
withFrozen :: AttackStage -> Int -> AttackSequence -> AttackSequence
withFrozen stage val seq = seq { frozen = Map.insert stage val (frozen seq) }

-- | Combine two attack sequences
instance Semigroup AttackSequence where
    (<>) a1 a2 = AttackSequence
        { values = Map.unionWith (+) (values a1) (values a2)
        , frozen = Map.unionWith (+) (frozen a1) (frozen a2)
        }

instance Monoid AttackSequence where
    mempty = AttackSequence Map.empty Map.empty
    mappend = (<>)

-- | Show stages in order, omitting zeros unless all values are zero
instance Show AttackSequence where
    show AttackSequence{..} = 
        let allZeros = all (== 0) (Map.elems values) && all (== 0) (Map.elems frozen)
            allStages = Map.keysSet values `Set.union` Map.keysSet frozen
            showStage stage = 
                let val = getValue stage
                    frozenVal = getFrozen stage
                in if frozenVal /= 0
                    then show stage ++ ":" ++ show val ++ "+" ++ show frozenVal ++ "*"
                    else show stage ++ ":" ++ show val
            parts = map showStage (Set.toList allStages)
        in "[" ++ intercalate ", " parts ++ "]"

-- | Hash an attack sequence
instance Hashable AttackSequence where
    hashWithSalt salt AttackSequence{..} = 
        let nonZeroValues = Map.filter (/= 0) values
            nonZeroFrozen = Map.filter (/= 0) frozen
        in hashWithSalt salt (Map.toList nonZeroValues, Map.toList nonZeroFrozen) 