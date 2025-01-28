{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Probability.Warhammer.Profile where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Probability.Distribution (Distribution)
import qualified Probability.Distribution as D
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Hashable (Hashable, hashWithSalt)
import Probability.Dice as Dice

import Data.Aeson.Types (withObject, withText, (.:), (.=), object)

import Debug.Trace (trace, traceShow, traceShowId)

llabelTraceId :: (Show a) => String -> a -> a
llabelTraceId label x = trace (label <> ": " <> show x) x
-- labelTraceId label x = x


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

instance Hashable AttackStage

instance ToJSONKey AttackStage
instance FromJSONKey AttackStage

instance ToJSON AttackStage where
    toJSON = String . \case
        Start -> "START"
        Attacks -> "ATTACKS"
        Hits -> "HITS"
        Wounds -> "WOUNDS"
        FailedSaves -> "FAILED_SAVES"
        Damage -> "DAMAGE"
        FeltDmg -> "FELT_DMG"
        ModelsSlain -> "MODELS_SLAIN"
        Complete -> "COMPLETE"

instance FromJSON AttackStage where
    parseJSON = withText "AttackStage" $ \case
        "START" -> pure Start
        "ATTACKS" -> pure Attacks
        "HITS" -> pure Hits
        "WOUNDS" -> pure Wounds
        "FAILED_SAVES" -> pure FailedSaves
        "DAMAGE" -> pure Damage
        "FELT_DMG" -> pure FeltDmg
        "MODELS_SLAIN" -> pure ModelsSlain
        "COMPLETE" -> pure Complete
        _ -> fail "Invalid AttackStage"

data AttackSequence = AttackSequence
    { values :: Map.Map AttackStage Int
    , frozen :: Map.Map AttackStage Int
    } deriving (Eq, Generic)

instance ToJSON AttackSequence
instance FromJSON AttackSequence

instance ToJSONKey AttackSequence
instance FromJSONKey AttackSequence

instance Ord AttackSequence where
    compare a1 a2 = compare (Map.toList $ (\a -> a.values) a1) (Map.toList $ (\a -> a.values) a2)

-- Helper functions for AttackSequence
getValue :: AttackStage -> AttackSequence -> Int
getValue stage seq = Map.findWithDefault 0 stage seq.values

getFrozen :: AttackStage -> AttackSequence -> Int
getFrozen stage seq = Map.findWithDefault 0 stage seq.frozen

-- | Modifier typeclass for things that can modify attack sequences
class Modifier m where
    modifyAttacker :: m -> AttackProfile -> Defender -> AttackProfile
    modifyAttacker _ a _ = a
    modifyDefender :: m -> AttackProfile -> Defender -> Defender
    modifyDefender _ _ d = d
    modifyRoll :: m -> Distribution DieResult -> AttackStage -> AttackProfile -> Defender -> Distribution DieResult
    modifyRoll _ dist _ _ _ = dist

data DieResult = DieResult
    { value :: Int
    , sequence :: AttackSequence
    , passesCheck :: Maybe Bool
    } deriving (Eq, Show, Generic, Ord)

instance ToJSON DieResult
instance FromJSON DieResult

data AttackProfile = AttackProfile
    { name :: Text
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
    { name :: Text
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
getDefenderModifiers defender = concatMap (\p -> p.modifiers) defender.profiles

-- | Check if defender has multiple save profiles
multipleSaveProfiles :: Defender -> Bool
multipleSaveProfiles defender = case defender.profiles of
    [] -> False
    (p:ps) -> any (\prof -> prof.armorSave /= p.armorSave || 
                           prof.invulnSave /= p.invulnSave) ps

-- | Check if defender has any feel no pain saves
hasAnyFnp :: Defender -> Bool
hasAnyFnp defender = any (\p -> isJust p.feelNoPain) defender.profiles

-- | Check if defender has multiple wound values
multipleWoundProfiles :: Defender -> Bool
multipleWoundProfiles defender = case defender.profiles of
    [] -> False
    (p:ps) -> any (\prof -> prof.wounds /= p.wounds) ps

-- | Get highest toughness in unit, prioritizing non-leader models
getHighestToughness :: Defender -> Int
getHighestToughness defender = 
    let nonLeaderToughness = [prof.toughness | prof <- defender.profiles, not prof.isLeader]
        leaderToughness = [prof.toughness | prof <- defender.profiles, prof.isLeader]
    in case nonLeaderToughness of
        [] -> maximum (leaderToughness ++ [0])
        ts -> maximum ts

-- | Get next profile to allocate wounds to
getNextProfile :: Defender -> Int -> Maybe DefenderProfile
getNextProfile defender modelsSlain = 
    let remainingProfiles = dropWhile (\p -> p.models <= modelsSlain) defender.profiles
    in case remainingProfiles of
        [] -> Nothing
        (p:_) -> Just p

-- | Existential type to box up different modifiers
data ModifierBox = forall m. (Modifier m, ToJSON m, FromJSON m, Show m) => ModifierBox m

instance Show ModifierBox where
    show (ModifierBox m) = show m

instance ToJSON ModifierBox where
    toJSON (ModifierBox m) = toJSON m

instance FromJSON ModifierBox where
    parseJSON = error "ModifierBox FromJSON not implemented" -- Needs custom implementation

-- Common modifiers
data RerollAllFails = RerollAllFails AttackStage
    deriving (Show, Generic)

instance Modifier RerollAllFails where
    modifyRoll (RerollAllFails stage) dist currentStage _ _
        | stage /= currentStage = dist
        | otherwise = llabelTraceId "RerollAllFails" $ rerollFails dist
      where
        rerollFails d = d `D.bind` \result -> 
            if not ((.passesCheck) result == Just False)
                then D.singleton result
                else dist

instance ToJSON RerollAllFails
instance FromJSON RerollAllFails

-- | Lethal Hits: Automatically wound on hit rolls of 6
data LethalHits = LethalHits
    deriving (Show, Generic)

instance Modifier LethalHits where
    modifyRoll _ dist stage _ _
        | stage /= Hits = dist
        | otherwise = D.bind dist $ \result ->
            if (.value) result == 6
                then D.singleton $ result { sequence = withFrozen Hits 1 (createAttackSequence Wounds 1) }
                else D.singleton result

instance ToJSON LethalHits
instance FromJSON LethalHits

-- | Devastating Wounds: On wound rolls of 6, ignore armor saves
data DevastatingWounds = DevastatingWounds
    deriving (Show, Generic)

instance Modifier DevastatingWounds where
    modifyAttacker :: DevastatingWounds -> AttackProfile -> Defender -> AttackProfile
    modifyAttacker _ a _ = a
    modifyDefender _ _ d = d
    modifyRoll _ dist stage _ _
        | stage /= Wounds = dist
        | otherwise = D.bind dist $ \result ->
            if (.value) result == 6
                then D.singleton $ result { sequence = withFrozen Wounds 1 (createAttackSequence FailedSaves 1) }
                else D.singleton result

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
    { name = T.pack "Space Marine"
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
    { name = "Bolter"
    , models = 1
    , gunsPerModel = 1
    , attacks = Dice.constant 2  -- Fixed 2 shots
    , ballisticSkill = 3
    , strength = 4
    , armorPen = 0
    , damage = Dice.constant 1  -- Fixed 1 damage
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
    { values = Map.delete stage ((.values) seq)
    , frozen = Map.insert stage (getValue stage seq + getFrozen stage seq) ((.frozen) seq)
    }

-- | Freeze one value at a stage
freezeOne :: AttackStage -> AttackSequence -> AttackSequence
freezeOne stage seq = AttackSequence
    { values = Map.adjust (subtract 1) stage ((.values) seq)
    , frozen = Map.insertWith (+) stage 1 ((.frozen) seq)
    }

-- | Add or update a value for a stage
withValue :: AttackStage -> Int -> AttackSequence -> AttackSequence
withValue stage val seq = seq { values = Map.insert stage val ((.values) seq) }

-- | Add or update a frozen value for a stage
withFrozen :: AttackStage -> Int -> AttackSequence -> AttackSequence
withFrozen stage val seq = seq { frozen = Map.insert stage val ((.frozen) seq) }

-- | Combine two attack sequences
instance Semigroup AttackSequence where
    (<>) a1 a2 = AttackSequence
        { values = Map.unionWith (+) ((.values) a1) ((.values) a2)
        , frozen = Map.unionWith (+) ((.frozen) a1) ((.frozen) a2)
        }

instance Monoid AttackSequence where
    mempty = AttackSequence Map.empty Map.empty
    mappend = (<>)

-- | Show stages in order, omitting zeros unless all values are zero
instance Show AttackSequence where
    show seq@AttackSequence{..} = 
        let 
            allStages = Map.keysSet values `Set.union` Map.keysSet frozen
            showStage stage = 
                let val = getValue stage seq
                    frozenVal = getFrozen stage seq
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