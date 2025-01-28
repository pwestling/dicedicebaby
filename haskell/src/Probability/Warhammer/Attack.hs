{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Probability.Warhammer.Attack 
    ( getWoundThreshold
    , simulateAttacks
    , AttackResults(..)
    , TimingInfo(..)
    , TestInput(..)
    , AttackInput(..)
    , DefenderInput(..)
    , ExpectedProbability(..)
    , TestResult(..)
    , processTestInput
    , resultsToOutput
    , checkProbability
    , stageProbs
    , validateResults
    , collapseStages
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.System (SystemTime, getSystemTime)
import Control.Monad (join, unless, forM_)
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import Probability.Distribution (Distribution)
import qualified Probability.Distribution as D
import Probability.Dice (DiceFormula)
import qualified Probability.Dice as Dice
import Probability.Warhammer.Profile
import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Data.Aeson.Types (withObject, withText, (.:), (.=), object)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import Probability.MemoUgly (memo)

-- import Debug.Trace (trace, traceShow, traceShowId, traceId)

-- labelTraceId :: (Show a) => String -> a -> a
-- labelTraceId label x = trace (label <> ": " <> show x) x
-- labelTraceId label x = x
-- | Get wound threshold based on strength vs toughness comparison
getWoundThreshold :: Int -> Int -> Int
getWoundThreshold strength toughness
    | strength >= 2 * toughness = 2
    | strength > toughness = 3
    | strength == toughness = 4
    | strength <= toughness `div` 2 = 6
    | otherwise = 5

-- | Timing information for performance tracking
data TimingInfo = TimingInfo
    { attackRollTime :: Double
    , hitRollTime :: Double
    , woundRollTime :: Double
    , saveRollTime :: Double
    , damageRollTime :: Double
    , fnpRollTime :: Double
    , slayModelsTime :: Double
    , totalTime :: Double
    } deriving (Show, Generic)

instance ToJSON TimingInfo where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase
instance FromJSON TimingInfo where
    parseJSON = genericParseJSON $ aesonDrop 0  snakeCase
-- | Results of attack simulation
data AttackResults = AttackResults
    { attacks :: Distribution AttackSequence
    , hits :: Distribution AttackSequence
    , wounds :: Distribution AttackSequence
    , damage :: Distribution AttackSequence
    , timing :: TimingInfo
    } deriving (Show, Generic)

instance ToJSON AttackResults
instance FromJSON AttackResults

collapseStage :: Distribution AttackSequence -> AttackStage -> Distribution AttackSequence
collapseStage dist stage = D.map (\seq -> createSequence stage (getValue stage seq + getFrozen stage seq)) dist

data CollapsedAttackResults = CollapsedAttackResults
    { hits :: Distribution AttackSequence
    , wounds :: Distribution AttackSequence
    , failedSaves :: Distribution AttackSequence
    , damage :: Distribution AttackSequence
    , feltDamage :: Distribution AttackSequence
    , modelsSlain :: Distribution AttackSequence
    } deriving (Generic)

instance Show CollapsedAttackResults where
    show c = "Results:\n" <> "Hits: " <> show c.hits <> "\n" <>
             "Wounds: " <> show c.wounds <> "\n" <>
             "Failed Saves: " <> show c.failedSaves <> "\n" <>
             "Damage: " <> show c.damage <> "\n" <>
             "Felt Damage: " <> show c.feltDamage <> "\n" <>
             "Models Slain: " <> show c.modelsSlain <> "\n"

collapseStages :: AttackResults -> CollapsedAttackResults
collapseStages results = CollapsedAttackResults
    { hits = collapseStage results.hits Hits
    , wounds = collapseStage results.wounds Wounds
    , failedSaves = collapseStage results.damage FailedSaves
    , damage = collapseStage results.damage Damage
    , feltDamage = collapseStage results.damage FeltDmg
    , modelsSlain = collapseStage results.damage ModelsSlain
    }

-- | Input format for test cases
data TestInput = TestInput
    { testName :: Text
    , attack :: AttackInput
    , defender :: DefenderInput
    , expected :: [ExpectedProbability]
    } deriving (Show, Generic)

instance FromJSON TestInput where
    parseJSON = withObject "TestInput" $ \v -> TestInput
        <$> v .: "name"
        <*> v .: "attack"
        <*> v .: "defender"
        <*> v .: "expected"

data ModifierType = RerollAllFailsType AttackStage
                  | LethalHitsType
                  | DevastatingWoundsType
                  deriving (Show, Generic)

instance FromJSON ModifierType where
    parseJSON = withObject "ModifierType" $ \v -> do
        typ <- v .: "type"
        case typ of
            "RerollAllFails" -> RerollAllFailsType <$> (parseJSON =<< (v .: "stage"))
            "LethalHits" -> pure LethalHitsType
            "DevastatingWounds" -> pure DevastatingWoundsType
            _ -> fail $ "Unknown modifier type: " ++ typ

modifierTypeToBox :: ModifierType -> ModifierBox
modifierTypeToBox (RerollAllFailsType stage) = ModifierBox $ RerollAllFails stage
modifierTypeToBox LethalHitsType = ModifierBox LethalHits
modifierTypeToBox DevastatingWoundsType = ModifierBox DevastatingWounds

data AttackInput = AttackInput
    { attacks :: DiceFormula
    , ballisticSkill :: Int
    , strength :: Int
    , armorPen :: Int
    , damage :: DiceFormula
    , modifiers :: [ModifierType]
    } deriving (Show, Generic)

instance FromJSON AttackInput where
    parseJSON = withObject "AttackInput" $ \v -> AttackInput
        <$> v .: "attacks"
        <*> v .: "ballistic_skill"
        <*> v .: "strength"
        <*> v .: "armor_pen"
        <*> v .: "damage"
        <*> v .: "modifiers"

data DefenderInput = DefenderInput
    { toughness :: Int
    , armorSave :: Int
    , invulnSave :: Maybe Int
    , wounds :: Int
    , models :: Int
    , feelNoPain :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON DefenderInput where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase
instance FromJSON DefenderInput where
    parseJSON = genericParseJSON $ aesonDrop 0  snakeCase

data ExpectedProbability = ExpectedProbability
    { stage :: AttackStage
    , value :: Int
    , probability :: Double
    , tolerance :: Double
    } deriving (Show, Generic)

instance FromJSON ExpectedProbability

-- | Output format for test results
data TestResult = TestResult
    { success :: Bool
    , probabilities :: AttackResults
    , error :: Maybe Text
    , timing :: Maybe TimingInfo
    } deriving (Show, Generic)

instance ToJSON TestResult where
    toJSON TestResult{..} = object
        [ "success" .= success
        , "probabilities" .= toJSON probabilities
        , "error" .= error
        , "timing" .= timing
        ]

-- | Convert test input to simulation inputs
processTestInput :: TestInput -> (AttackProfile, Defender)
processTestInput TestInput{..} = 
    ( AttackProfile
        { name = "Test Profile"
        , models = 1
        , gunsPerModel = 1
        , attacks = attack.attacks
        , ballisticSkill = attack.ballisticSkill
        , strength = attack.strength
        , armorPen = attack.armorPen
        , damage = attack.damage
        , modifiers = map modifierTypeToBox attack.modifiers
        , keywords = []
        }
    , Defender
        { defenderName = "Test Defender"
        , profiles = 
            [ DefenderProfile
                { name = "Test Model"
                , models = defender.models
                , toughness = defender.toughness
                , armorSave = defender.armorSave
                , invulnSave = defender.invulnSave
                , wounds = defender.wounds
                , feelNoPain = defender.feelNoPain
                , modifiers = []
                , keywords = []
                , isLeader = False
                }
            ]
        }
    )

-- | Convert simulation results to test output
resultsToOutput :: AttackResults -> Either Text () -> TestResult
resultsToOutput results validation = case validation of
    Left err -> TestResult False results (Just err) (Just results.timing)
    Right () -> TestResult True results Nothing (Just results.timing)

checkProbability :: Double -> Double -> Double -> Bool
checkProbability actual expected tolerance =
    if actual == expected 
    then True
    else if actual == 0 || expected == 0
         then abs (actual - expected) < tolerance
         else abs ((actual - expected)) < tolerance

stageProbs :: AttackStage -> Distribution AttackSequence -> Map.Map Int Double
stageProbs stage dist = 
    Map.fromList [ (v, fromRational p)
                 | (seq, p) <- Map.toList $ D.probabilities dist
                 , let v = getValue stage seq + getFrozen stage seq ]

validateResults :: CollapsedAttackResults -> [ExpectedProbability] -> Either Text ()
validateResults results expected = 
    forM_ expected $ \e -> validateResult results e
validateResult :: CollapsedAttackResults -> ExpectedProbability -> Either Text ()
validateResult results ExpectedProbability{..} = do
    let stageResults = case stage of
            Hits -> results.hits.probabilities
            Wounds -> results.wounds.probabilities
            FailedSaves -> results.failedSaves.probabilities
            Damage -> results.damage.probabilities
            FeltDmg -> results.feltDamage.probabilities
            ModelsSlain -> results.modelsSlain.probabilities
            _ -> Map.empty
    let actualProb = Map.findWithDefault 0 (createSequence stage value) ( stageResults)
    unless (checkProbability (fromRational actualProb) probability tolerance) $
        Left $ "Probability mismatch for " <> T.pack (show stage) <> 
              " value " <> T.pack (show value) <> 
              ": expected " <> T.pack (show probability) <> 
              " (±" <> T.pack (show tolerance) <> 
              "), got " <> T.pack (show (fromRational actualProb))


unwrapDieResult :: Distribution DieResult -> Distribution AttackSequence
unwrapDieResult dist = D.map (\(DieResult v seq _) ->  seq) dist




-- | Perform rolls for a given stage
performRolls :: AttackStage 
             -> Distribution DieResult 
             -> AttackSequence 
             -> Distribution AttackSequence
performRolls stage dieRoll state = 
    let quant = getValue stage state
    in if quant == 0 
       then D.singleton state
       else let unwrapped = unwrapDieResult dieRoll
                combined = D.repeated unwrapped quant 
                pruned = D.prune combined
                withFrozen = (freezeAll stage state)
            in D.map (<> withFrozen) pruned

-- | Helper to bind performRolls
performRollsBind :: AttackStage 
                 -> Distribution DieResult 
                 -> AttackSequence 
                 -> Distribution AttackSequence
performRollsBind stage dieRoll = performRolls stage dieRoll

-- | Roll for initial number of attacks
rollForAttacks :: AttackProfile -> [ModifierBox] -> Distribution AttackSequence
rollForAttacks profile mods = do
    let attackDist = Dice.roll profile.attacks
        attackDist' = D.map (\x -> DieResult x (createSequence Attacks x) Nothing) attackDist
        modelDist = D.singleton $ createSequence Start (profile.models * profile.gunsPerModel)
    modelDist D.>>== performRollsBind Start attackDist'

-- | Apply modifiers to a die roll
applyModifiers :: Distribution DieResult 
               -> AttackStage 
               -> AttackProfile 
               -> Defender 
               -> [ModifierBox] 
               -> Distribution DieResult
applyModifiers dist stage prof def mods =
    let allMods = prof.modifiers ++ getDefenderModifiers def ++ mods
    in foldr (\(ModifierBox m) d -> modifyRoll m d stage prof def) dist allMods

-- | Roll to hit
rollToHit :: Distribution AttackSequence 
          -> AttackProfile 
          -> Defender 
          -> [ModifierBox] 
          -> Distribution AttackSequence
rollToHit attackDist profile defender mods = do
    let threshold = min 6 $ max 2 $ profile.ballisticSkill
        singleHitRoll = D.d6 D.>>== \x -> 
            if x >= threshold
                then D.singleton $ DieResult x (createSequence Hits 1) (Just True)
                else D.singleton $ DieResult x (createSequence Hits 0) (Just False)
        modifiedRoll = applyModifiers (singleHitRoll) Hits profile defender mods
    attackDist D.>>== performRollsBind Attacks modifiedRoll

-- | Roll to wound
rollToWound :: Distribution AttackSequence 
            -> AttackProfile 
            -> Defender 
            -> [ModifierBox] 
            -> Distribution AttackSequence
rollToWound hitDist profile defender mods = do
    let threshold = getWoundThreshold profile.strength (getHighestToughness defender)
        singleWoundRoll = D.d6 D.>>== \x ->
            if x >= threshold
                then D.singleton $ DieResult x (createSequence Wounds 1) (Just True)
                else D.singleton $ DieResult x (createSequence Wounds 0) (Just False)
        modifiedRoll = applyModifiers singleWoundRoll Wounds profile defender mods
    hitDist D.>>== performRollsBind Hits modifiedRoll

-- | Configuration for attack sequence
data AttackConfig = AttackConfig
    { batchSaveRoll :: Bool
    , batchDamageRoll :: Bool
    }

defaultConfig :: AttackConfig
defaultConfig = AttackConfig True True

-- | Roll for saves
saveRoll :: Defender 
         -> AttackProfile 
         -> [ModifierBox] 
         -> AttackSequence 
         -> Distribution AttackSequence
saveRoll defender profile mods state = do
    let currentProfile = getNextProfile defender (getValue ModelsSlain state)
    case currentProfile of
        Nothing -> D.singleton state
        Just prof | getValue Wounds state == 0 -> D.singleton state
                 | otherwise -> do
            let armorSaveThreshold = prof.armorSave + profile.armorPen
                finalThreshold = maybe armorSaveThreshold (min armorSaveThreshold) prof.invulnSave
                saveRoll' = D.d6 D.>>== \x ->
                    if x >= finalThreshold
                        then D.singleton $ DieResult x (createSequence FailedSaves 0) (Just True)
                        else D.singleton $ DieResult x (createSequence FailedSaves 1) (Just False)
                modifiedRoll = applyModifiers saveRoll' FailedSaves profile defender mods

            if multipleSaveProfiles defender || not ((.batchSaveRoll) defaultConfig)
                then if getValue FailedSaves state > 0
                    then D.singleton state
                    else let nextState = freezeOne Wounds state
                         in D.map (\x -> x.sequence <> nextState) modifiedRoll
                else performRolls Wounds modifiedRoll state

-- | Check if batch damage is allowed
batchDamageAllowed :: Defender -> AttackProfile -> Bool
-- batchDamageAllowed defender profile = 
--     not (hasAnyFnp defender) &&
--     not (Dice.isVariable profile.damage) &&
--     not (multipleWoundProfiles defender) &&
--     (.batchDamageRoll) defaultConfig
batchDamageAllowed _ _ = False

-- | Roll for damage
damageRoll :: Defender 
           -> AttackProfile 
           -> [ModifierBox] 
           -> AttackSequence 
           -> Distribution AttackSequence
damageRoll defender profile mods state = do
    let currentProfile = getNextProfile defender (getValue ModelsSlain state)
    case currentProfile of
        Nothing -> D.singleton state
        Just prof | getValue FailedSaves state == 0 -> D.singleton state
                 | otherwise -> do
            let nextState = freezeOne FailedSaves state
                damageDist = Dice.roll profile.damage
                damageRolls = D.map (\n -> DieResult n (createSequence Damage n) Nothing) damageDist
                modifiedRolls = applyModifiers damageRolls Damage profile defender mods

            if not $ batchDamageAllowed defender profile
                then let unwrapped = unwrapDieResult modifiedRolls
                     in D.map (<> nextState) unwrapped
                else let d = profile.damage.modifier
                         woundCap = prof.wounds
                         d' = until (>= woundCap) (+ d) d
                         damageIgnored = d' - woundCap
                     in performRolls FailedSaves modifiedRolls state 

-- | Roll for Feel No Pain saves
fnpRoll :: Defender 
        -> AttackProfile 
        -> [ModifierBox] 
        -> AttackSequence 
        -> Distribution AttackSequence
fnpRoll defender profile mods state = do
    let currentProfile = getNextProfile defender (getValue ModelsSlain state)
    case currentProfile of
        Nothing -> D.singleton state
        Just prof | getValue Damage state == 0 -> D.singleton state
                 | otherwise -> do
            let fnpThreshold = fromMaybe 7 prof.feelNoPain
                fnpRoll' = D.d6 D.>>== \x ->
                    if x >= fnpThreshold
                        then D.singleton $ DieResult x (createSequence FeltDmg 0) (Just True)
                        else D.singleton $ DieResult x (createSequence FeltDmg 1) (Just False)
                modifiedRoll = applyModifiers fnpRoll' FeltDmg profile defender mods
                result = performRolls Damage modifiedRoll state

            -- Cap damage at wound characteristic
            let woundCap = prof.wounds
                capDmgFelt seq = withValue FeltDmg 
                    (min (getValue FeltDmg seq) woundCap) 
                    seq
            D.map capDmgFelt result

-- | Slay models based on damage dealt
slayModels :: Defender 
           -> AttackProfile 
           -> [ModifierBox] 
           -> AttackSequence 
           -> AttackSequence
slayModels defender profile mods state = do
    let currentProfile = getNextProfile defender (getValue ModelsSlain state)
    case currentProfile of
        Nothing -> state
        Just prof | getValue FeltDmg state == 0 -> state
                 | otherwise -> do
            let defenderWounds = prof.wounds
                feltDmg = getValue FeltDmg state
                numSlain = feltDmg `div` defenderWounds
            if feltDmg >= defenderWounds
                then withValue ModelsSlain 
                        (getValue ModelsSlain state + numSlain) 
                        (freezeAll FeltDmg state)
                else state

-- | Process the damage sequence with timing information
doDmgSequence :: Distribution AttackSequence 
              -> Defender 
              -> AttackProfile 
              -> [ModifierBox] 
              -> TimingInfo 
              -> Distribution AttackSequence
doDmgSequence woundDist defender profile mods _ = do
    let saveRoll' = D.liftM (saveRoll defender profile mods)
        damageRoll' = D.liftM (damageRoll defender profile mods)
        fnpRoll' = D.liftM (fnpRoll defender profile mods)
        slayModels' = slayModels defender profile mods

        go current = do
            let saved = (saveRoll' current) 
                damaged = (damageRoll' saved)
                feltDmg = (fnpRoll' damaged)
                slain = (D.map slayModels' feltDmg)
                next = (slain)
                       
            if next == current
                then current
                else go next

    go woundDist

-- | Main simulation function
simulateAttacks :: AttackProfile 
                -> Defender 
                -> [ModifierBox] 
                -> AttackResults
simulateAttacks profile defender mods =
    let -- Apply modifiers to profile and defender
        allMods =  profile.modifiers ++ getDefenderModifiers defender ++ mods
        profile' = foldr (\(ModifierBox m) p -> modifyAttacker m p defender) profile allMods
        defender' = foldr (\(ModifierBox m) d -> modifyDefender m profile d) defender allMods

        -- Initial attack rolls
        attackDist = rollForAttacks profile' mods & D.prune
        attackDist' = D.prune attackDist

        -- Hit rolls
        hitDist = rollToHit attackDist' profile' defender' ( mods) & D.prune
        hitDist' = D.map clearFrozen hitDist 

        -- Wound rolls
        woundDist = rollToWound hitDist' profile' defender' mods & D.prune
        woundDist' = D.map clearFrozen woundDist 

        -- Initialize timing info
        timing = TimingInfo 0 0 0 0 0 0 0 0

        -- Process damage sequence
        finalDist = doDmgSequence woundDist' defender' profile' mods timing
    in AttackResults
        { attacks = attackDist
        , hits = hitDist
        , wounds = woundDist
        , damage = finalDist
        , timing = timing
        } 