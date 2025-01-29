from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional, Union, Protocol, Dict
from probability.dice import DiceFormula, D6, D3
from copy import deepcopy
from probability.distribution import Distribution

@dataclass
class DieResult:
    value: int
    sequence: 'AttackSequence'
    passes_check: Union[bool, None] = None
    is_critical: bool = False


    def __str__(self) -> str:
        if self.sequence is None:
            return f"Roll({self.value})"
        return f"Roll({self.value}, {self.sequence})"

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, DieResult):
            return False
        return self.value == other.value
    
    def __hash__(self) -> int:
        return hash(self.value)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, DieResult):
            return False
        return self.value < other.value
    
    def __add__(self, other: object) -> 'DieResult':
        if not isinstance(other, DieResult):
            return NotImplemented
        return DieResult(self.value + other.value, self.sequence + other.sequence)

class AttackStage(Enum):
    START = auto()
    ATTACKS = auto()
    HITS = auto()
    WOUNDS = auto()
    MORTAL_WOUNDS = auto()
    MORTAL_DAMAGE = auto()
    FAILED_SAVES = auto()
    PROCESSING_FAILED_SAVES = auto()
    DAMAGE = auto()
    FELT_DMG = auto()
    MODELS_SLAIN = auto()
    COMPLETE = auto()
    
    def __lt__(self, other: 'AttackStage') -> bool:
        if not isinstance(other, AttackStage):
            return NotImplemented
        return self.value < other.value

    def __hash__(self) -> int:
        return self.value

class Modifier(Protocol):
    def modify_attacker(self, profile: 'AttackProfile', defender: 'Defender') -> 'AttackProfile':
        """Modify the attack profile before rolling"""
        return profile
        
    def modify_defender(self, profile: 'AttackProfile', defender: 'Defender') -> 'Defender':
        """Modify the defender profile before rolling"""
        return defender
        
    def modify_roll(self, value: 'Distribution[DieResult]', stage: 'AttackStage', 
                   profile: 'AttackProfile', defender: 'Defender', 
                   prev_sequence: Optional['AttackSequence'] = None) -> 'Distribution[DieResult]':
        """Modify a roll result"""
        return value

   
@dataclass
class AttackProfile:
    name: str
    models: int
    guns_per_model: int
    attacks: DiceFormula
    ballistic_skill: int
    strength: int
    armor_pen: int
    damage: DiceFormula
    keywords: List[str]
    modifiers: List[Modifier]


@dataclass
class DefenderProfile:
    name: str
    models: int
    toughness: int
    armor_save: int
    invuln_save: Optional[int]
    wounds: int
    keywords: List[str]
    feel_no_pain: Optional[int]
    is_leader: bool
    modifiers: List[Modifier]

    def total_wounds(self) -> int:
        return self.wounds * self.models

@dataclass
class Attacker:
    name: str
    profiles: List[AttackProfile]

    def get_modifiers(self) -> List[Modifier]:
        return [modifier for profile in self.profiles for modifier in profile.modifiers]

@dataclass
class Defender:
    name: str
    profiles: List[DefenderProfile]

    def get_modifiers(self) -> List[Modifier]:
        return [modifier for profile in self.profiles for modifier in profile.modifiers]

    def multiple_save_profiles(self) -> bool:       
        first_armor_save = self.profiles[0].armor_save
        first_invuln_save = self.profiles[0].invuln_save
        for profile in self.profiles[1:]:
            if profile.armor_save != first_armor_save or profile.invuln_save != first_invuln_save:
                return True
        return False

    def has_any_fnp(self) -> bool:
        return any(p.feel_no_pain is not None for p in self.profiles)

    def multiple_wound_profiles(self) -> bool:
        first_wound_profile = self.profiles[0].wounds
        for profile in self.profiles[1:]:
            if profile.wounds != first_wound_profile:
                return True
        return False

    def get_highest_toughness(self) -> int:
        """Get the highest toughness in the unit, prioritizing non-leader models."""
        non_leader_toughness = [p.toughness for p in self.profiles if not p.is_leader]
        if non_leader_toughness:
            return max(non_leader_toughness)
        
        # If we only have leader models, use their toughness
        leader_toughness = [p.toughness for p in self.profiles if p.is_leader]
        if not leader_toughness:
            raise ValueError(f"Unit {self.name} has no models with toughness values")
        return max(leader_toughness)
    
    def get_next_profile(self, num_models_slain: int) -> Optional[DefenderProfile]:
        """Get the next profile to allocate wounds to"""
        if not self.profiles:
            return None
        index = 0
        current_profile = self.profiles[index]
        remaining_models = num_models_slain
        while remaining_models >= current_profile.models:
            remaining_models -= current_profile.models
            index += 1
            if index >= len(self.profiles):
                return None
            current_profile = self.profiles[index]
        return current_profile

    def get_wound_cap(self, num_models_slain: int) -> int:
        if not self.profiles:
            return 0
        index = 0
        wound_cap = 0
        current_profile = self.profiles[index]
        while num_models_slain >= current_profile.models:
            wound_cap += current_profile.wounds * num_models_slain
            num_models_slain -= current_profile.models
            index += 1
            if index >= len(self.profiles):
                return wound_cap + current_profile.wounds
            current_profile = self.profiles[index]
        return wound_cap + current_profile.wounds

    def total_wounds(self) -> int:
        return sum(p.total_wounds() for p in self.profiles)
    

class AttackSequence:
    # Values for each stage
    start: int = 0
    attacks: int = 0
    hits: int = 0
    wounds: int = 0
    mortal_wounds: int = 0
    mortal_damage: int = 0
    failed_saves: int = 0
    processing_failed_saves: int = 0
    damage: int = 0
    felt_dmg: int = 0
    models_slain: int = 0
    complete: int = 0

    # Frozen values for each stage
    frozen_start: int = 0
    frozen_attacks: int = 0
    frozen_hits: int = 0
    frozen_wounds: int = 0
    frozen_mortal_wounds: int = 0
    frozen_mortal_damage: int = 0
    frozen_failed_saves: int = 0
    frozen_processing_failed_saves: int = 0
    frozen_damage: int = 0
    frozen_felt_dmg: int = 0
    frozen_models_slain: int = 0
    frozen_complete: int = 0

    hash_val: int = 0

    def __init__(self, start=0, attacks=0, hits=0, wounds=0, mortal_wounds=0, mortal_damage=0, failed_saves=0, processing_failed_saves=0, damage=0, felt_dmg=0, models_slain=0, complete=0, frozen_start=0, frozen_attacks=0, frozen_hits=0, frozen_wounds=0, frozen_mortal_wounds=0, frozen_mortal_damage=0, frozen_failed_saves=0, frozen_processing_failed_saves=0, frozen_damage=0, frozen_felt_dmg=0, frozen_models_slain=0, frozen_complete=0):
        self.start = start
        self.attacks = attacks
        self.hits = hits
        self.wounds = wounds
        self.mortal_wounds = mortal_wounds
        self.mortal_damage = mortal_damage
        self.failed_saves = failed_saves
        self.processing_failed_saves = processing_failed_saves
        self.damage = damage
        self.felt_dmg = felt_dmg
        self.models_slain = models_slain
        self.complete = complete
        self.frozen_start = frozen_start
        self.frozen_attacks = frozen_attacks
        self.frozen_hits = frozen_hits
        self.frozen_wounds = frozen_wounds
        self.frozen_mortal_wounds = frozen_mortal_wounds
        self.frozen_mortal_damage = frozen_mortal_damage
        self.frozen_failed_saves = frozen_failed_saves
        self.frozen_processing_failed_saves = frozen_processing_failed_saves
        self.frozen_damage = frozen_damage
        self.frozen_felt_dmg = frozen_felt_dmg
        self.frozen_models_slain = frozen_models_slain
        self.frozen_complete = frozen_complete

    @classmethod
    def create(cls, value: int, stage: AttackStage) -> 'AttackSequence':
        return cls(**{stage.name.lower(): value})

    def _get_stage_attr(self, stage: AttackStage) -> str:
        return stage.name.lower()

    def _get_frozen_attr(self, stage: AttackStage) -> str:
        return f"frozen_{stage.name.lower()}"

    def clear_frozen(self) -> 'AttackSequence':
        """Clear all frozen values"""
        return AttackSequence(
            start=self.start,
            attacks=self.attacks,
            hits=self.hits,
            wounds=self.wounds,
            mortal_wounds=self.mortal_wounds,
            mortal_damage=self.mortal_damage,
            failed_saves=self.failed_saves,
            processing_failed_saves=self.processing_failed_saves,
            damage=self.damage,
            felt_dmg=self.felt_dmg,
            models_slain=self.models_slain,
            complete=self.complete
        )

    def freeze_all(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze a value at a stage"""
        return self.with_frozen(stage, self.get_value(stage) + self.get_frozen(stage)).with_value(stage, 0)

    def freeze_quant(self, stage: AttackStage, quant: int) -> 'AttackSequence':
        """Freeze a quantity at a stage"""
        if quant > self.get_value(stage):
            raise ValueError(f"Cannot freeze more than the value at stage {stage}")
        return self.with_frozen(stage, self.get_frozen(stage) + quant).with_value(stage, self.get_value(stage) - quant)
    
    def void_quant(self, stage: AttackStage, quant: int) -> 'AttackSequence':
        """deduct and do not freeze a quantity at a stage"""
        if quant > self.get_value(stage):
            raise ValueError(f"Cannot void more than the value at stage {stage}")
        return self.with_value(stage, self.get_value(stage) - quant)

    def zero_out(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a value at a stage"""
        return self.with_value(stage, 0).with_frozen(stage, 0)

    def zero_frozen(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a frozen value at a stage"""
        return self.with_frozen(stage, 0)

    def freeze_one(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze one value at a stage"""
        return self.freeze_quant(stage, min(self.get_value(stage), 1))

    def get_value(self, stage: AttackStage, default: int = 0) -> int:
        """Get the value for a stage"""
        match stage:
            case AttackStage.START:
                return self.start
            case AttackStage.ATTACKS:
                return self.attacks
            case AttackStage.HITS:
                return self.hits
            case AttackStage.WOUNDS:
                return self.wounds
            case AttackStage.MORTAL_WOUNDS:
                return self.mortal_wounds
            case AttackStage.MORTAL_DAMAGE:
                return self.mortal_damage
            case AttackStage.FAILED_SAVES:
                return self.failed_saves
            case AttackStage.PROCESSING_FAILED_SAVES:
                return self.processing_failed_saves
            case AttackStage.DAMAGE:
                return self.damage
            case AttackStage.FELT_DMG:
                return self.felt_dmg
            case AttackStage.MODELS_SLAIN:
                return self.models_slain
            case AttackStage.COMPLETE:
                return self.complete
        return default

    def get_frozen(self, stage: AttackStage, default: int = 0) -> int:
        """Get the frozen value for a stage"""
        match stage:
            case AttackStage.START:
                return self.frozen_start
            case AttackStage.ATTACKS:
                return self.frozen_attacks
            case AttackStage.HITS:
                return self.frozen_hits
            case AttackStage.WOUNDS:
                return self.frozen_wounds
            case AttackStage.MORTAL_WOUNDS:
                return self.frozen_mortal_wounds
            case AttackStage.MORTAL_DAMAGE:
                return self.frozen_mortal_damage
            case AttackStage.FAILED_SAVES:
                return self.frozen_failed_saves
            case AttackStage.PROCESSING_FAILED_SAVES:
                return self.frozen_processing_failed_saves
            case AttackStage.DAMAGE:
                return self.frozen_damage
            case AttackStage.FELT_DMG:
                return self.frozen_felt_dmg
            case AttackStage.MODELS_SLAIN:
                return self.frozen_models_slain
            case AttackStage.COMPLETE:
                return self.frozen_complete
        return default

    def with_value(self, stage: AttackStage, value: int) -> 'AttackSequence':
        """Add or update a value for a stage"""
        return AttackSequence(
            start=self.start if stage != AttackStage.START else value,
            attacks=self.attacks if stage != AttackStage.ATTACKS else value,
            hits=self.hits if stage != AttackStage.HITS else value,
            wounds=self.wounds if stage != AttackStage.WOUNDS else value,
            mortal_wounds=self.mortal_wounds if stage != AttackStage.MORTAL_WOUNDS else value,
            mortal_damage=self.mortal_damage if stage != AttackStage.MORTAL_DAMAGE else value,
            failed_saves=self.failed_saves if stage != AttackStage.FAILED_SAVES else value,
            processing_failed_saves=self.processing_failed_saves if stage != AttackStage.PROCESSING_FAILED_SAVES else value,
            damage=self.damage if stage != AttackStage.DAMAGE else value,
            felt_dmg=self.felt_dmg if stage != AttackStage.FELT_DMG else value,
            models_slain=self.models_slain if stage != AttackStage.MODELS_SLAIN else value,
            complete=self.complete if stage != AttackStage.COMPLETE else value,
            frozen_start=self.frozen_start,
            frozen_attacks=self.frozen_attacks,
            frozen_hits=self.frozen_hits,
            frozen_wounds=self.frozen_wounds,
            frozen_mortal_wounds=self.frozen_mortal_wounds,
            frozen_mortal_damage=self.frozen_mortal_damage,
            frozen_failed_saves=self.frozen_failed_saves,
            frozen_processing_failed_saves=self.frozen_processing_failed_saves,
            frozen_damage=self.frozen_damage,
            frozen_felt_dmg=self.frozen_felt_dmg,
            frozen_models_slain=self.frozen_models_slain,
            frozen_complete=self.frozen_complete
        )

    def with_frozen(self, stage: AttackStage, value: int) -> 'AttackSequence':
        """Add or update a frozen value for a stage"""
        return AttackSequence(
            start=self.start,
            attacks=self.attacks,
            hits=self.hits,
            wounds=self.wounds,
            mortal_wounds=self.mortal_wounds,
            mortal_damage=self.mortal_damage,
            failed_saves=self.failed_saves,
            processing_failed_saves=self.processing_failed_saves,
            damage=self.damage,
            felt_dmg=self.felt_dmg,
            models_slain=self.models_slain,
            complete=self.complete,
            frozen_start=self.frozen_start if stage != AttackStage.START else value,
            frozen_attacks=self.frozen_attacks if stage != AttackStage.ATTACKS else value,
            frozen_hits=self.frozen_hits if stage != AttackStage.HITS else value,
            frozen_wounds=self.frozen_wounds if stage != AttackStage.WOUNDS else value,
            frozen_mortal_wounds=self.frozen_mortal_wounds if stage != AttackStage.MORTAL_WOUNDS else value,
            frozen_mortal_damage=self.frozen_mortal_damage if stage != AttackStage.MORTAL_DAMAGE else value,
            frozen_failed_saves=self.frozen_failed_saves if stage != AttackStage.FAILED_SAVES else value,
            frozen_processing_failed_saves=self.frozen_processing_failed_saves if stage != AttackStage.PROCESSING_FAILED_SAVES else value,
            frozen_damage=self.frozen_damage if stage != AttackStage.DAMAGE else value,
            frozen_felt_dmg=self.frozen_felt_dmg if stage != AttackStage.FELT_DMG else value,
            frozen_models_slain=self.frozen_models_slain if stage != AttackStage.MODELS_SLAIN else value,
            frozen_complete=self.frozen_complete if stage != AttackStage.COMPLETE else value
        )

    def __str__(self) -> str:
        """Print stages in order, omitting zeros unless all values are zero."""
        parts = []
        all_zeros = all(self.get_value(stage) == 0 and self.get_frozen(stage) == 0 
                       for stage in AttackStage)
        
        for stage in sorted(AttackStage):
            val = self.get_value(stage)
            frozen_val = self.get_frozen(stage)
            
            if val != 0 or frozen_val != 0 or all_zeros:
                if frozen_val != 0:
                    parts.append(f"{stage.name}:{val}+{frozen_val}*")
                else:
                    parts.append(f"{stage.name}:{val}")
        
        return f"[{', '.join(parts)}]"

    def __add__(self, other: 'AttackSequence') -> 'AttackSequence':
        """Combine two sequences by adding their values."""
        return AttackSequence(
            start=self.start + other.start,
            attacks=self.attacks + other.attacks,
            hits=self.hits + other.hits,
            wounds=self.wounds + other.wounds,
            mortal_wounds=self.mortal_wounds + other.mortal_wounds,
            mortal_damage=self.mortal_damage + other.mortal_damage,
            failed_saves=self.failed_saves + other.failed_saves,
            processing_failed_saves=self.processing_failed_saves + other.processing_failed_saves,
            damage=self.damage + other.damage,
            felt_dmg=self.felt_dmg + other.felt_dmg,
            models_slain=self.models_slain + other.models_slain,
            complete=self.complete + other.complete,
            frozen_start=self.frozen_start + other.frozen_start,
            frozen_attacks=self.frozen_attacks + other.frozen_attacks,
            frozen_hits=self.frozen_hits + other.frozen_hits,
            frozen_wounds=self.frozen_wounds + other.frozen_wounds,
            frozen_mortal_wounds=self.frozen_mortal_wounds + other.frozen_mortal_wounds,
            frozen_mortal_damage=self.frozen_mortal_damage + other.frozen_mortal_damage,
            frozen_failed_saves=self.frozen_failed_saves + other.frozen_failed_saves,
            frozen_processing_failed_saves=self.frozen_processing_failed_saves + other.frozen_processing_failed_saves,
            frozen_damage=self.frozen_damage + other.frozen_damage,
            frozen_felt_dmg=self.frozen_felt_dmg + other.frozen_felt_dmg,
            frozen_models_slain=self.frozen_models_slain + other.frozen_models_slain,
            frozen_complete=self.frozen_complete + other.frozen_complete
        )

    def do_hash(self) -> int:
        # Combine all values into a single hash using prime multipliers
        # Use negative values for frozen to distinguish them
        h = 0
        h = h * 31 + self.start
        h = h * 31 + self.attacks
        h = h * 31 + self.hits
        h = h * 31 + self.wounds
        h = h * 31 + self.mortal_wounds
        h = h * 31 + self.mortal_damage
        h = h * 31 + self.failed_saves
        h = h * 31 + self.processing_failed_saves
        h = h * 31 + self.damage
        h = h * 31 + self.felt_dmg
        h = h * 31 + self.models_slain
        h = h * 31 + self.complete
        h = h * 31 - self.frozen_start
        h = h * 31 - self.frozen_attacks
        h = h * 31 - self.frozen_hits
        h = h * 31 - self.frozen_wounds
        h = h * 31 - self.frozen_mortal_wounds
        h = h * 31 - self.frozen_mortal_damage
        h = h * 31 - self.frozen_failed_saves
        h = h * 31 - self.frozen_processing_failed_saves
        h = h * 31 - self.frozen_damage
        h = h * 31 - self.frozen_felt_dmg
        h = h * 31 - self.frozen_models_slain
        h = h * 31 - self.frozen_complete
        return h

    def __hash__(self) -> int:
        if self.hash_val == 0:
            self.hash_val = self.do_hash()
        return self.hash_val

    def __eq__(self, other: object) -> bool:
        return self.__hash__() == other.__hash__()

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, AttackSequence):
            return NotImplemented
            
        for stage in sorted(AttackStage):
            self_total = (getattr(self, self._get_stage_attr(stage)) + 
                         getattr(self, self._get_frozen_attr(stage)))
            other_total = (getattr(other, self._get_stage_attr(stage)) + 
                          getattr(other, self._get_frozen_attr(stage)))
            if self_total != other_total:
                return self_total < other_total
                
        return False  # Equal sequences

    def with_value_mut(self, stage: AttackStage, value: int) -> 'AttackSequence':
        """Mutate a value for a stage in place and update hash"""
        match stage:
            case AttackStage.START:
                self.start = value
            case AttackStage.ATTACKS:
                self.attacks = value
            case AttackStage.HITS:
                self.hits = value
            case AttackStage.WOUNDS:
                self.wounds = value
            case AttackStage.MORTAL_WOUNDS:
                self.mortal_wounds = value
            case AttackStage.MORTAL_DAMAGE:
                self.mortal_damage = value
            case AttackStage.FAILED_SAVES:
                self.failed_saves = value
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.processing_failed_saves = value
            case AttackStage.DAMAGE:
                self.damage = value
            case AttackStage.FELT_DMG:
                self.felt_dmg = value
            case AttackStage.MODELS_SLAIN:
                self.models_slain = value
            case AttackStage.COMPLETE:
                self.complete = value

        # Update hash value since we modified the sequence
        self.hash_val = 0
        return self.validate_non_negative()

    def clear_frozen_mut(self) -> 'AttackSequence':
        """Clear all frozen values in place"""
        self.frozen_start = 0
        self.frozen_attacks = 0
        self.frozen_hits = 0
        self.frozen_wounds = 0
        self.frozen_mortal_wounds = 0
        self.frozen_mortal_damage = 0
        self.frozen_failed_saves = 0
        self.frozen_processing_failed_saves = 0
        self.frozen_damage = 0
        self.frozen_felt_dmg = 0
        self.frozen_models_slain = 0
        self.frozen_complete = 0
        self.hash_val = 0
        return self.validate_non_negative()

    def freeze_all_mut(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze a value at a stage in place"""
        match stage:
            case AttackStage.START:
                self.frozen_start += self.start
                self.start = 0
            case AttackStage.ATTACKS:
                self.frozen_attacks += self.attacks
                self.attacks = 0
            case AttackStage.HITS:
                self.frozen_hits += self.hits
                self.hits = 0
            case AttackStage.WOUNDS:
                self.frozen_wounds += self.wounds
                self.wounds = 0
            case AttackStage.MORTAL_WOUNDS:
                self.frozen_mortal_wounds += self.mortal_wounds
                self.mortal_wounds = 0
            case AttackStage.MORTAL_DAMAGE:
                self.frozen_mortal_damage += self.mortal_damage
                self.mortal_damage = 0
            case AttackStage.FAILED_SAVES:
                self.frozen_failed_saves += self.failed_saves
                self.failed_saves = 0
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.frozen_processing_failed_saves += self.processing_failed_saves
                self.processing_failed_saves = 0
            case AttackStage.DAMAGE:
                self.frozen_damage += self.damage
                self.damage = 0
            case AttackStage.FELT_DMG:
                self.frozen_felt_dmg += self.felt_dmg
                self.felt_dmg = 0
            case AttackStage.MODELS_SLAIN:
                self.frozen_models_slain += self.models_slain
                self.models_slain = 0
            case AttackStage.COMPLETE:
                self.frozen_complete += self.complete
                self.complete = 0
        self.hash_val = 0
        return self.validate_non_negative()

    def freeze_quant_mut(self, stage: AttackStage, quant: int) -> 'AttackSequence':
        """Freeze a quantity at a stage in place"""
        current_val = self.get_value(stage)
        if quant > current_val:
            raise ValueError(f"Cannot freeze more than the value at stage {stage}")
        
        match stage:
            case AttackStage.START:
                self.frozen_start += quant
                self.start -= quant
            case AttackStage.ATTACKS:
                self.frozen_attacks += quant
                self.attacks -= quant
            case AttackStage.HITS:
                self.frozen_hits += quant
                self.hits -= quant
            case AttackStage.WOUNDS:
                self.frozen_wounds += quant
                self.wounds -= quant
            case AttackStage.MORTAL_WOUNDS:
                self.frozen_mortal_wounds += quant
                self.mortal_wounds -= quant
            case AttackStage.MORTAL_DAMAGE:
                self.frozen_mortal_damage += quant
                self.mortal_damage -= quant
            case AttackStage.FAILED_SAVES:
                self.frozen_failed_saves += quant
                self.failed_saves -= quant
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.frozen_processing_failed_saves += quant
                self.processing_failed_saves -= quant
            case AttackStage.DAMAGE:
                self.frozen_damage += quant
                self.damage -= quant
            case AttackStage.FELT_DMG:
                self.frozen_felt_dmg += quant
                self.felt_dmg -= quant
            case AttackStage.MODELS_SLAIN:
                self.frozen_models_slain += quant
                self.models_slain -= quant
            case AttackStage.COMPLETE:
                self.frozen_complete += quant
                self.complete -= quant
        self.hash_val = 0
        return self.validate_non_negative()
    
    def void_quant_mut(self, stage: AttackStage, quant: int) -> 'AttackSequence':
        """Deduct and do not freeze a quantity at a stage in place"""
        current_val = self.get_value(stage)
        if quant > current_val:
            raise ValueError(f"Cannot void more than the value at stage {stage}")
        
        match stage:
            case AttackStage.START:
                self.start -= quant
            case AttackStage.ATTACKS:
                self.attacks -= quant
            case AttackStage.HITS:
                self.hits -= quant
            case AttackStage.WOUNDS:
                self.wounds -= quant
            case AttackStage.MORTAL_WOUNDS:
                self.mortal_wounds -= quant
            case AttackStage.MORTAL_DAMAGE:
                self.mortal_damage -= quant
            case AttackStage.FAILED_SAVES:
                self.failed_saves -= quant
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.processing_failed_saves -= quant
            case AttackStage.DAMAGE:
                self.damage -= quant
            case AttackStage.FELT_DMG:
                self.felt_dmg -= quant
            case AttackStage.MODELS_SLAIN:
                self.models_slain -= quant
            case AttackStage.COMPLETE:
                self.complete -= quant
        self.hash_val = 0
        return self.validate_non_negative()

    def zero_out_mut(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a value at a stage in place"""
        match stage:
            case AttackStage.START:
                self.start = 0
                self.frozen_start = 0
            case AttackStage.ATTACKS:
                self.attacks = 0
                self.frozen_attacks = 0
            case AttackStage.HITS:
                self.hits = 0
                self.frozen_hits = 0
            case AttackStage.WOUNDS:
                self.wounds = 0
                self.frozen_wounds = 0
            case AttackStage.MORTAL_WOUNDS:
                self.mortal_wounds = 0
                self.frozen_mortal_wounds = 0
            case AttackStage.MORTAL_DAMAGE:
                self.mortal_damage = 0
                self.frozen_mortal_damage = 0
            case AttackStage.FAILED_SAVES:
                self.failed_saves = 0
                self.frozen_failed_saves = 0
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.processing_failed_saves = 0
                self.frozen_processing_failed_saves = 0
            case AttackStage.DAMAGE:
                self.damage = 0
                self.frozen_damage = 0
            case AttackStage.FELT_DMG:
                self.felt_dmg = 0
                self.frozen_felt_dmg = 0
            case AttackStage.MODELS_SLAIN:
                self.models_slain = 0
                self.frozen_models_slain = 0
            case AttackStage.COMPLETE:
                self.complete = 0
                self.frozen_complete = 0
        self.hash_val = 0
        return self.validate_non_negative()

    def zero_frozen_mut(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a frozen value at a stage in place"""
        match stage:
            case AttackStage.START:
                self.frozen_start = 0
            case AttackStage.ATTACKS:
                self.frozen_attacks = 0
            case AttackStage.HITS:
                self.frozen_hits = 0
            case AttackStage.WOUNDS:
                self.frozen_wounds = 0
            case AttackStage.MORTAL_WOUNDS:
                self.frozen_mortal_wounds = 0
            case AttackStage.MORTAL_DAMAGE:
                self.frozen_mortal_damage = 0
            case AttackStage.FAILED_SAVES:
                self.frozen_failed_saves = 0
            case AttackStage.PROCESSING_FAILED_SAVES:
                self.frozen_processing_failed_saves = 0
            case AttackStage.DAMAGE:
                self.frozen_damage = 0
            case AttackStage.FELT_DMG:
                self.frozen_felt_dmg = 0
            case AttackStage.MODELS_SLAIN:
                self.frozen_models_slain = 0
            case AttackStage.COMPLETE:
                self.frozen_complete = 0
        self.hash_val = 0
        return self.validate_non_negative()

    def freeze_one_mut(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze one value at a stage in place"""
        return self.freeze_quant_mut(stage, min(self.get_value(stage), 1)).validate_non_negative()

    def copy(self) -> 'AttackSequence':
        return AttackSequence(
            start=self.start,
            attacks=self.attacks,
            hits=self.hits,
            wounds=self.wounds,
            mortal_wounds=self.mortal_wounds,
            mortal_damage=self.mortal_damage,
            failed_saves=self.failed_saves,
            processing_failed_saves=self.processing_failed_saves,
            damage=self.damage,
            felt_dmg=self.felt_dmg,
            models_slain=self.models_slain,
            complete=self.complete
        )

    def validate_non_negative(self) -> 'AttackSequence':
        # """Validate that all values in the sequence are non-negative"""
        # for stage in AttackStage:
        #     val = self.get_value(stage)
        #     frozen_val = self.get_frozen(stage)
        #     if val < 0:
        #         raise ValueError(f"Negative value {val} at stage {stage}")
        #     if frozen_val < 0:
        #         raise ValueError(f"Negative frozen value {frozen_val} at stage {stage}")
        return self

# Example modifiers
@dataclass
class RerollOnes:
    """Reroll 1s for a specific stage"""
    stage: AttackStage

    def modify_roll(self, value: Distribution[DieResult], stage: 'AttackStage', 
                   profile: 'AttackProfile', defender: 'DefenderProfile',
                   prev_sequence: Optional['AttackSequence'] = None) -> Distribution[DieResult]:
        if stage != self.stage:
            return value

        def reroll_one(result: DieResult) -> Distribution[DieResult]:
            if result.value == 1:
                return value
            return Distribution.singleton(result)
        
        return value.bind(reroll_one)

@dataclass
class RerollAllFails(Modifier):

    stage: AttackStage

    def modify_roll(self, value: Distribution[DieResult], stage: 'AttackStage', 
                   profile: 'AttackProfile', defender: 'DefenderProfile',
                   prev_sequence: Optional['AttackSequence'] = None) -> Distribution[DieResult]:
        if stage != self.stage:
            return value
        
        def reroll_fail(result: DieResult) -> Distribution[DieResult]:
            if result.passes_check:
                return Distribution.singleton(result)
            return value

        result = value.bind(reroll_fail)
        return result

@dataclass
class LethalHits(Modifier):
    """Automatically wound on hit rolls of 6"""

    def crit_hit_auto_wound(self, result: DieResult) -> DieResult:
        if result.is_critical:
            return DieResult(result.value, AttackSequence(wounds=1, frozen_hits=1), True)
        return result
    
    def modify_roll(self, value: Distribution[DieResult], stage: AttackStage, 
                   profile: AttackProfile, defender: DefenderProfile,
                   prev_sequence: Optional[AttackSequence] = None) -> Distribution[DieResult]:
        
        if stage != AttackStage.HITS:
            return value

        result = value.map(self.crit_hit_auto_wound)
        return result

@dataclass
class DevastatingWounds(Modifier):
    """On wound rolls of 6, ignore armor saves"""

    def crit_wound_mortal_wound(self, result: DieResult) -> DieResult:
        if result.is_critical:
            return DieResult(result.value, AttackSequence(mortal_wounds=1, frozen_wounds=1), True)
        return result
    
    def modify_roll(self, value: Distribution[DieResult], stage: AttackStage, 
                   profile: AttackProfile, defender: DefenderProfile,
                   prev_sequence: Optional[AttackSequence] = None) -> Distribution[DieResult]:
        
        if stage != AttackStage.WOUNDS:
            return value
            
        result = value.map(self.crit_wound_mortal_wound)
        return result

SPACE_MARINE_PROFILE = DefenderProfile(
    name="Space Marine",
    models=1,
    toughness=4,
    armor_save=3,
    invuln_save=None,
    wounds=2,
    feel_no_pain=None,
    modifiers=[],
    keywords=[],
    is_leader=False
)

SPACE_MARINE = Defender(
    name="Space Marine",
    profiles=[SPACE_MARINE_PROFILE]
)

GUARDSMAN_PROFILE = DefenderProfile(
    name="Guardsman",
    models=5,
    toughness=3,
    armor_save=4,
    invuln_save=None,
    wounds=1,
    feel_no_pain=5,
    modifiers=[],
    keywords=[],
    is_leader=False
)

GUARDSMAN = Defender(
    name="Guardsman",
    profiles=[GUARDSMAN_PROFILE]
)

# Example profiles
BOLTER = AttackProfile(
    name="Bolter",
    models=1,
    guns_per_model=1,
    attacks=DiceFormula.constant(2),  # Fixed 2 shots
    ballistic_skill=3,
    strength=4,
    armor_pen=0,
    damage=DiceFormula.constant(1),  # Fixed 1 damage
    modifiers=[],
    keywords=[]
)

PLASMA_CANNON = AttackProfile(
    name="Plasma Cannon",
    models=1,
    guns_per_model=1,
    attacks=D6,  # D6 shots
    ballistic_skill=3,
    strength=7,
    armor_pen=2,
    damage=DiceFormula.constant(2),  # Fixed 2 damage
    modifiers=[],
    keywords=[]
)

THUNDER_HAMMER = AttackProfile(
    name="Thunder Hammer",
    models=1,
    guns_per_model=1,
    attacks=DiceFormula.constant(4),  # Fixed 4 attacks
    ballistic_skill=4,
    strength=8,
    armor_pen=2,
    damage=DiceFormula(1, 3, 3),  # D3+3 damage
    modifiers=[],
    keywords=[]
) 

LASGUN = AttackProfile(
    name="Lasgun",
    models=1,
    guns_per_model=1,
    attacks=DiceFormula.constant(2),
    ballistic_skill=4,
    strength=3,
    armor_pen=0,
    damage=DiceFormula.constant(1),
    modifiers=[],
    keywords=[]
)

LASGUN_BARRAGE = AttackProfile(
    name="Lasgun Barrage",
    models=1,
    guns_per_model=1,
    attacks=DiceFormula.constant(20),
    ballistic_skill=4,
    strength=3,
    armor_pen=0,
    damage=DiceFormula.constant(2),
    modifiers=[RerollAllFails(AttackStage.HITS)],
    keywords=[]
)



