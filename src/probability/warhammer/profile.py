from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional, Union, Protocol, Dict
from ..dice import DiceFormula, D6, D3
from copy import deepcopy
from ..distribution import Distribution

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
    
@dataclass(frozen=True)
class ModelState:
    """Track state of current model being wounded"""
    profile: DefenderProfile
    wounds_remaining: int

@dataclass(frozen=True) 
class AttackSequence:
    values: Dict[AttackStage, int]
    frozen: Dict[AttackStage, int] = field(default_factory=dict)
    hash_val: int = 0

    def __post_init__(self):
        object.__setattr__(self, 'hash_val', self.do_hash())


    @classmethod
    def create(cls, value: int, stage: AttackStage) -> 'AttackSequence':
        """Create a new sequence with a single value at the given stage"""
        return cls(values={stage: value})

    def clear_frozen(self) -> 'AttackSequence':
        """Clear all frozen values"""
        return AttackSequence(values=self.values, frozen={})

    def freeze_all(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze a value at a stage"""
        new_values = self.values.copy()
        new_frozen = self.frozen.copy()
        new_frozen[stage] = self.get_value(stage) + self.get_frozen(stage)
        new_values[stage] = 0
        return AttackSequence(values=new_values, frozen=new_frozen)

    def freeze_quant(self, stage: AttackStage, quant: int) -> 'AttackSequence':
        """Freeze a value at a stage"""
        new_values = self.values.copy()
        new_frozen = self.frozen.copy()
        if quant > self.get_value(stage):
            raise ValueError(f"Cannot freeze more than the value at stage {stage}")
        new_frozen[stage] = self.get_frozen(stage) + quant
        new_values[stage] = self.get_value(stage) - quant
        return AttackSequence(values=new_values, frozen=new_frozen)

    def zero_out(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a value at a stage"""
        new_values = self.values.copy()
        new_frozen = self.frozen.copy()
        new_values[stage] = 0
        new_frozen[stage] = 0
        return AttackSequence(values=new_values, frozen=new_frozen)

    def zero_frozen(self, stage: AttackStage) -> 'AttackSequence':
        """Zero out a frozen value at a stage"""
        new_frozen = self.frozen.copy()
        new_frozen[stage] = 0
        return AttackSequence(values=self.values, frozen=new_frozen)

    def freeze_one(self, stage: AttackStage) -> 'AttackSequence':
        """Freeze a value at a stage"""
        new_values = self.values.copy()
        new_frozen = self.frozen.copy()
        amount = min(self.get_value(stage), 1)
        new_frozen[stage] = amount + self.get_frozen(stage)
        new_values[stage] = self.get_value(stage) - amount
        return AttackSequence(values=new_values, frozen=new_frozen)

    
    def with_value(self, stage: AttackStage, value: int) -> 'AttackSequence':
        """Add or update a value for a stage"""
        new_values = self.values.copy()
        new_values[stage] = value
        return AttackSequence(values=new_values, frozen=self.frozen)

    def with_frozen(self, stage: AttackStage, value: int) -> 'AttackSequence':
        """Add or update a frozen value for a stage"""
        new_frozen = self.frozen.copy()
        new_frozen[stage] = value
        return AttackSequence(values=self.values.copy(), frozen=new_frozen)
    
    def get_value(self, stage: AttackStage, default: int = 0) -> int:
        """Get the value for a stage"""
        return self.values.get(stage, default)
    
    def get_frozen(self, stage: AttackStage, default: int = 0) -> int:
        """Get the frozen value for a stage"""
        return self.frozen.get(stage, default)
    
    def __str__(self) -> str:
        """Print stages in order, omitting zeros unless all values are zero."""
        parts = []
        all_zeros = all(val == 0 for val in self.values.values()) and all(val == 0 for val in self.frozen.values())
        
        # Process stages in enum order
        for stage in sorted(AttackStage):
            val = self.get_value(stage)
            frozen_val = self.get_frozen(stage)

            if stage in self.values or stage in self.frozen:
                if frozen_val != 0:
                    parts.append(f"{stage.name}:{val}+{frozen_val}*")
                else:                            
                    parts.append(f"{stage.name}:{val}")
        
        return f"[{', '.join(parts)}]"

    def __add__(self, other: 'AttackSequence') -> 'AttackSequence':
        """Combine two sequences by adding their values.
        Both sequences must be at the same stage.
        """
            
        # Combine all values from both sequences
        new_values = {}
        new_frozen = {}
        for stage in set(self.values.keys()) | set(other.values.keys()):
            new_values[stage] = self.get_value(stage) + other.get_value(stage)
        
            
        for stage in set(self.frozen.keys()) | set(other.frozen.keys()):
            new_frozen[stage] = self.get_frozen(stage) + other.get_frozen(stage)
            

        return AttackSequence(values=new_values, frozen=new_frozen)

    def do_hash(self) -> int:
        # Convert dict to tuple of tuples for hashing
        items = tuple(sorted((stage, value) for stage, value in self.values.items() if value != 0))
        frozen_items = tuple(sorted((stage, value) for stage, value in self.frozen.items() if value != 0))
        return hash(items + frozen_items)
    
    def __hash__(self) -> int:
        return self.hash_val    
        
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, AttackSequence):
            return NotImplemented
        return self.__hash__() == other.__hash__()

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, AttackSequence):
            return NotImplemented
            
        # Get all stages that appear in either sequence
        all_stages = sorted(set(self.values.keys()) | set(self.frozen.keys()) | 
                          set(other.values.keys()) | set(other.frozen.keys()))
        
        # Compare stage by stage
        for stage in all_stages:
            self_total = self.get_value(stage) + self.get_frozen(stage)
            other_total = other.get_value(stage) + other.get_frozen(stage)
            if self_total != other_total:
                return self_total < other_total
                
        return False  # Equal sequences

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
            return DieResult(result.value, AttackSequence.create(1, AttackStage.WOUNDS)
            .with_frozen(AttackStage.HITS, 1), True)
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
            return DieResult(result.value, AttackSequence.create(1, AttackStage.MORTAL_WOUNDS)
            .with_frozen(AttackStage.WOUNDS, 1), True)
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



