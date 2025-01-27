from .profile import *
from ..distribution import Distribution, d6, memoize, liftM, lift, Box
from typing import List, Optional, Callable, TypeVar, Union, Dict, Any
from fractions import Fraction
from ..dice import DiceFormula
from functools import partial
from dataclasses import dataclass, field, asdict
from time import perf_counter
import sys
import json


def get_wound_threshold(strength: int, toughness: int) -> int:
    """Determine the threshold needed to wound based on S vs T comparison"""
    if strength >= 2 * toughness:
        return 2
    if strength > toughness:
        return 3
    if strength == toughness:
        return 4
    if strength <= toughness / 2:
        return 6
    return 5

def perform_rolls(quant_stage: AttackStage, die_roll: Distribution[DieResult], state: AttackSequence) -> Distribution[AttackSequence]:
    quant = state.get_value(quant_stage)
    if quant == 0:
        return Distribution.singleton(state)
    unwrapped = unwrap_die_result(die_roll)
    combined =  unwrapped.repeated(quant)
    combined = combined.prune()
 
    result = combined.map(lambda x: x + state.freeze_all(quant_stage))
    return result

def perform_rolls_bind(quant_stage: AttackStage, die_roll: Distribution[DieResult]) -> Callable[[AttackSequence], Distribution[AttackSequence]]:
    def fn(state: AttackSequence) -> Distribution[AttackSequence]:
        return perform_rolls(quant_stage, die_roll, state)
    return fn

def roll_for_attacks(profile: AttackProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
    """Roll to determine number of attacks"""
    # Get attack count distribution
    attack_dist = profile.attacks.roll()
    attack_dist = attack_dist.map(lambda x: DieResult(x, AttackSequence({AttackStage.ATTACKS: x})))
    model_dist = Distribution.singleton(AttackSequence({AttackStage.START: profile.models * profile.guns_per_model}))
    model_dist = model_dist.bind(perform_rolls_bind(AttackStage.START, attack_dist))
    
    return model_dist


def merge_stage(old_seq: AttackSequence, roll_seq_boxed: Union[AttackSequence, DieResult], stage: AttackStage) -> AttackSequence:
    """Generic merge function that adds values for a specific stage."""
    new_vals = dict(old_seq.values)

    roll_seq: AttackSequence
    if isinstance(roll_seq_boxed, DieResult):
        if roll_seq_boxed.sequence is None:
            raise ValueError("DieResult has no sequence")
        roll_seq = roll_seq_boxed.sequence
    else:
        roll_seq = roll_seq_boxed

    return old_seq + roll_seq

def apply_modifiers(dist: Distribution[DieResult], stage: AttackStage, profile: AttackProfile, 
                   defender: Defender, modifiers: List[Modifier]) -> Distribution[DieResult]:
    all_modifiers = profile.modifiers + defender.get_modifiers() + modifiers
    for modifier in all_modifiers:
        dist = modifier.modify_roll(dist, stage, profile, defender)
    return dist

def roll_to_hit(attack_dist: Distribution[AttackSequence], profile: AttackProfile, 
                defender: Defender, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
    # Apply profile modifiers first
    
    threshold = min(6, max(2, profile.ballistic_skill))
    
    # Create distribution for a single hit roll
    single_hit_roll = d6.filter(
        pred=lambda x: x >= threshold,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.HITS: 1}), True),  # Hit
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.HITS: 0}), False)   # Miss
    )

    single_hit_roll = apply_modifiers(single_hit_roll, AttackStage.HITS, profile, defender, modifiers)
    
    return attack_dist.bind(perform_rolls_bind(AttackStage.ATTACKS, single_hit_roll))


   

def roll_to_wound(hit_dist: Distribution[AttackSequence], profile: AttackProfile, 
                 defender: Defender, modifiers: List[Modifier]) -> Distribution[AttackSequence]:

    threshold = get_wound_threshold(profile.strength, defender.get_highest_toughness())
    
    # Create distribution for a single wound roll
    single_wound_roll = d6.filter(
        pred=lambda x: x >= threshold,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.WOUNDS: 1}), True),  # Wound
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.WOUNDS: 0}), False)   # Fail
    )

    single_wound_roll = apply_modifiers(single_wound_roll, AttackStage.WOUNDS, profile, defender, modifiers)

    return hit_dist.bind(perform_rolls_bind(AttackStage.HITS, single_wound_roll))


def unwrap_die_result(dist: Distribution[DieResult]) -> Distribution[AttackSequence]:
    return dist.map(lambda x: x.sequence if x.sequence else AttackSequence.create(0, AttackStage.DAMAGE))

class AttackConfig:
    batch_save_roll = True
    batch_damage_roll = True

def save_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.WOUNDS) == 0:
        return Distribution.singleton(state)

    armor_save_threshold = current_defender_profile.armor_save + profile.armor_pen
    if current_defender_profile.invuln_save is not None:
        armor_save_threshold = min(armor_save_threshold, current_defender_profile.invuln_save)
    save_roll = d6.filter(
        pred=lambda x: x >= armor_save_threshold,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 0}), True),  # Save successful - no damage
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 1}), False)   # Save failed - wound goes through
    )
    save_roll = apply_modifiers(save_roll, AttackStage.FAILED_SAVES, profile, defender, modifiers)
    
    if defender.multiple_save_profiles() or not AttackConfig.batch_save_roll:
        if state.get_value(AttackStage.FAILED_SAVES) > 0:
            return Distribution.singleton(state)
        next_state = state.freeze_one(AttackStage.WOUNDS)
        result = save_roll.map(lambda x: (x.sequence + next_state) if x.sequence else next_state)
        return result
    else:
        return perform_rolls(AttackStage.WOUNDS, save_roll, state)

def batch_damage_allowed(defender: Defender, profile: AttackProfile) -> bool:
    return not defender.has_any_fnp() and \
        not profile.damage.is_variable() and \
        not defender.multiple_wound_profiles() and \
        AttackConfig.batch_damage_roll

def damage_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.FAILED_SAVES) == 0:
        return Distribution.singleton(state)
    
    next_state = state.freeze_one(AttackStage.FAILED_SAVES)
    
    damage_dist = profile.damage.roll()
    damage_rolls = damage_dist.map(lambda n: DieResult(n, AttackSequence({AttackStage.DAMAGE: n})))
    damage_rolls = apply_modifiers(damage_rolls, AttackStage.DAMAGE, profile, defender, modifiers)
    
    if not batch_damage_allowed(defender, profile):
        damage_rolls = unwrap_die_result(damage_rolls)
        result = damage_rolls.map(lambda x: x + next_state)
        return result
    else:
        d = profile.damage.modifier
        while d < current_defender_profile.wounds:
            d += profile.damage.modifier
        damage_ignored_per_model = d - current_defender_profile.wounds
        
        result = perform_rolls(AttackStage.FAILED_SAVES, damage_rolls, state)
        return result


def fnp_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.DAMAGE) == 0:
        return Distribution.singleton(state)
    fnp = current_defender_profile.feel_no_pain if current_defender_profile.feel_no_pain else 7
    fnp_roll = d6.filter(
        pred=lambda x: x >= fnp,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 0}), True),  # FNP successful - no damage
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 1}), False)   # FNP failed - damage goes through
    )
    fnp_roll = apply_modifiers(fnp_roll, AttackStage.FELT_DMG, profile, defender, modifiers)
    
    result = perform_rolls(AttackStage.DAMAGE, fnp_roll, state)

    wound_cap = current_defender_profile.wounds
    def cap_dmg_felt(seq: AttackSequence) -> AttackSequence:
        return seq.with_value(
            AttackStage.FELT_DMG,
            min(seq.get_value(AttackStage.FELT_DMG), wound_cap),
        )

    result = result.map(cap_dmg_felt)
    return result

def slay_models(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> AttackSequence:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.FELT_DMG) == 0:
        return state
    defender_wounds = current_defender_profile.wounds
    result : AttackSequence
    if state.get_value(AttackStage.FELT_DMG) >= defender_wounds:
        num_slain : int = state.get_value(AttackStage.FELT_DMG) // defender_wounds
        result = state.with_value(AttackStage.MODELS_SLAIN, state.get_value(AttackStage.MODELS_SLAIN) + num_slain) \
            .freeze_all(AttackStage.FELT_DMG)
    else:
        result = state
    return result

state_cache = {}



def do_dmg_sequence(wound_dist: Distribution[AttackSequence], defender: Defender, profile: AttackProfile, modifiers: List[Modifier], timing: 'TimingInfo') -> Distribution[AttackSequence]:
    """Run the damage sequence with timing information"""
    allow_batch_saves = not defender.multiple_save_profiles() and AttackConfig.batch_save_roll
    allow_batch_damage = not defender.has_any_fnp() and AttackConfig.batch_damage_roll


    save_roll_fn = liftM(memoize(partial(save_roll, defender, profile, modifiers)))
    damage_roll_fn = liftM(memoize(partial(damage_roll, defender, profile, modifiers)))
    fnp_roll_fn = liftM(memoize(partial(fnp_roll, defender, profile, modifiers)))
    slay_models_fn = lift(memoize(partial(slay_models, defender, profile, modifiers)))

    # if allow_batch_saves:
    #     save_start = perf_counter()
    #     wound_dist = roll_to_save(wound_dist, profile, defender, modifiers)
    #     timing.save_roll_time = perf_counter() - save_start
    #     save_roll_fn = lambda d: d

    current = wound_dist
    while True:
        save_start = perf_counter()
        next_dist = save_roll_fn(current)
        timing.save_roll_time += perf_counter() - save_start

        damage_start = perf_counter()
        next_dist = damage_roll_fn(next_dist)
        timing.damage_roll_time += perf_counter() - damage_start

        fnp_start = perf_counter()
        next_dist = fnp_roll_fn(next_dist)
        timing.fnp_roll_time += perf_counter() - fnp_start

        slay_start = perf_counter()
        next_dist = slay_models_fn(next_dist)
        timing.slay_models_time += perf_counter() - slay_start

        next_dist = next_dist.prune()

        if current == next_dist:
            break
        current = next_dist

    return current


def bind_stage(dist: Distribution[AttackSequence], stage: AttackStage, 
              f: Callable[[int], Distribution[int]]) -> Distribution[AttackSequence]:
    """Bind a function to a specific stage's value in an AttackSequence."""
    return dist.bind(lambda seq: f(seq.get_value(stage)).map(
        lambda new_val: AttackSequence({
            **seq.values,
            stage: new_val
        })
    ))

def collapse_stage(dist: Distribution[AttackSequence], stage: AttackStage) -> Distribution[AttackSequence]:
    """Collapse a stage's value into a single value"""
    return dist.map(lambda seq: AttackSequence.create(seq.get_value(stage) + seq.get_frozen(stage), stage))

def cap_stage(dist: Distribution[AttackSequence], stage: AttackStage, cap: int) -> Distribution[AttackSequence]:
    """Cap a stage's value at a specific value"""
    return dist.map(lambda seq: AttackSequence.create(min(seq.get_value(stage), cap), stage))


@dataclass
class TimingInfo:
    attack_roll_time: float
    hit_roll_time: float
    wound_roll_time: float
    save_roll_time: float
    damage_roll_time: float
    fnp_roll_time: float
    slay_models_time: float
    total_time: float

    @property
    def breakdown(self) -> Dict[str, float]:
        return {
            "attacks": self.attack_roll_time,
            "hits": self.hit_roll_time,
            "wounds": self.wound_roll_time,
            "saves": self.save_roll_time,
            "damage": self.damage_roll_time,
            "fnp": self.fnp_roll_time,
            "slay_models": self.slay_models_time,
            "total": self.total_time
        }

    def __str__(self) -> str:
        result = ["\nTiming:"]
        for stage, time in self.breakdown.items():
            result.append(f"  {stage}: {time:.3f}s") 
        return "\n".join(result)

@dataclass
class AttackResults:
    attacks: Distribution[AttackSequence]
    hits: Distribution[AttackSequence]
    wounds: Distribution[AttackSequence]
    damage: Distribution[AttackSequence]
    timing: TimingInfo
    
    def __str__(self) -> str:
        sections = [
            ("Attacks", self.collapsed_hits),
            ("Wounds", self.collapsed_wounds),
            ("Failed Saves", self.collapsed_failed_saves),
            ("Damage", self.collapsed_damage),
            ("Models Slain", self.collapsed_slain_models)
        ]
        
        result = []
        for name, dist in sections:
            result.append(f"\n{name}:")
            result.append(str(dist))
            
        result.append(str(self.timing))
            
        return "\n".join(result)
    
    @property
    def collapsed_hits(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.hits, AttackStage.HITS)
        
    @property
    def collapsed_wounds(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.wounds, AttackStage.WOUNDS)
        
    @property
    def collapsed_failed_saves(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.damage, AttackStage.FAILED_SAVES)
        
    @property
    def collapsed_damage(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.damage, AttackStage.DAMAGE)
        
    @property
    def collapsed_felt_damage(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.damage, AttackStage.FELT_DMG)
        
    @property
    def collapsed_slain_models(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.damage, AttackStage.MODELS_SLAIN)

    def __add__(self, other: 'AttackResults') -> 'AttackResults':
        return AttackResults(
            attacks=self.attacks.merge(other.attacks),
            hits=self.hits.merge(other.hits),
            wounds=self.wounds.merge(other.wounds),
            damage=self.damage.merge(other.damage),
            timing=self.timing
        )

    def normalize(self) -> 'AttackResults':
        return AttackResults(
            attacks=self.attacks.normalize(),
            hits=self.hits.normalize(),
            wounds=self.wounds.normalize(),
            damage=self.damage.normalize(),
            timing=self.timing
        )



def simulate_attacks(
    attack_profile: AttackProfile,
    defender: Defender,
    modifiers: List[Modifier] = []
) -> AttackResults:
    start_time = perf_counter()
    
    all_modifiers = attack_profile.modifiers + defender.get_modifiers() + modifiers
    for modifier in all_modifiers:
        attack_profile = modifier.modify_attacker(attack_profile, defender)
        defender = modifier.modify_defender(attack_profile, defender)

    attack_start = perf_counter()
    attack_dist = roll_for_attacks(attack_profile, modifiers).prune()
    attack_time = perf_counter() - attack_start

    hit_start = perf_counter()
    hit_dist = roll_to_hit(attack_dist, attack_profile, defender, modifiers).prune()
    
    cleaned_hit_dist = hit_dist.map(lambda x: x.clear_frozen()).prune()
    hit_time = perf_counter() - hit_start

    
    wound_start = perf_counter()
    wound_dist = roll_to_wound(cleaned_hit_dist, attack_profile, defender, modifiers).prune()
    cleaned_wound_dist = wound_dist.map(lambda x: x.clear_frozen()).prune()
    wound_time = perf_counter() - wound_start
    
    timing = TimingInfo(
        attack_roll_time=attack_time,
        hit_roll_time=hit_time,
        wound_roll_time=wound_time,
        save_roll_time=0.0,
        damage_roll_time=0.0,
        fnp_roll_time=0.0,
        slay_models_time=0.0,
        total_time=0.0
    )
    
    damage_start = perf_counter()
    final_dist = do_dmg_sequence(cleaned_wound_dist, defender, attack_profile, modifiers, timing)
    
    total_time = perf_counter() - start_time
    timing.total_time = total_time
    
    return AttackResults(
        attacks=attack_dist,
        hits=hit_dist,
        wounds=wound_dist,
        damage=final_dist,
        timing=timing
    )

def process_test_case(test_case: Dict[str, Any]) -> Dict[str, Any]:
    """Process a single test case and return results"""
    try:
        # Create attack profile from test case
        attack = test_case["attack"]
        attack_profile = AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.from_dict(attack["attacks"]),
            ballistic_skill=attack["ballistic_skill"],
            strength=attack["strength"],
            armor_pen=attack["armor_pen"],
            damage=DiceFormula.from_dict(attack["damage"]),
            modifiers=[],
            keywords=[]
        )

        # Create defender from test case
        defend = test_case["defender"]
        defender = Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=defend["models"],
                toughness=defend["toughness"],
                armor_save=defend["armor_save"],
                invuln_save=defend["invuln_save"],
                wounds=defend["wounds"],
                feel_no_pain=defend["feel_no_pain"],
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        )

        # Run simulation
        results = simulate_attacks(attack_profile, defender)

        # Convert results to probabilities dict
        probabilities = {
            "HITS": {str(k.get_value(AttackStage.HITS)): float(v) 
                    for k, v in results.collapsed_hits.probabilities.items()},
            "WOUNDS": {str(k.get_value(AttackStage.WOUNDS)): float(v) 
                      for k, v in results.collapsed_wounds.probabilities.items()},
            "FAILED_SAVES": {str(k.get_value(AttackStage.FAILED_SAVES)): float(v) 
                           for k, v in results.collapsed_failed_saves.probabilities.items()},
            "DAMAGE": {str(k.get_value(AttackStage.DAMAGE)): float(v) 
                      for k, v in results.collapsed_damage.probabilities.items()},
            "MODELS_SLAIN": {str(k.get_value(AttackStage.MODELS_SLAIN)): float(v) 
                           for k, v in results.collapsed_slain_models.probabilities.items()}
        }

        return {
            "success": True,
            "probabilities": probabilities,
            "timing": asdict(results.timing)
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e)
        }

def run_calculator() -> None:
    """Run as a calculator process reading from stdin and writing to stdout"""
    while True:
        try:
            line = sys.stdin.readline()
            if not line:
                break

            test_case = json.loads(line)
            result = process_test_case(test_case)
            json.dump(result, sys.stdout)
            sys.stdout.write('\n')
            sys.stdout.flush()

        except Exception as e:
            json.dump({
                "success": False,
                "error": f"Calculator error: {str(e)}"
            }, sys.stdout)
            sys.stdout.write('\n')
            sys.stdout.flush()

if __name__ == "__main__":
    run_calculator()