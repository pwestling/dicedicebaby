from probability.warhammer.profile import *
from probability.distribution import Distribution, d6, memoize, liftM, lift, Box, close_multiprocessing_pool
from probability.dice import DiceFormula
from typing import List, Optional, Callable, TypeVar, Union, Dict, Any, Tuple
from fractions import Fraction
from functools import partial
from dataclasses import dataclass, field, asdict
from time import perf_counter
import sys
import json
from multiprocessing import Pool
import os


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

def perform_rolls(quant_stage: AttackStage, die_roll: Distribution[DieResult], state: AttackSequence, max_rolls: int | None = None) -> Distribution[AttackSequence]:
    quant = state.get_value(quant_stage) if max_rolls is None else min(state.get_value(quant_stage), max_rolls)
    if quant == 0:
        return Distribution.singleton(state)
    
    unwrapped = unwrap_die_result(die_roll)
    combined = monte_carlo_repeat(unwrapped, quant)
    combined = combined.prune()
 
    result = combined.map(lambda x: x + state.freeze_quant(quant_stage, quant))
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
    single_hit_roll = single_hit_roll.map(six_critical)
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
    single_wound_roll = single_wound_roll.map(six_critical)

    single_wound_roll = apply_modifiers(single_wound_roll, AttackStage.WOUNDS, profile, defender, modifiers)

    return hit_dist.bind(perform_rolls_bind(AttackStage.HITS, single_wound_roll))


def unwrap_die_result(dist: Distribution[DieResult]) -> Distribution[AttackSequence]:
    return dist.map(lambda x: x.sequence if x.sequence else AttackSequence.create(0, AttackStage.DAMAGE))

class AttackConfig:
    batch_save_roll = True
    batch_damage_roll = True
    monte_carlo = False
    monte_carlo_simulations = 10000

T = TypeVar("T")
def monte_carlo(dist: Distribution[T]) -> Distribution[T]:
    if AttackConfig.monte_carlo:
        return dist.collapse_to_choice()
    return dist

def monte_carlo_repeat(dist: Distribution[T], n: int) -> Distribution[T]:
    if AttackConfig.monte_carlo:
        result = dist.collapse_to_choice()
        for _ in range(n - 1):
            result = result.combine(dist.collapse_to_choice(), lambda x, y: x + y)
        return result
    else:
        return dist.repeated(n)

def six_critical(dist: DieResult) -> DieResult:
    if dist.value == 6:
        return DieResult(dist.value, dist.sequence, dist.passes_check, True)
    return dist

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
    
    rolls_in_batch = None # unlimited
    if not AttackConfig.batch_save_roll:
        rolls_in_batch = 1
    elif defender.multiple_save_profiles():
        currently_dealt_damage = state.get_value(AttackStage.FELT_DMG)
        rolls_in_batch = (current_defender_profile.wounds - currently_dealt_damage) // profile.damage.max_possible()
        rolls_in_batch = max(rolls_in_batch, 1)
    return perform_rolls(AttackStage.WOUNDS, save_roll, state, max_rolls=rolls_in_batch)

def cleanly_divisible_wounds_batch_size(defender: Defender, possible_results: Distribution[DieResult], state: AttackSequence, output_stage: AttackStage) -> int | None:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if current_defender_profile is None:
        return None
    if state.get_value(AttackStage.FELT_DMG) > 0:
        return None
    if current_defender_profile.feel_no_pain is not None:
        return None
    if len(possible_results.probabilities) != 1:
        return None
    damage = possible_results.get_singleton().sequence.get_value(output_stage)
    if damage == 0:
        return None
    if current_defender_profile.wounds % damage != 0 or damage > current_defender_profile.wounds:
        return None
    shots_per_model = current_defender_profile.wounds // damage
    return (current_defender_profile.models - state.get_value(AttackStage.MODELS_SLAIN)) * shots_per_model

def safely_reduce_damage_result(current_defender_profile: DefenderProfile) -> Callable[[DieResult], DieResult]:
    def fn(result: DieResult) -> DieResult:
        current_damage = result.sequence.get_value(AttackStage.DAMAGE)
        new_sequence = result.sequence.freeze_quant(AttackStage.DAMAGE, current_damage - current_defender_profile.wounds)
        print(new_sequence)
        return DieResult(result.value, new_sequence, result.passes_check)
    return fn

def damage_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    target_stage = AttackStage.FAILED_SAVES
    output_stage = AttackStage.DAMAGE
    if state.get_value(AttackStage.FAILED_SAVES) == 0 and state.get_value(AttackStage.MORTAL_WOUNDS) > 0:
        target_stage = AttackStage.MORTAL_WOUNDS
        output_stage = AttackStage.MORTAL_DAMAGE
    if not current_defender_profile or state.get_value(target_stage) == 0:
        return Distribution.singleton(state)
    
    damage_dist = profile.damage.roll()
    damage_rolls = damage_dist.map(lambda n: DieResult(n, AttackSequence({output_stage: n})))
    damage_rolls = apply_modifiers(damage_rolls, output_stage, profile, defender, modifiers)

    rolls_in_batch = 1
    if AttackConfig.batch_damage_roll:
        maybe_batch_size = cleanly_divisible_wounds_batch_size(defender, damage_rolls, state, output_stage)
        if maybe_batch_size is not None:
            rolls_in_batch = maybe_batch_size
        else:
            defender_remaining_wounds = current_defender_profile.wounds - state.get_value(AttackStage.FELT_DMG)
            rolls_in_batch = defender_remaining_wounds // profile.damage.max_possible()
            rolls_in_batch = max(rolls_in_batch, 1)
    return perform_rolls(target_stage, damage_rolls, state, max_rolls=rolls_in_batch)


def fnp_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    target_stage = AttackStage.DAMAGE
    if state.get_value(AttackStage.DAMAGE) == 0 and state.get_value(AttackStage.MORTAL_DAMAGE) > 0:
        target_stage = AttackStage.MORTAL_DAMAGE
    if not current_defender_profile or state.get_value(target_stage) == 0:
        return Distribution.singleton(state)
    
    fnp = current_defender_profile.feel_no_pain if current_defender_profile.feel_no_pain else 7
    fnp_roll = d6.filter(
        pred=lambda x: x >= fnp,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 0}), True),  # FNP successful - no damage
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 1}), False)   # FNP failed - damage goes through
    )
    fnp_roll = apply_modifiers(fnp_roll, AttackStage.FELT_DMG, profile, defender, modifiers)
    
    result = perform_rolls(target_stage, fnp_roll, state)

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

    current = wound_dist
    while True:
        save_start = perf_counter()
        next_dist = save_roll_fn(current).prune()
        timing.save_roll_time += perf_counter() - save_start

        damage_start = perf_counter()
        next_dist = damage_roll_fn(next_dist).prune()
        timing.damage_roll_time += perf_counter() - damage_start

        fnp_start = perf_counter()
        next_dist = fnp_roll_fn(next_dist).prune()
        timing.fnp_roll_time += perf_counter() - fnp_start

        slay_start = perf_counter()
        next_dist = slay_models_fn(next_dist).prune()
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

def collapse_stages(dist: Distribution[AttackSequence], stages: List[AttackStage]) -> Distribution[AttackSequence]:
    """Collapse a stage's value into a single value"""
    first_stage = stages[0]
    def get_values(s: AttackSequence) -> int:
        total = 0
        for stage in stages:
            total += s.get_value(stage) + s.get_frozen(stage)
        return total
    result = dist.map(lambda seq: AttackSequence.create(get_values(seq), first_stage))
    return result


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
    max_entries_achieved: int

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
            "total": self.total_time,
            "max_entries_achieved": self.max_entries_achieved
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
            ("Mortal Wounds", self.collapsed_mortal_wounds),
            ("Damage", self.collapsed_damage),
            ("Felt Damage", self.collapsed_felt_damage),
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
    def collapsed_mortal_wounds(self) -> Distribution[AttackSequence]:
        return collapse_stage(self.damage, AttackStage.MORTAL_WOUNDS)
        
    @property
    def collapsed_damage(self) -> Distribution[AttackSequence]:
        return collapse_stages(self.damage, [AttackStage.DAMAGE, AttackStage.MORTAL_DAMAGE])
        
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

def get_batch_size() -> int:
    """Calculate batch size to get ~4 batches per core"""
    cores = os.cpu_count() or 1
    print(f"Cores: {cores}")
    return max(1000, AttackConfig.monte_carlo_simulations // (cores * 4))

def _run_monte_carlo_batch(args: Tuple[AttackProfile, Defender, List[Modifier], int]) -> AttackResults:
    profile, defender, modifiers, count = args
    result = simulate_attacks_internal(profile, defender, modifiers)
    for _ in range(count - 1):
        next_result = simulate_attacks_internal(profile, defender, modifiers)
        result = result + next_result
    return result

def simulate_attacks(
    attack_profile: AttackProfile,
    defender: Defender,
    modifiers: List[Modifier] = []
) -> AttackResults:
    if AttackConfig.monte_carlo:
        start_time = perf_counter()
        batch_size = get_batch_size()
        num_tasks = (AttackConfig.monte_carlo_simulations + batch_size - 1) // batch_size
        with Pool() as pool:
            tasks = [(attack_profile, defender, modifiers, batch_size)] * num_tasks
            results = pool.map(_run_monte_carlo_batch, tasks)
        first = results[0]
        for result in results[1:]:
            first = first + result
        total_time = perf_counter() - start_time
        first.timing.total_time = total_time
        return first.normalize()
    else:
        return simulate_attacks_internal(attack_profile, defender, modifiers)

def simulate_attacks_internal(
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
        total_time=0.0,
        max_entries_achieved=0
    )
    
    damage_start = perf_counter()
    final_dist = do_dmg_sequence(cleaned_wound_dist, defender, attack_profile, modifiers, timing)
    
    total_time = perf_counter() - start_time
    timing.total_time = total_time
    timing.max_entries_achieved = Distribution.MAX_ENTRIES_ACHIEVED

    close_multiprocessing_pool()

    return AttackResults(
        attacks=attack_dist,
        hits=hit_dist,
        wounds=wound_dist,
        damage=final_dist,
        timing=timing
    )
