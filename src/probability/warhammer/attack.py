from .profile import *
from ..distribution import Distribution, d6, memoize, lift
from typing import List, Optional, Callable, TypeVar, Union, Dict
from fractions import Fraction
from ..dice import DiceFormula
from functools import partial
from dataclasses import dataclass, field
from time import perf_counter


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

def roll_many_times(
    prior_dist: Distribution[AttackSequence],
    quantity_stage: AttackStage,
    target_stage: AttackStage,
    single_roll_dist: Distribution[DieResult],
    keep_history: bool = False,
) -> Distribution[AttackSequence]:
    """
    Generic function for "roll N times" logic.
      prior_dist: distribution of AttackSequence so far.
      quantity_stage: which stage in AttackSequence says "how many times do we roll?"
      target_stage: which stage in AttackSequence we're accumulating
      single_roll_dist: distribution describing a single roll's outcome as an AttackSequence
      merge_fn(old_seq, roll_seq) => AttackSequence
        how to update old_seq with the single-roll outcome.
    """

    result = Distribution.empty()  # We'll build it up by merging partial distributions.

    for seq, prob in prior_dist.probabilities.items():
        quantity = seq.get_value(quantity_stage)  # e.g. number of hits or number of wounds
        existing_values = Distribution.singleton(AttackSequence({target_stage: seq.get_value(target_stage)}))
       

        # Start from a distribution of just "no outcome"
        accum : Distribution[AttackSequence] 
        if not keep_history:
            accum = Distribution.singleton(AttackSequence({target_stage: 0}))
        else:
            accum = prior_dist.map(lambda x: x)

        accum = accum.combine(existing_values, lambda old_seq, roll_seq: merge_stage(old_seq, roll_seq, target_stage))
       

        # For each roll, combine accum with the single-roll distribution
        for _ in range(quantity):
            accum = accum.combine(single_roll_dist, lambda old_seq, roll_seq: merge_stage(old_seq, roll_seq, target_stage))
        
        # Now accum is the result of rolling that many times, scaled by prob
        result = result.merge(accum.scale(prob))

    return result

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


        
def save_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.WOUNDS) == 0:
        return Distribution.singleton(state)

    if state.get_value(AttackStage.FAILED_SAVES) > 0:
        return Distribution.singleton(state)
    next_state = state.with_value(AttackStage.WOUNDS, state.get_value(AttackStage.WOUNDS) - 1)
    # next_state = next_state.with_frozen(AttackStage.WOUNDS, next_state.get_frozen(AttackStage.WOUNDS) + 1)
    armor_save_threshold = current_defender_profile.armor_save + profile.armor_pen
    if current_defender_profile.invuln_save is not None:
        armor_save_threshold = min(armor_save_threshold, current_defender_profile.invuln_save)
    save_roll = d6.filter(
        pred=lambda x: x >= armor_save_threshold,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 0})),  # Save successful - no damage
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 1}))   # Save failed - wound goes through
    )
    save_roll = apply_modifiers(save_roll, AttackStage.FAILED_SAVES, profile, defender, modifiers)
    return save_roll.map(lambda x: x.sequence + next_state if x.sequence else next_state)

def damage_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.FAILED_SAVES) == 0:
        return Distribution.singleton(state)
    
    next_state = state.with_value(AttackStage.FAILED_SAVES, state.get_value(AttackStage.FAILED_SAVES) - 1)
    next_state = next_state.with_frozen(AttackStage.FAILED_SAVES, next_state.get_frozen(AttackStage.FAILED_SAVES) + 1)
    
    damage_dist = profile.damage.roll()
    
    damage_rolls = damage_dist.map(lambda n: DieResult(n, AttackSequence({AttackStage.DAMAGE: n})))
    damage_rolls = apply_modifiers(damage_rolls, AttackStage.DAMAGE, profile, defender, modifiers)
    damage_rolls = unwrap_die_result(damage_rolls)
    
    return damage_rolls.map(lambda x: x + next_state)



def fnp_roll(defender: Defender, profile: AttackProfile, modifiers: List[Modifier], state: AttackSequence) -> Distribution[AttackSequence]:
    current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
    if not current_defender_profile or state.get_value(AttackStage.DAMAGE) == 0:
        return Distribution.singleton(state)
    fnp = current_defender_profile.feel_no_pain if current_defender_profile.feel_no_pain else 7
    fnp_roll = d6.filter(
        pred=lambda x: x >= fnp,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 0})),  # FNP successful - no damage
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 1}))   # FNP failed - damage goes through
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
        result = state.with_value(AttackStage.MODELS_SLAIN, state.get_value(AttackStage.MODELS_SLAIN) + 1) \
            .with_value(AttackStage.FELT_DMG, min(state.get_value(AttackStage.FELT_DMG), defender_wounds)) \
            .freeze_all(AttackStage.FELT_DMG)
    else:
        result = state
    return result

state_cache = {}

def compose(f: Callable[[Distribution[AttackSequence]], Distribution[AttackSequence]], g: Callable[[Distribution[AttackSequence]], Distribution[AttackSequence]]) -> Callable[[Distribution[AttackSequence]], Distribution[AttackSequence]]:
    def composed(a: Distribution[AttackSequence]) -> Distribution[AttackSequence]:
        return g(f(a))
    return composed

def do_dmg_sequence(wound_dist: Distribution[AttackSequence], defender: Defender, profile: AttackProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:

    save_roll_fn = lift(memoize(partial(save_roll, defender, profile, modifiers)))
    damage_roll_fn = lift(memoize(partial(damage_roll, defender, profile, modifiers)))
    fnp_roll_fn = lift(memoize(partial(fnp_roll, defender, profile, modifiers)))
    slay_models_fn = memoize(partial(slay_models, defender, profile, modifiers))
    
    
    def composed_full(dist: Distribution[AttackSequence]) -> Distribution[AttackSequence]:
        return fnp_roll_fn(damage_roll_fn(save_roll_fn(dist))).map(slay_models_fn).prune()
    composed = memoize(composed_full)

    current = wound_dist
    while True:
        next_dist = composed(current)
        # next_dist = next_dist.map(lambda x: x.clear_frozen())        
        
        
        
        if next_dist == current:
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

def cumulate_probabilities(dist: Distribution[AttackSequence], limit: Fraction, stage: AttackStage) -> Distribution[AttackSequence]:
    collapsed = collapse_stage(dist, stage)
    # Sort the values for this stage, and find the first value where all probabilities for values above it are less than limit
    values = [(x.get_value(stage), p) for x, p in collapsed.probabilities.items()]
    values = sorted(values, key=lambda x: -x[0])
    index = 0
    while index < len(values) and values[index][1] < limit:
        index += 1
    cap = values[index][0]
    # return collapsed.map(lambda x: x.with_value(stage, min(x.get_value(stage), cap)))
    return dist

@dataclass
class TimingInfo:
    attack_roll_time: float
    hit_roll_time: float
    wound_roll_time: float
    damage_roll_time: float
    total_time: float

    @property
    def breakdown(self) -> Dict[str, float]:
        return {
            "attacks": self.attack_roll_time,
            "hits": self.hit_roll_time,
            "wounds": self.wound_roll_time,
            "damage": self.damage_roll_time,
            "total": self.total_time
        }

@dataclass
class AttackResults:
    attacks: Distribution[AttackSequence]
    hits: Distribution[AttackSequence]
    wounds: Distribution[AttackSequence]
    damage: Distribution[AttackSequence]
    timing: TimingInfo
    
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

def simulate_attacks(
    attack_profile: AttackProfile,
    defender: Defender,
    modifiers: List[Modifier] = []
) -> AttackResults:
    """Simulate a full attack sequence and return the results at each stage."""
    
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
    
    damage_start = perf_counter()
    dmg_dist = do_dmg_sequence(cleaned_wound_dist, defender, attack_profile, modifiers).prune()
    damage_time = perf_counter() - damage_start
    
    total_time = perf_counter() - start_time
    
    timing = TimingInfo(
        attack_roll_time=attack_time,
        hit_roll_time=hit_time,
        wound_roll_time=wound_time,
        damage_roll_time=damage_time,
        total_time=total_time
    )
    
    return AttackResults(
        attacks=attack_dist,
        hits=hit_dist,
        wounds=wound_dist,
        damage=dmg_dist,
        timing=timing
    )

def full_demo() -> None:
    results = simulate_attacks(LASGUN_BARRAGE, GUARDSMAN)
    print("Number of attacks:")
    print(results.attacks)
    
    print("\nAfter hit rolls:")
    print(results.hits)
    print("\nCollapsed hit rolls:")
    print(results.collapsed_hits)
    
    print("\nAfter wound rolls:")
    print(results.wounds)
    print("\nCollapsed wound rolls:")
    print(results.collapsed_wounds)
    
    print("\nAfter damage rolls:")
    print(results.damage)
    print("\nCollapsed failed saves:")
    print(results.collapsed_failed_saves)
    print("\nCollapsed damage:")
    print(results.collapsed_damage)
    print("\nCollapsed felt damage:")
    print(results.collapsed_felt_damage)
    print("\nCollapsed slain models:")
    print(results.collapsed_slain_models)

@dataclass
class ExpectedProbability:
    stage: AttackStage
    value: int
    probability: float
    tolerance: float = 0.00001

@dataclass
class TestCase:
    name: str
    attack_profile: AttackProfile
    defender: Defender
    modifiers: List[Modifier]
    expected: List[ExpectedProbability]

def check_probability(results: AttackResults, expected: ExpectedProbability) -> bool:
    """Check if a specific probability in the results matches expected within tolerance"""
    if expected.stage == AttackStage.HITS:
        dist = results.collapsed_hits
    elif expected.stage == AttackStage.WOUNDS:
        dist = results.collapsed_wounds
    elif expected.stage == AttackStage.FAILED_SAVES:
        dist = results.collapsed_failed_saves
    elif expected.stage == AttackStage.DAMAGE:
        dist = results.collapsed_damage
    elif expected.stage == AttackStage.FELT_DMG:
        dist = results.collapsed_felt_damage
    elif expected.stage == AttackStage.MODELS_SLAIN:
        dist = results.collapsed_slain_models
    else:
        raise ValueError(f"Unsupported stage for testing: {expected.stage}")

    actual = 0.0
    for seq, prob in dist.probabilities.items():
        if seq.get_value(expected.stage) == expected.value:
            actual = float(prob)
            break

    diff = abs(actual - expected.probability)
    if diff > expected.tolerance:
        print(f"Failed: {expected.stage} = {expected.value}")
        print(f"Expected: {expected.probability}")
        print(f"Got: {actual}")
        print(f"Diff: {diff}")
        return False
    return True

def run_test_case(test: TestCase) -> bool:
    """Run a single test case and check all expected probabilities"""
    print(f"\nRunning test: {test.name}")
    results = simulate_attacks(test.attack_profile, test.defender, test.modifiers)
    
    all_passed = True
    for exp in test.expected:
        if not check_probability(results, exp):
            all_passed = False
    
    return all_passed

def run_test_suite() -> None:
    """Run all test cases"""
    basic_profile = AttackProfile(
        name="Test Profile",
        models=1,
        guns_per_model=1,
        attacks=DiceFormula.constant(7),
        ballistic_skill=4,
        strength=3,
        armor_pen=0,
        damage=DiceFormula.constant(2),
        modifiers=[RerollAllFails(AttackStage.HITS), LethalHits(), DevastatingWounds()],
        keywords=[]
    )

    defender = Defender(
        name="Test Defender",
        profiles=[DefenderProfile(
            name="Test Model",
            models=20,
            toughness=3,
            armor_save=4,
            invuln_save=None,
            wounds=1,
            feel_no_pain=5,
            modifiers=[],
            keywords=[],
            is_leader=False
        )]
    )

    test_cases = [
        TestCase(
            name="Basic 7 attacks",
            attack_profile=basic_profile,
            defender=defender,
            modifiers=[],
            expected=[
                # Hit results
                ExpectedProbability(AttackStage.HITS, 0, 0.00006103515625),
                ExpectedProbability(AttackStage.HITS, 1, 0.00128173828125),
                ExpectedProbability(AttackStage.HITS, 2, 0.01153564453125),
                ExpectedProbability(AttackStage.HITS, 3, 0.05767822265625),
                ExpectedProbability(AttackStage.HITS, 4, 0.17303466796875),
                ExpectedProbability(AttackStage.HITS, 5, 0.31146240234375),
                ExpectedProbability(AttackStage.HITS, 6, 0.31146240234375),
                ExpectedProbability(AttackStage.HITS, 7, 0.13348388671875),
                
                # Wound results
                ExpectedProbability(AttackStage.WOUNDS, 0, 0.0078125),
                ExpectedProbability(AttackStage.WOUNDS, 1, 0.0546875),
                ExpectedProbability(AttackStage.WOUNDS, 2, 0.1640625),
                ExpectedProbability(AttackStage.WOUNDS, 3, 0.2734375),
                ExpectedProbability(AttackStage.WOUNDS, 4, 0.2734375),
                ExpectedProbability(AttackStage.WOUNDS, 5, 0.1640625),
                ExpectedProbability(AttackStage.WOUNDS, 6, 0.0546875),
                ExpectedProbability(AttackStage.WOUNDS, 7, 0.00781208137752915),
                
                # Failed saves
                ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.08946718186289958),
                ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.2578759947812988),
                ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.318552699435722),
                ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.21861459765196609),
                ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.0900157087176373),
                ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.022232902813852855),
                ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.0030467831363588625),
                
                # Damage results
                ExpectedProbability(AttackStage.DAMAGE, 0, 0.08946718186289958),
                ExpectedProbability(AttackStage.DAMAGE, 2, 0.2578759947812988),
                ExpectedProbability(AttackStage.DAMAGE, 4, 0.318552699435722),
                ExpectedProbability(AttackStage.DAMAGE, 6, 0.21861459765196609),
                ExpectedProbability(AttackStage.DAMAGE, 8, 0.0900157087176373),
                ExpectedProbability(AttackStage.DAMAGE, 10, 0.022232902813852855),
                
                # Felt damage
                ExpectedProbability(AttackStage.FELT_DMG, 0, 0.12236508717748674),
                ExpectedProbability(AttackStage.FELT_DMG, 1, 0.29979449212130976),
                ExpectedProbability(AttackStage.FELT_DMG, 2, 0.3147854245032073),
                ExpectedProbability(AttackStage.FELT_DMG, 3, 0.18362324508497507),
                ExpectedProbability(AttackStage.FELT_DMG, 4, 0.06426789895234573),
                ExpectedProbability(AttackStage.FELT_DMG, 5, 0.01349572230118509),
                
                # Models slain
                ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.12236508717748674),
                ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.29979449212130976),
                ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.3147854245032073),
                ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 0.18362324508497507),
                ExpectedProbability(AttackStage.MODELS_SLAIN, 4, 0.06426789895234573),
                ExpectedProbability(AttackStage.MODELS_SLAIN, 5, 0.01349572230118509),
            ]
        )
    ]

    all_passed = True
    for test in test_cases:
        if not run_test_case(test):
            all_passed = False
    
    if all_passed:
        print("\nAll tests passed!")
    else:
        print("\nSome tests failed!")
        exit(1)

def run_attack_benchmark(max_time: float = 10.0) -> None:
    """Run increasingly large attack counts until simulation takes longer than max_time seconds"""
    last_time = 0
    attacks = 0
    
    while last_time < max_time:
        attacks += 3
        profile = AttackProfile(
            name=f"Test Profile ({attacks} attacks)",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(attacks),
            ballistic_skill=4,
            strength=3,
            armor_pen=0,
            damage=DiceFormula.constant(1),
            modifiers=[RerollAllFails(AttackStage.HITS), LethalHits(), DevastatingWounds()],
            keywords=[]
        )
        
        results = simulate_attacks(profile, GUARDSMAN)
        last_time = results.timing.total_time
        print(f"Simulated {attacks} attacks in {last_time:.3f} seconds")
    
    print(f"\nReached {attacks} attacks before simulation took {last_time:.3f} seconds")

if __name__ == "__main__":
    run_test_suite()
    run_attack_benchmark(max_time=4)
