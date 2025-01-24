from .profile import *
from ..distribution import Distribution, d6
from typing import List, Optional, Callable
from fractions import Fraction
from ..dice import DiceFormula

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

def roll_for_attacks(profile: AttackProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
    """Roll to determine number of attacks"""
    # Get attack count distribution
    if isinstance(profile.attacks, DiceFormula):
        attack_dist = profile.attacks.roll()
    else:
        attack_dist = Distribution.singleton(profile.attacks)
        
    # Map to initial attack sequence
    return attack_dist.map(
        lambda n: AttackSequence({AttackStage.ATTACKS: n})
    )

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
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.HITS: 1})),  # Hit
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.HITS: 0}))   # Miss
    )

    single_hit_roll = apply_modifiers(single_hit_roll, AttackStage.HITS, profile, defender, modifiers)
    print(single_hit_roll)

    return roll_many_times(
        attack_dist, 
        AttackStage.ATTACKS, 
        AttackStage.HITS,
        single_hit_roll,
    )

def roll_to_wound(hit_dist: Distribution[AttackSequence], profile: AttackProfile, 
                 defender: Defender, modifiers: List[Modifier]) -> Distribution[AttackSequence]:

    threshold = get_wound_threshold(profile.strength, defender.get_highest_toughness())
    
    # Create distribution for a single wound roll
    single_wound_roll = d6.filter(
        pred=lambda x: x >= threshold,
        if_true=lambda x: DieResult(x, AttackSequence({AttackStage.WOUNDS: 1})),  # Wound
        if_false=lambda x: DieResult(x, AttackSequence({AttackStage.WOUNDS: 0}))   # Fail
    )

    single_wound_roll = apply_modifiers(single_wound_roll, AttackStage.WOUNDS, profile, defender, modifiers)

    return roll_many_times(
        hit_dist,
        AttackStage.HITS,
        AttackStage.WOUNDS,
        single_wound_roll,
    )

def unwrap_die_result(dist: Distribution[DieResult]) -> Distribution[AttackSequence]:
    return dist.map(lambda x: x.sequence if x.sequence else AttackSequence.create(0, AttackStage.DAMAGE))

def do_individual_dmg_sequence(defender: Defender, profile: AttackProfile, modifiers: List[Modifier]) -> Callable[[AttackSequence], Distribution[AttackSequence]]:
    def fn(state: AttackSequence) -> Distribution[AttackSequence]:
        
        def save_roll(state: AttackSequence) -> Distribution[AttackSequence]:
            current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
            if not current_defender_profile or state.get_value(AttackStage.WOUNDS) == 0:
                return Distribution.singleton(state)
            next_state = state.with_value(AttackStage.WOUNDS, state.get_value(AttackStage.WOUNDS) - 1)
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

        def damage_roll(state: AttackSequence) -> Distribution[AttackSequence]:
            current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
            if not current_defender_profile or state.get_value(AttackStage.FAILED_SAVES) == 0:
                return Distribution.singleton(state)
            
            damage_dist = profile.damage.roll() if isinstance(profile.damage, DiceFormula) else Distribution.singleton(profile.damage)
            
            damage_rolls = damage_dist.map(lambda n: DieResult(n, AttackSequence({AttackStage.DAMAGE: n})))
            damage_rolls = apply_modifiers(damage_rolls, AttackStage.DAMAGE, profile, defender, modifiers)
            damage_rolls = unwrap_die_result(damage_rolls)

            # result = roll_many_times(
            #     Distribution.singleton(state),
            #     AttackStage.FAILED_SAVES,
            #     AttackStage.DAMAGE,
            #     damage_rolls,
            #     keep_history=False,
            # )

            def perform_damage_repeat(seq: AttackSequence) -> Distribution[AttackSequence]:
                amount = seq.get_value(AttackStage.FAILED_SAVES)
                if amount == 0:
                    return Distribution.singleton(seq)
                return damage_rolls.repeated(amount)
            
            result = perform_damage_repeat(state)
            # print("dmg result", result)
            return result.map(lambda x: x + state.freeze_all(AttackStage.FAILED_SAVES))



        def fnp_roll(state: AttackSequence) -> Distribution[AttackSequence]:
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
            fnp_roll = unwrap_die_result(fnp_roll)

            # result = roll_many_times(
            #     Distribution.singleton(state),
            #     AttackStage.DAMAGE,
            #     AttackStage.FELT_DMG,
            #     fnp_roll,
            #     keep_history=False,
            # )

            def perform_fnp_repeat(seq: AttackSequence) -> Distribution[AttackSequence]:
                amount = seq.get_value(AttackStage.DAMAGE)
                # print("amount", amount)

                if amount == 0:
                    return Distribution.singleton(seq)
                # print("repeated", fnp_roll.repeated(amount))
                return fnp_roll.repeated(amount)
            
            result = perform_fnp_repeat(state)
            # print("result", result)
       
            # wound_cap = defender.get_wound_cap(state.get_value(AttackStage.MODELS_SLAIN))
            # print("wound_cap", wound_cap)
            # def cap_dmg_felt(seq: AttackSequence) -> AttackSequence:
            #     return seq.with_value(
            #         AttackStage.FELT_DMG,
            #         min(seq.get_value(AttackStage.FELT_DMG), wound_cap),
            #     )

            # result = result.map(cap_dmg_felt)
            return result.map(lambda x: x + state.freeze_all(AttackStage.DAMAGE))

        def slay_models(state: AttackSequence) -> AttackSequence:
            current_defender_profile = defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN))
            if not current_defender_profile or state.get_value(AttackStage.FELT_DMG) == 0:
                return state
            defender_wounds = current_defender_profile.wounds
            if state.get_value(AttackStage.FELT_DMG) >= defender_wounds:
                return state.with_value(AttackStage.MODELS_SLAIN, state.get_value(AttackStage.MODELS_SLAIN) + 1) \
                    .freeze_all(AttackStage.FELT_DMG)
            return state

        do_saves = save_roll(state)
        # print("do_saves", do_saves)
        do_damage = do_saves.bind(damage_roll)
        # print("do_damage", do_damage)
        do_fnp = do_damage.bind(fnp_roll)
        # print("do_fnp", do_fnp)
        do_slay = do_fnp.map(slay_models)
        # print("do_slay", do_slay)

        
        return do_slay

    return fn

def do_dmg_sequence(wound_dist: Distribution[AttackSequence], defender: Defender, profile: AttackProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
    distribution_transform = do_individual_dmg_sequence(defender, profile, modifiers)
    def do_until_exhausted(state: AttackSequence) -> Distribution[AttackSequence]:
        print("do_until_exhausted", state)
        if state.get_value(AttackStage.WOUNDS) == 0:
            return Distribution.singleton(state)
        if defender.get_next_profile(state.get_value(AttackStage.MODELS_SLAIN)) is None:
            return Distribution.singleton(state)
        return distribution_transform(state).bind(do_until_exhausted)
    
    return wound_dist.bind(do_until_exhausted)


# def roll_to_save(wound_dist: Distribution[AttackSequence], profile: AttackProfile, 
#                 defender: Defender, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
#     """Roll armor/invuln saves for each wound"""
#     # Determine best save available
#     save_value = defender.armor_save + profile.armor_pen
#     if defender.invuln_save is not None:
#         save_value = min(save_value, defender.invuln_save)
#     # Create distribution for a single save roll
#     single_save_roll = d6.filter(
#         pred=lambda x: x >= save_value,
#         if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 0})),  # Save successful - no damage
#         if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FAILED_SAVES: 1}))   # Save failed - wound goes through
#     )

#     all_modifiers = profile.modifiers + defender.modifiers + modifiers

#     for modifier in all_modifiers:
#         single_save_roll = modifier.modify_roll(single_save_roll, AttackStage.FAILED_SAVES, profile, defender)
    
#     return roll_many_times(
#         wound_dist,
#         AttackStage.WOUNDS,
#         AttackStage.FAILED_SAVES,
#         single_save_roll,
#     )

# def roll_feel_no_pain(save_dist: Distribution[AttackSequence], profile: AttackProfile, 
#                      defender: DefenderProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
#     """Roll Feel No Pain saves for each failed save"""
#     fnp = 7
#     if defender.feel_no_pain:
#         fnp = defender.feel_no_pain
        
#     # Create distribution for a single FNP roll
#     single_fnp_roll = d6.filter(
#         pred=lambda x: x >= fnp,
#         if_true=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 0})),  # FNP successful - no damage
#         if_false=lambda x: DieResult(x, AttackSequence({AttackStage.FELT_DMG: 1}))   # FNP failed - damage goes through
#     )
    
#     return roll_many_times(
#         save_dist,
#         AttackStage.DAMAGE,
#         AttackStage.FELT_DMG,
#         single_fnp_roll,
#     )

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

def roll_damage(save_dist: Distribution[AttackSequence], profile: AttackProfile, 
                defender: DefenderProfile, modifiers: List[Modifier]) -> Distribution[AttackSequence]:
    """Roll damage for each failed save"""
    # Get damage distribution
    if isinstance(profile.damage, DiceFormula):
        damage_dist = profile.damage.roll()
    else:
        damage_dist = Distribution.singleton(profile.damage)
        
    # Map to damage sequence
    damage_roll = damage_dist.map(
        lambda n: DieResult(n, AttackSequence({AttackStage.DAMAGE: n}))
    )
    
    return roll_many_times(
        save_dist,
        AttackStage.FAILED_SAVES,
        AttackStage.DAMAGE,
        damage_roll,
    )

def full_demo():
    attack_profile = LASGUN_BARRAGE
    defender: Defender = GUARDSMAN
    
    modifiers: List[Modifier] = []

    all_modifiers = attack_profile.modifiers + defender.get_modifiers() + modifiers
    for modifier in all_modifiers:
        attack_profile = modifier.modify_attacker(attack_profile, defender)
        defender = modifier.modify_defender(attack_profile, defender)

    attack_dist = roll_for_attacks(attack_profile, modifiers)
    print("Number of attacks:")
    print(attack_dist)
    
    hit_dist = roll_to_hit(attack_dist, attack_profile, defender, modifiers)
    print("\nAfter hit rolls:")
    print(hit_dist)
    print("\nCollapsed hit rolls:")
    print(collapse_stage(hit_dist, AttackStage.HITS))
    
    wound_dist = roll_to_wound(hit_dist, attack_profile, defender, modifiers)
    print("\nAfter wound rolls:")
    print(wound_dist)
    print("\nCollapsed wound rolls:")
    print(collapse_stage(wound_dist, AttackStage.WOUNDS))
    
    # save_dist = roll_to_save(wound_dist, attack_profile, defender, modifiers)
    # print("\nAfter save rolls:")
    # print(save_dist)
    
    # damage_dist = roll_damage(save_dist, attack_profile, defender, modifiers)
    # print("\nAfter damage rolls:")
    # print(damage_dist)
    # print("\nCollapsed damage:")
    # print(collapse_stage(damage_dist, AttackStage.DAMAGE))
    
    # fnp_dist = roll_feel_no_pain(damage_dist, attack_profile, defender, modifiers)
    # print("\nAfter Feel No Pain rolls:")
    # print(fnp_dist) 

    dmg_dist = do_dmg_sequence(wound_dist, defender, attack_profile, modifiers)
    print("\nAfter damage rolls:")
    print(dmg_dist)

    print("\nCollapsed failed saves:")
    print(collapse_stage(dmg_dist, AttackStage.FAILED_SAVES))

    print("\nCollapsed damage:")
    print(collapse_stage(dmg_dist, AttackStage.DAMAGE))

    print("\nCollapsed felt damage:")
    print(collapse_stage(dmg_dist, AttackStage.FELT_DMG))

    print("\nCollapsed slain models:")
    print(collapse_stage(dmg_dist, AttackStage.MODELS_SLAIN))

    

if __name__ == "__main__":
    full_demo()

