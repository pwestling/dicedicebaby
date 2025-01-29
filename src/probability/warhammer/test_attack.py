from probability.warhammer.attack import *
from probability.warhammer.profile import *
from probability.distribution import Distribution
from dataclasses import dataclass, asdict
from typing import List, Dict, Any, Optional
import json
import sys
import math


@dataclass
class ExpectedProbability:
    stage: AttackStage
    value: int
    probability: float
    tolerance: float = 0.0001

@dataclass
class TestCase:
    name: str
    attack_profile: AttackProfile
    defender: Defender
    modifiers: List[Modifier]
    expected: List[ExpectedProbability]



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
            ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.1948847471315175),
            ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.35900679066688196),
            ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.2834269531114777),
            ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.1243067270366254),
            ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.03270970224169408),
            ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.005161607903314754),
            ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.00045021681280629815),
            ExpectedProbability(AttackStage.FAILED_SAVES, 7, 1.5287448355867913e-05),

            # MORTAL_WOUNDS
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 0.5438466243349526),
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 1, 0.3460832050790171),
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 2, 0.09438308886321625),
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 3, 0.014294785467471819),
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 4, 0.0012921013951897086),
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 5, 6.222721282617095e-05),
            
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
    ),
    TestCase(
        name="10 attacks damage 3 vs 2 wounds",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(10),
            ballistic_skill=4,
            strength=4,
            armor_pen=1,
            damage=DiceFormula.constant(3),
            modifiers=[],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=5,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=2,
                feel_no_pain=None,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        ),
        modifiers=[],
        expected=[
            # HITS
            ExpectedProbability(AttackStage.HITS, 0, 0.0009765625),
            ExpectedProbability(AttackStage.HITS, 1, 0.009765625),
            ExpectedProbability(AttackStage.HITS, 2, 0.0439453125),
            ExpectedProbability(AttackStage.HITS, 3, 0.1171875),
            ExpectedProbability(AttackStage.HITS, 4, 0.205078125),
            ExpectedProbability(AttackStage.HITS, 5, 0.24609375),
            ExpectedProbability(AttackStage.HITS, 6, 0.205078125),
            ExpectedProbability(AttackStage.HITS, 7, 0.1171875),
            ExpectedProbability(AttackStage.HITS, 8, 0.0439453125),
            ExpectedProbability(AttackStage.HITS, 9, 0.009765625),
            ExpectedProbability(AttackStage.HITS, 10, 0.0009765625),
            # WOUNDS
            ExpectedProbability(AttackStage.WOUNDS, 0, 0.056313514709472656),
            ExpectedProbability(AttackStage.WOUNDS, 1, 0.1877117156982422),
            ExpectedProbability(AttackStage.WOUNDS, 2, 0.2815675735473633),
            ExpectedProbability(AttackStage.WOUNDS, 3, 0.25028228759765625),
            ExpectedProbability(AttackStage.WOUNDS, 4, 0.1459980010986328),
            ExpectedProbability(AttackStage.WOUNDS, 5, 0.058399200439453125),
            ExpectedProbability(AttackStage.WOUNDS, 6, 0.016222000122070312),
            ExpectedProbability(AttackStage.WOUNDS, 7, 0.00308990478515625),
            ExpectedProbability(AttackStage.WOUNDS, 8, 0.00038623809814453125),
            ExpectedProbability(AttackStage.WOUNDS, 9, 2.86102294921875e-05),
            ExpectedProbability(AttackStage.WOUNDS, 10, 9.5367431640625e-07),
            # FAILED_SAVES
            ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.3230101973922164),
            ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.29070983698338637),
            ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.15504436728395063),
            ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.05426582158511231),
            ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.013023679966135116),
            ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.0021704179705361224),
            ExpectedProbability(AttackStage.FAILED_SAVES, 7, 0.0002478245027434842),
            ExpectedProbability(AttackStage.FAILED_SAVES, 8, 1.8419388717421123e-05),
            
            # MORTAL_WOUNDS
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 1),
            
            # DAMAGE
            ExpectedProbability(AttackStage.DAMAGE, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.DAMAGE, 3, 0.3230101973922164),
            ExpectedProbability(AttackStage.DAMAGE, 6, 0.29070983698338637),
            ExpectedProbability(AttackStage.DAMAGE, 9, 0.15504436728395063),
            ExpectedProbability(AttackStage.DAMAGE, 12, 0.05426582158511231),
            ExpectedProbability(AttackStage.DAMAGE, 15, 0.015460341828132145),
            # FELT_DMG
            ExpectedProbability(AttackStage.FELT_DMG, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.FELT_DMG, 2, 0.3230101973922164),
            ExpectedProbability(AttackStage.FELT_DMG, 4, 0.29070983698338637),
            ExpectedProbability(AttackStage.FELT_DMG, 6, 0.15504436728395063),
            ExpectedProbability(AttackStage.FELT_DMG, 8, 0.05426582158511231),
            ExpectedProbability(AttackStage.FELT_DMG, 10, 0.015460341828132145),
            # MODELS_SLAIN
            ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.3230101973922164),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.29070983698338637),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 0.15504436728395063),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 4, 0.05426582158511231),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 5, 0.015460341828132145),
        ]
    ),
     TestCase(
        name="10 attacks damage 3 vs 4 wounds",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(10),
            ballistic_skill=4,
            strength=4,
            armor_pen=1,
            damage=DiceFormula.constant(3),
            modifiers=[],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=5,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=4,
                feel_no_pain=None,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        ),
        modifiers=[],
        expected=[
            # HITS
            ExpectedProbability(AttackStage.HITS, 0, 0.0009765625),
            ExpectedProbability(AttackStage.HITS, 1, 0.009765625),
            ExpectedProbability(AttackStage.HITS, 2, 0.0439453125),
            ExpectedProbability(AttackStage.HITS, 3, 0.1171875),
            ExpectedProbability(AttackStage.HITS, 4, 0.205078125),
            ExpectedProbability(AttackStage.HITS, 5, 0.24609375),
            ExpectedProbability(AttackStage.HITS, 6, 0.205078125),
            ExpectedProbability(AttackStage.HITS, 7, 0.1171875),
            ExpectedProbability(AttackStage.HITS, 8, 0.0439453125),
            ExpectedProbability(AttackStage.HITS, 9, 0.009765625),
            ExpectedProbability(AttackStage.HITS, 10, 0.0009765625),
            # WOUNDS
            ExpectedProbability(AttackStage.WOUNDS, 0, 0.056313514709472656),
            ExpectedProbability(AttackStage.WOUNDS, 1, 0.1877117156982422),
            ExpectedProbability(AttackStage.WOUNDS, 2, 0.2815675735473633),
            ExpectedProbability(AttackStage.WOUNDS, 3, 0.25028228759765625),
            ExpectedProbability(AttackStage.WOUNDS, 4, 0.1459980010986328),
            ExpectedProbability(AttackStage.WOUNDS, 5, 0.058399200439453125),
            ExpectedProbability(AttackStage.WOUNDS, 6, 0.016222000122070312),
            ExpectedProbability(AttackStage.WOUNDS, 7, 0.00308990478515625),
            ExpectedProbability(AttackStage.WOUNDS, 8, 0.00038623809814453125),
            ExpectedProbability(AttackStage.WOUNDS, 9, 2.86102294921875e-05),
            ExpectedProbability(AttackStage.WOUNDS, 10, 9.5367431640625e-07),
            # FAILED_SAVES
            ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.32301113929277586),
            ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.2907100462946218),
            ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.15504534406971593),
            ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.05426582158511231),
            ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.013023810204237159),
            ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.0021706350340395266),
            ExpectedProbability(AttackStage.FAILED_SAVES, 7, 0.000248072575318803),
            ExpectedProbability(AttackStage.FAILED_SAVES, 8, 1.8605443148910226e-05),
            ExpectedProbability(AttackStage.FAILED_SAVES, 9, 7.442177259564091e-07),
            # MORTAL_WOUNDS
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 1),
            # DAMAGE
            ExpectedProbability(AttackStage.DAMAGE, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.DAMAGE, 3, 0.32301113929277586),
            ExpectedProbability(AttackStage.DAMAGE, 6, 0.2907100462946218),
            ExpectedProbability(AttackStage.DAMAGE, 9, 0.15504534406971593),
            ExpectedProbability(AttackStage.DAMAGE, 12, 0.05426582158511231),
            ExpectedProbability(AttackStage.DAMAGE, 15, 0.013023810204237159),
            ExpectedProbability(AttackStage.DAMAGE, 18, 0.0021706350340395266),
            ExpectedProbability(AttackStage.DAMAGE, 21, 0.000248072575318803),
            ExpectedProbability(AttackStage.DAMAGE, 24, 1.8605443148910226e-05),
            ExpectedProbability(AttackStage.DAMAGE, 27, 7.442177259564091e-07),
            # FELT_DMG
            ExpectedProbability(AttackStage.FELT_DMG, 0, 0.16150552255135994),
            ExpectedProbability(AttackStage.FELT_DMG, 3, 0.32301113929277586),
            ExpectedProbability(AttackStage.FELT_DMG, 4, 0.2907100462946218),
            ExpectedProbability(AttackStage.FELT_DMG, 7, 0.15504534406971593),
            ExpectedProbability(AttackStage.FELT_DMG, 8, 0.05426582158511231),
            ExpectedProbability(AttackStage.FELT_DMG, 11, 0.013023810204237159),
            ExpectedProbability(AttackStage.FELT_DMG, 12, 0.0021706350340395266),
            ExpectedProbability(AttackStage.FELT_DMG, 15, 0.000248072575318803),
            ExpectedProbability(AttackStage.FELT_DMG, 16, 1.8605443148910226e-05),
            ExpectedProbability(AttackStage.FELT_DMG, 19, 7.442177259564091e-07),
            # MODELS_SLAIN
            ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.4845166618441358),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.44575539036433776),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.06728963178934948),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 0.0024187076093583294),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 4, 1.9349660874866636e-05),
        ]
    ),
         TestCase(
        name="20 attacks damage 2 vs 9 wounds",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(20),
            ballistic_skill=4,
            strength=6,
            armor_pen=1,
            damage=DiceFormula.constant(3),
            modifiers=[],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=5,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=9,
                feel_no_pain=None,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        ),
        modifiers=[],
        expected=[
          
        ]
    )
]

slow_test_cases = [
    TestCase(
        name="60 attacks damage 1d3 vs 3 wounds SUPER SLOW",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(60),
            ballistic_skill=4,
            strength=4,
            armor_pen=1,
            damage=DiceFormula(0, 0, 3),
            modifiers=[RerollAllFails(AttackStage.HITS), RerollAllFails(AttackStage.WOUNDS), LethalHits(), DevastatingWounds()],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model Hardy",
                models=15,
                toughness=4,
                armor_save=2,
                invuln_save=3,
                wounds=2,
                feel_no_pain=5,
                modifiers=[],
                keywords=[],
                is_leader=False
            ),DefenderProfile(
                name="Test Model Soft",
                models=15,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=3,
                feel_no_pain=4,
                modifiers=[],
                keywords=[],
                is_leader=False
            ),]
        ),
        modifiers=[],
        expected=[]
    )
]

def run_test_case(test: TestCase) -> bool:
    print(f"\nRunning test: {test.name}")
    results = simulate_attacks(test.attack_profile, test.defender, test.modifiers)
    all_passed = True
    for exp in test.expected:
        dist = results.damage
        if exp.stage == AttackStage.HITS:
            dist = results.collapsed_hits
        elif exp.stage == AttackStage.WOUNDS:
            dist = results.collapsed_wounds
        elif exp.stage == AttackStage.FAILED_SAVES:
            dist = results.collapsed_failed_saves
        elif exp.stage == AttackStage.DAMAGE:
            dist = results.collapsed_damage
        elif exp.stage == AttackStage.MODELS_SLAIN:
            dist = results.collapsed_slain_models
        elif exp.stage == AttackStage.FELT_DMG:
            dist = results.collapsed_felt_damage
        elif exp.stage == AttackStage.MORTAL_WOUNDS:
            dist = results.collapsed_mortal_wounds
            
        actual = float(dist.probabilities.get(AttackSequence.create(exp.value, exp.stage), 0))
        if abs(actual - exp.probability) > exp.tolerance:
            print(f"Failed: {exp.stage.name}={exp.value} expected {exp.probability:.4f} but got {actual:.4f}")
            all_passed = False
    if all_passed:
        print(f"Passed: {test.name} in {results.timing.total_time:.3f} seconds")
        if len(test.expected) == 0:
            print(results)
            print_test_expectations(results)
    else:
        print(f"Failed: {test.name} with results: \n{results}")
        print_test_expectations(results)
    return all_passed

def run_test_suite() -> None:
    all_passed = True
    all_tests = []
    all_tests.extend(test_cases)
    # all_tests.extend(slow_test_cases)

    failed_tests = []
    passed_tests = []
    
    for test in all_tests:
        if not run_test_case(test):
            all_passed = False
            failed_tests.append(test)
            break
        else:
            passed_tests.append(test)
    
    if all_passed:
        print("\nAll tests passed!")
    else:
        print("\nSome tests failed!")
        print(f"Failed tests: {[test.name for test in failed_tests]}")
        print(f"Passed tests: {[test.name for test in passed_tests]}")
        exit(1)
    

def run_attack_benchmark2() -> None:
    """Run increasingly large attack counts until simulation takes longer than max_time seconds"""
    last_time = 0
    attacks = 30

    defender = Defender(
        name="Test Defender",
        profiles=[DefenderProfile(
            name="Test Model",
            models=20,
            toughness=3,
            armor_save=4,
            invuln_save=None,
            wounds=1,
            feel_no_pain=None,
            modifiers=[],
            keywords=[],
            is_leader=False
        )]
    )
    profile = AttackProfile(
        name=f"Test Profile ({attacks} attacks)",
        models=1,
        guns_per_model=1,
        attacks=DiceFormula.constant(attacks),
        ballistic_skill=4,
        strength=3,
        armor_pen=0,
        damage=DiceFormula.constant(1),
        modifiers=[],
        keywords=[]
    )
    
    results = simulate_attacks(profile, defender)
    last_time = results.timing.total_time
    print(f"Simulated {attacks} attacks in {last_time:.3f} seconds")

    profile.modifiers = [RerollAllFails(AttackStage.HITS)]
    results = simulate_attacks(profile, defender)
    last_time = results.timing.total_time
    print(f"Simulated {attacks} attacks with rerolls in {last_time:.3f} seconds")

    print(results.timing)

def print_test_expectations(results: AttackResults) -> None:
    """Print test expectations in a format that can be pasted into a test case"""
    stages = [
        (AttackStage.HITS, results.collapsed_hits),
        (AttackStage.WOUNDS, results.collapsed_wounds),
        (AttackStage.FAILED_SAVES, results.collapsed_failed_saves),
        (AttackStage.MORTAL_WOUNDS, results.collapsed_mortal_wounds),
        (AttackStage.DAMAGE, results.collapsed_damage),
        (AttackStage.FELT_DMG, results.collapsed_felt_damage),
        (AttackStage.MODELS_SLAIN, results.collapsed_slain_models)
    ]
    
    print("expected_probabilities=[")
    for stage, dist in stages:
        print(f"    # {stage.name}")
        for seq, prob in sorted(dist.probabilities.items()):
            print(f"    ExpectedProbability(AttackStage.{stage.name}, {seq.get_value(stage)}, {float(prob)}),")
    print("]")

if __name__ == "__main__":
    run_test_suite()