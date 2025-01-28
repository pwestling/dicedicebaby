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
    tolerance: float = 0.00001

@dataclass
class TestCase:
    name: str
    attack_profile: AttackProfile
    defender: Defender
    modifiers: List[Modifier]
    expected: List[ExpectedProbability]

@dataclass
class Distribution:
    probabilities: Dict[str, float]

@dataclass
class AttackResults:
    attacks: Distribution
    hits: Distribution
    wounds: Distribution
    damage: Distribution
    timing: Optional[Dict[str, float]]

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
            armor_pen=-1,
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
            # Hit results
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
            
            # Wound results
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
            
            # Failed saves
            ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.41890312712869526),
            ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.38082163356786236),
            ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.15579051605140884),
            ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.03776719486217386),
            ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.006008239767024874),
            ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.0006553377797737697),
            ExpectedProbability(AttackStage.FAILED_SAVES, 6, 4.862561637972608e-05),
            ExpectedProbability(AttackStage.FAILED_SAVES, 7, 1.4128508391203703e-06),
            
            # Damage results
            ExpectedProbability(AttackStage.DAMAGE, 0, 0.41890312712869526),
            ExpectedProbability(AttackStage.DAMAGE, 3, 0.38082163356786236),
            ExpectedProbability(AttackStage.DAMAGE, 6, 0.15579051605140884),
            ExpectedProbability(AttackStage.DAMAGE, 9, 0.03776719486217386),
            ExpectedProbability(AttackStage.DAMAGE, 12, 0.006008239767024874),
            ExpectedProbability(AttackStage.DAMAGE, 15, 0.0007053762469926162),
            
            # Models slain
            ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.41890312712869526),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.38082163356786236),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.15579051605140884),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 0.03776719486217386),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 4, 0.006008239767024874),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 5, 0.0007053762469926162),
        ]
    )
]

slow_test_cases = [
    TestCase(
        name="10 attacks damage 5 vs 3 wounds",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(60),
            ballistic_skill=4,
            strength=4,
            armor_pen=-1,
            damage=DiceFormula(1, 3, 1),
            modifiers=[RerollAllFails(AttackStage.HITS), RerollAllFails(AttackStage.WOUNDS), LethalHits(), DevastatingWounds()],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=100,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=3,
                feel_no_pain=4,
                modifiers=[],
                keywords=[],
                is_leader=False
            ),DefenderProfile(
                name="Test Model",
                models=4,
                toughness=4,
                armor_save=2,
                invuln_save=3,
                wounds=3,
                feel_no_pain=5,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
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
            
        actual = float(dist.probabilities.get(AttackSequence.create(exp.value, exp.stage), 0))
        if abs(actual - exp.probability) > exp.tolerance:
            print(f"Failed: {exp.stage.name}={exp.value} expected {exp.probability:.4f} but got {actual:.4f}")
            all_passed = False
    return all_passed

def run_test_suite() -> None:
    prev_prune_factor = Distribution.PRUNE_FACTOR
    all_passed = True
    for test in test_cases:
        if not run_test_case(test):
            all_passed = False
    
    if all_passed:
        print("\nAll tests passed!")
    else:
        print("\nSome tests failed!")
        exit(1)
    
    Distribution.PRUNE_FACTOR = prev_prune_factor

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

class TestRunner:
    @staticmethod
    def serialize_test_case(test: TestCase) -> Dict[str, Any]:
        """Convert a test case to JSON-serializable format"""
        return {
            "name": test.name,
            "attack": {
                "attacks": test.attack_profile.attacks.to_dict(),
                "ballistic_skill": test.attack_profile.ballistic_skill,
                "strength": test.attack_profile.strength,
                "armor_pen": test.attack_profile.armor_pen,
                "damage": test.attack_profile.damage.to_dict(),
                "modifiers": [
                    {
                        "type": mod.__class__.__name__,
                        "stage": mod.stage.name if hasattr(mod, "stage") else None
                    }
                    for mod in test.attack_profile.modifiers
                ]
            },
            "defender": {
                "toughness": test.defender.profiles[0].toughness,
                "armor_save": test.defender.profiles[0].armor_save,
                "invuln_save": test.defender.profiles[0].invuln_save,
                "wounds": test.defender.profiles[0].wounds,
                "models": test.defender.profiles[0].models,
                "feel_no_pain": test.defender.profiles[0].feel_no_pain,
                "modifiers": []  # Add defender modifiers if needed
            },
            "expected": [
                {
                    "stage": exp.stage.name,
                    "value": exp.value,
                    "probability": exp.probability,
                    "tolerance": exp.tolerance
                }
                for exp in test.expected
            ]
        }

    @staticmethod
    def run_tests_standalone() -> None:
        """Run tests by communicating via stdin/stdout"""
        test_cases.reverse()
        # Write test cases to stdout as JSON
        for test in test_cases + slow_test_cases:
            json.dump(TestRunner.serialize_test_case(test), sys.stdout)
            sys.stdout.write("\n")
            sys.stdout.flush()
            
            # Read results from stdin
            result_line = sys.stdin.readline()
            if not result_line:
                print("No response from probability calculator", file=sys.stderr)
                sys.exit(1)
                
            try:
                result = json.loads(result_line)

                if not result.get("success"):
                    print(f"Test {test.name} execution failed: {result.get('error')}", file=sys.stderr)
                    sys.exit(1)
            except json.JSONDecodeError:
                print(f"Invalid JSON response: {result_line}", file=sys.stderr)
                sys.exit(1)
         


if __name__ == "__main__":
    TestRunner.run_tests_standalone() 