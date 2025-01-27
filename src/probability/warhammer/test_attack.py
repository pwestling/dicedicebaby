from probability.warhammer.attack import *
from probability.warhammer.profile import *
from probability.distribution import Distribution
from dataclasses import dataclass, asdict
from typing import List, Dict, Any
import json
import sys

@dataclass
class ExpectedProbability:
    stage: AttackStage
    value: int
    probability: float
    tolerance: float = 0.001

@dataclass
class TestCase:
    name: str
    attack_profile: AttackProfile
    defender: Defender
    modifiers: List[Modifier]
    expected: List[ExpectedProbability]

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
    
    test_cases = [
        TestCase(
            name="Basic attack sequence",
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
                # ... rest of probabilities ...
                # (Include all the probabilities from the previous test case)
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
            },
            "defender": {
                "toughness": test.defender.profiles[0].toughness,
                "armor_save": test.defender.profiles[0].armor_save,
                "invuln_save": test.defender.profiles[0].invuln_save,
                "wounds": test.defender.profiles[0].wounds,
                "models": test.defender.profiles[0].models,
                "feel_no_pain": test.defender.profiles[0].feel_no_pain
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
        test_cases: List[TestCase] = [
            # Basic attack test
            TestCase(
                name="Basic bolter attack",
                attack_profile=AttackProfile(
                    name="Bolter",
                    models=1,
                    guns_per_model=1,
                    attacks=DiceFormula.constant(1),
                    ballistic_skill=3,
                    strength=4,
                    armor_pen=0,
                    damage=DiceFormula.constant(1),
                    modifiers=[],
                    keywords=[]
                ),
                defender=Defender(
                    name="Space Marine",
                    profiles=[DefenderProfile(
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
                    )]
                ),
                modifiers=[],
                expected=[
                    ExpectedProbability(AttackStage.HITS, 1, 2/3),
                    ExpectedProbability(AttackStage.WOUNDS, 1, 1/3),
                    ExpectedProbability(AttackStage.FAILED_SAVES, 1, 1/9),
                    ExpectedProbability(AttackStage.DAMAGE, 1, 1/9),
                ]
            ),
            
            # Test with rerolls
            TestCase(
                name="Bolter with rerolls",
                attack_profile=AttackProfile(
                    name="Bolter",
                    models=1,
                    guns_per_model=1,
                    attacks=DiceFormula.constant(1),
                    ballistic_skill=3,
                    strength=4,
                    armor_pen=0,
                    damage=DiceFormula.constant(1),
                    modifiers=[RerollAllFails(AttackStage.HITS)],
                    keywords=[]
                ),
                defender=Defender(
                    name="Space Marine",
                    profiles=[DefenderProfile(
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
                    )]
                ),
                modifiers=[],
                expected=[
                    ExpectedProbability(AttackStage.HITS, 1, 8/9),
                    ExpectedProbability(AttackStage.WOUNDS, 1, 4/9),
                    ExpectedProbability(AttackStage.FAILED_SAVES, 1, 4/27),
                ]
            ),

            # Test with Lethal Hits
            TestCase(
                name="Lethal Hits test",
                attack_profile=AttackProfile(
                    name="Lethal Weapon",
                    models=1,
                    guns_per_model=1,
                    attacks=DiceFormula.constant(1),
                    ballistic_skill=3,
                    strength=4,
                    armor_pen=0,
                    damage=DiceFormula.constant(1),
                    modifiers=[LethalHits()],
                    keywords=[]
                ),
                defender=Defender(
                    name="Space Marine",
                    profiles=[DefenderProfile(
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
                    )]
                ),
                modifiers=[],
                expected=[
                    ExpectedProbability(AttackStage.HITS, 1, 2/3),
                    ExpectedProbability(AttackStage.WOUNDS, 1, 1/3 + 1/6),  # Normal wounds + lethal hits
                ]
            ),

            # Test with Devastating Wounds
            TestCase(
                name="Devastating Wounds test",
                attack_profile=AttackProfile(
                    name="Devastating Weapon",
                    models=1,
                    guns_per_model=1,
                    attacks=DiceFormula.constant(1),
                    ballistic_skill=3,
                    strength=4,
                    armor_pen=0,
                    damage=DiceFormula.constant(1),
                    modifiers=[DevastatingWounds()],
                    keywords=[]
                ),
                defender=Defender(
                    name="Space Marine",
                    profiles=[DefenderProfile(
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
                    )]
                ),
                modifiers=[],
                expected=[
                    ExpectedProbability(AttackStage.HITS, 1, 2/3),
                    ExpectedProbability(AttackStage.WOUNDS, 1, 1/3),
                    ExpectedProbability(AttackStage.FAILED_SAVES, 1, 1/9 + 1/18),  # Normal fails + devastating wounds
                ]
            ),
        ]
        
        # Write test cases to stdout as JSON
        for test in test_cases:
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
                    print(f"Test {test.name} failed: {result.get('error')}", file=sys.stderr)
                    sys.exit(1)
            except json.JSONDecodeError:
                print(f"Invalid JSON response: {result_line}", file=sys.stderr)
                sys.exit(1)

if __name__ == "__main__":
    TestRunner.run_tests_standalone() 