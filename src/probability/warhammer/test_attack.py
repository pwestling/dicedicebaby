from probability.warhammer.attack import *
from probability.warhammer.unit_profile import *
from probability.distribution import Distribution
from dataclasses import dataclass, asdict
from typing import List, Dict, Any, Optional, ClassVar
import json
import sys
import math


@dataclass
class ExpectedProbability:

    tolerance: ClassVar[float] = 0.0001

    stage: AttackStage
    value: int
    probability: float

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
        name="20 attacks damage 2 vs 9 wounds multi saves",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(20),
            ballistic_skill=4,
            strength=6,
            armor_pen=1,
            damage=DiceFormula.constant(2),
            modifiers=[],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="Test Model",
                models=3,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=9,
                feel_no_pain=None,
                modifiers=[],
                keywords=[],
                is_leader=False
            ),
            DefenderProfile(
                name="Test Model 2",
                models=3,
                toughness=4,
                armor_save=3,
                invuln_save=None,
                wounds=9,
                feel_no_pain=5,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        ),
        modifiers=[],
        expected=[
            # HITS
            ExpectedProbability(AttackStage.HITS, 0, 9.5367431640625e-07),
            ExpectedProbability(AttackStage.HITS, 1, 1.9073486328125e-05),
            ExpectedProbability(AttackStage.HITS, 2, 0.0001811981201171875),
            ExpectedProbability(AttackStage.HITS, 3, 0.001087188720703125),
            ExpectedProbability(AttackStage.HITS, 4, 0.004620552062988281),
            ExpectedProbability(AttackStage.HITS, 5, 0.0147857666015625),
            ExpectedProbability(AttackStage.HITS, 6, 0.03696441650390625),
            ExpectedProbability(AttackStage.HITS, 7, 0.0739288330078125),
            ExpectedProbability(AttackStage.HITS, 8, 0.12013435363769531),
            ExpectedProbability(AttackStage.HITS, 9, 0.16017913818359375),
            ExpectedProbability(AttackStage.HITS, 10, 0.17619705200195312),
            ExpectedProbability(AttackStage.HITS, 11, 0.16017913818359375),
            ExpectedProbability(AttackStage.HITS, 12, 0.12013435363769531),
            ExpectedProbability(AttackStage.HITS, 13, 0.0739288330078125),
            ExpectedProbability(AttackStage.HITS, 14, 0.03696441650390625),
            ExpectedProbability(AttackStage.HITS, 15, 0.0147857666015625),
            ExpectedProbability(AttackStage.HITS, 16, 0.004620552062988281),
            ExpectedProbability(AttackStage.HITS, 17, 0.001087188720703125),
            ExpectedProbability(AttackStage.HITS, 18, 0.0001811981201171875),
            ExpectedProbability(AttackStage.HITS, 19, 1.9073486328125e-05),
            ExpectedProbability(AttackStage.HITS, 20, 9.5367431640625e-07),
            # WOUNDS
            ExpectedProbability(AttackStage.WOUNDS, 0, 0.0003007197847938619),
            ExpectedProbability(AttackStage.WOUNDS, 1, 0.0030072519463271987),
            ExpectedProbability(AttackStage.WOUNDS, 2, 0.014284606034753903),
            ExpectedProbability(AttackStage.WOUNDS, 3, 0.042853785044135353),
            ExpectedProbability(AttackStage.WOUNDS, 4, 0.09106437336454848),
            ExpectedProbability(AttackStage.WOUNDS, 5, 0.14570302944158192),
            ExpectedProbability(AttackStage.WOUNDS, 6, 0.1821287654297745),
            ExpectedProbability(AttackStage.WOUNDS, 7, 0.18212879189059714),
            ExpectedProbability(AttackStage.WOUNDS, 8, 0.1479796367959045),
            ExpectedProbability(AttackStage.WOUNDS, 9, 0.09865307355672125),
            ExpectedProbability(AttackStage.WOUNDS, 10, 0.05425920339259886),
            ExpectedProbability(AttackStage.WOUNDS, 11, 0.02466327426936312),
            ExpectedProbability(AttackStage.WOUNDS, 12, 0.00924872785101117),
            ExpectedProbability(AttackStage.WOUNDS, 13, 0.0028457624156957447),
            ExpectedProbability(AttackStage.WOUNDS, 14, 0.0007114406039239362),
            ExpectedProbability(AttackStage.WOUNDS, 15, 0.00014228812078478725),
            ExpectedProbability(AttackStage.WOUNDS, 16, 2.2232518872623005e-05),
            ExpectedProbability(AttackStage.WOUNDS, 17, 2.574721854733914e-06),
            ExpectedProbability(AttackStage.WOUNDS, 18, 2.0434300434396145e-07),
            # FAILED_SAVES
            ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.006563081486256541),
            ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.03750344067356038),
            ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.10179527802366092),
            ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.17450625713577228),
            ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.21190046961103562),
            ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.1937375645022345),
            ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.1383839093726914),
            ExpectedProbability(AttackStage.FAILED_SAVES, 7, 0.07907650704538823),
            ExpectedProbability(AttackStage.FAILED_SAVES, 8, 0.03671398818646753),
            ExpectedProbability(AttackStage.FAILED_SAVES, 9, 0.013986157739780063),
            ExpectedProbability(AttackStage.FAILED_SAVES, 10, 0.004395608687901603),
            ExpectedProbability(AttackStage.FAILED_SAVES, 11, 0.0011416632030519082),
            ExpectedProbability(AttackStage.FAILED_SAVES, 12, 0.00024454717313626635),
            ExpectedProbability(AttackStage.FAILED_SAVES, 13, 4.279648832757734e-05),
            ExpectedProbability(AttackStage.FAILED_SAVES, 14, 5.918750072859942e-06),
            ExpectedProbability(AttackStage.FAILED_SAVES, 15, 5.28023343906846e-07),
            # MORTAL_WOUNDS
            ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 0.9999977161026816),
            # DAMAGE
            ExpectedProbability(AttackStage.DAMAGE, 0, 0.006563081486256541),
            ExpectedProbability(AttackStage.DAMAGE, 2, 0.03750344067356038),
            ExpectedProbability(AttackStage.DAMAGE, 4, 0.10179527802366092),
            ExpectedProbability(AttackStage.DAMAGE, 6, 0.17450625713577228),
            ExpectedProbability(AttackStage.DAMAGE, 8, 0.21190046961103562),
            ExpectedProbability(AttackStage.DAMAGE, 10, 0.1937375645022345),
            ExpectedProbability(AttackStage.DAMAGE, 12, 0.1383839093726914),
            ExpectedProbability(AttackStage.DAMAGE, 14, 0.07907650704538823),
            ExpectedProbability(AttackStage.DAMAGE, 16, 0.03671398818646753),
            ExpectedProbability(AttackStage.DAMAGE, 18, 0.013986157739780063),
            ExpectedProbability(AttackStage.DAMAGE, 20, 0.004395608687901603),
            ExpectedProbability(AttackStage.DAMAGE, 22, 0.0011416632030519082),
            ExpectedProbability(AttackStage.DAMAGE, 24, 0.00024454717313626635),
            ExpectedProbability(AttackStage.DAMAGE, 26, 4.279648832757734e-05),
            ExpectedProbability(AttackStage.DAMAGE, 28, 5.918750072859942e-06),
            ExpectedProbability(AttackStage.DAMAGE, 30, 5.28023343906846e-07),
            # FELT_DMG
            ExpectedProbability(AttackStage.FELT_DMG, 0, 0.006563081486256541),
            ExpectedProbability(AttackStage.FELT_DMG, 2, 0.03750344067356038),
            ExpectedProbability(AttackStage.FELT_DMG, 4, 0.10179527802366092),
            ExpectedProbability(AttackStage.FELT_DMG, 6, 0.17450625713577228),
            ExpectedProbability(AttackStage.FELT_DMG, 8, 0.21190046961103562),
            ExpectedProbability(AttackStage.FELT_DMG, 9, 0.1937375645022345),
            ExpectedProbability(AttackStage.FELT_DMG, 11, 0.1383839093726914),
            ExpectedProbability(AttackStage.FELT_DMG, 13, 0.07907650704538823),
            ExpectedProbability(AttackStage.FELT_DMG, 15, 0.03671398818646753),
            ExpectedProbability(AttackStage.FELT_DMG, 17, 0.013986157739780063),
            ExpectedProbability(AttackStage.FELT_DMG, 18, 0.004395608687901603),
            ExpectedProbability(AttackStage.FELT_DMG, 20, 0.0011416632030519082),
            ExpectedProbability(AttackStage.FELT_DMG, 22, 0.00024454717313626635),
            ExpectedProbability(AttackStage.FELT_DMG, 24, 4.279648832757734e-05),
            ExpectedProbability(AttackStage.FELT_DMG, 26, 5.918750072859942e-06),
            ExpectedProbability(AttackStage.FELT_DMG, 27, 5.28023343906846e-07),
            # MODELS_SLAIN
            ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.5322685269302857),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.46189812684656173),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.005830534302490215),
            ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 5.28023343906846e-07),
        ]
    ), 
    TestCase(
        name="20 attacks damage 2 vs 10 wounds multi saves",
        attack_profile=AttackProfile(
            name="Test Profile",
            models=1,
            guns_per_model=1,
            attacks=DiceFormula.constant(20),
            ballistic_skill=4,
            strength=6,
            armor_pen=1,
            damage=DiceFormula.constant(2),
            modifiers=[],
            keywords=[]
        ),
        defender=Defender(
            name="Test Defender",
            profiles=[DefenderProfile(
                name="FirstModelNoFNP",
                models=3,
                toughness=4,
                armor_save=4,
                invuln_save=None,
                wounds=10,
                feel_no_pain=None,
                modifiers=[],
                keywords=[],
                is_leader=False
            ),
            DefenderProfile(
                name="SecondModelHasFNP",
                models=3,
                toughness=4,
                armor_save=3,
                invuln_save=None,
                wounds=10,
                feel_no_pain=5,
                modifiers=[],
                keywords=[],
                is_leader=False
            )]
        ),
        modifiers=[],
        expected=[
    # HITS
    ExpectedProbability(AttackStage.HITS, 0, 9.5367431640625e-07),
    ExpectedProbability(AttackStage.HITS, 1, 1.9073486328125e-05),
    ExpectedProbability(AttackStage.HITS, 2, 0.0001811981201171875),
    ExpectedProbability(AttackStage.HITS, 3, 0.001087188720703125),
    ExpectedProbability(AttackStage.HITS, 4, 0.004620552062988281),
    ExpectedProbability(AttackStage.HITS, 5, 0.0147857666015625),
    ExpectedProbability(AttackStage.HITS, 6, 0.03696441650390625),
    ExpectedProbability(AttackStage.HITS, 7, 0.0739288330078125),
    ExpectedProbability(AttackStage.HITS, 8, 0.12013435363769531),
    ExpectedProbability(AttackStage.HITS, 9, 0.16017913818359375),
    ExpectedProbability(AttackStage.HITS, 10, 0.17619705200195312),
    ExpectedProbability(AttackStage.HITS, 11, 0.16017913818359375),
    ExpectedProbability(AttackStage.HITS, 12, 0.12013435363769531),
    ExpectedProbability(AttackStage.HITS, 13, 0.0739288330078125),
    ExpectedProbability(AttackStage.HITS, 14, 0.03696441650390625),
    ExpectedProbability(AttackStage.HITS, 15, 0.0147857666015625),
    ExpectedProbability(AttackStage.HITS, 16, 0.004620552062988281),
    ExpectedProbability(AttackStage.HITS, 17, 0.001087188720703125),
    ExpectedProbability(AttackStage.HITS, 18, 0.0001811981201171875),
    ExpectedProbability(AttackStage.HITS, 19, 1.9073486328125e-05),
    ExpectedProbability(AttackStage.HITS, 20, 9.5367431640625e-07),
    # WOUNDS
    ExpectedProbability(AttackStage.WOUNDS, 0, 0.0003007197847938619),
    ExpectedProbability(AttackStage.WOUNDS, 1, 0.0030072519463271987),
    ExpectedProbability(AttackStage.WOUNDS, 2, 0.014284606034753903),
    ExpectedProbability(AttackStage.WOUNDS, 3, 0.042853785044135353),
    ExpectedProbability(AttackStage.WOUNDS, 4, 0.09106437336454848),
    ExpectedProbability(AttackStage.WOUNDS, 5, 0.14570302944158192),
    ExpectedProbability(AttackStage.WOUNDS, 6, 0.1821287654297745),
    ExpectedProbability(AttackStage.WOUNDS, 7, 0.18212879189059714),
    ExpectedProbability(AttackStage.WOUNDS, 8, 0.1479796367959045),
    ExpectedProbability(AttackStage.WOUNDS, 9, 0.09865307355672125),
    ExpectedProbability(AttackStage.WOUNDS, 10, 0.05425920339259886),
    ExpectedProbability(AttackStage.WOUNDS, 11, 0.02466327426936312),
    ExpectedProbability(AttackStage.WOUNDS, 12, 0.00924872785101117),
    ExpectedProbability(AttackStage.WOUNDS, 13, 0.0028457624156957447),
    ExpectedProbability(AttackStage.WOUNDS, 14, 0.0007114406039239362),
    ExpectedProbability(AttackStage.WOUNDS, 15, 0.00014228812078478725),
    ExpectedProbability(AttackStage.WOUNDS, 16, 2.2232518872623005e-05),
    ExpectedProbability(AttackStage.WOUNDS, 17, 2.574721854733914e-06),
    ExpectedProbability(AttackStage.WOUNDS, 18, 2.0434300434396145e-07),
    # FAILED_SAVES
    ExpectedProbability(AttackStage.FAILED_SAVES, 0, 0.006563081486256541),
    ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.03750347547978666),
    ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.10179527802366092),
    ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.17450627260520618),
    ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.2119004845251565),
    ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.1937374981819948),
    ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.13838396642449252),
    ExpectedProbability(AttackStage.FAILED_SAVES, 7, 0.07907651250596617),
    ExpectedProbability(AttackStage.FAILED_SAVES, 8, 0.03671407542437232),
    ExpectedProbability(AttackStage.FAILED_SAVES, 9, 0.01398624707984404),
    ExpectedProbability(AttackStage.FAILED_SAVES, 10, 0.004395656432182202),
    ExpectedProbability(AttackStage.FAILED_SAVES, 11, 0.0011416942391383796),
    ExpectedProbability(AttackStage.FAILED_SAVES, 12, 0.0002446059632070943),
    ExpectedProbability(AttackStage.FAILED_SAVES, 13, 4.296628655833735e-05),
    ExpectedProbability(AttackStage.FAILED_SAVES, 14, 6.096590231551701e-06),
    ExpectedProbability(AttackStage.FAILED_SAVES, 15, 5.787948192825044e-07),
    # MORTAL_WOUNDS
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 0.9999984900428736),
    # DAMAGE
    ExpectedProbability(AttackStage.DAMAGE, 0, 0.006563081486256541),
    ExpectedProbability(AttackStage.DAMAGE, 2, 0.03750347547978666),
    ExpectedProbability(AttackStage.DAMAGE, 4, 0.10179527802366092),
    ExpectedProbability(AttackStage.DAMAGE, 6, 0.17450627260520618),
    ExpectedProbability(AttackStage.DAMAGE, 8, 0.2119004845251565),
    ExpectedProbability(AttackStage.DAMAGE, 10, 0.1937374981819948),
    ExpectedProbability(AttackStage.DAMAGE, 12, 0.13838396642449252),
    ExpectedProbability(AttackStage.DAMAGE, 14, 0.07907651250596617),
    ExpectedProbability(AttackStage.DAMAGE, 16, 0.03671407542437232),
    ExpectedProbability(AttackStage.DAMAGE, 18, 0.01398624707984404),
    ExpectedProbability(AttackStage.DAMAGE, 20, 0.004395656432182202),
    ExpectedProbability(AttackStage.DAMAGE, 22, 0.0011416942391383796),
    ExpectedProbability(AttackStage.DAMAGE, 24, 0.0002446059632070943),
    ExpectedProbability(AttackStage.DAMAGE, 26, 4.296628655833735e-05),
    ExpectedProbability(AttackStage.DAMAGE, 28, 6.096590231551701e-06),
    ExpectedProbability(AttackStage.DAMAGE, 30, 5.787948192825044e-07),
    # FELT_DMG
    ExpectedProbability(AttackStage.FELT_DMG, 0, 0.006563081486256541),
    ExpectedProbability(AttackStage.FELT_DMG, 2, 0.03750347547978666),
    ExpectedProbability(AttackStage.FELT_DMG, 4, 0.10179527802366092),
    ExpectedProbability(AttackStage.FELT_DMG, 6, 0.17450627260520618),
    ExpectedProbability(AttackStage.FELT_DMG, 8, 0.2119004845251565),
    ExpectedProbability(AttackStage.FELT_DMG, 10, 0.1937374981819948),
    ExpectedProbability(AttackStage.FELT_DMG, 12, 0.13838396642449252),
    ExpectedProbability(AttackStage.FELT_DMG, 14, 0.07907651250596617),
    ExpectedProbability(AttackStage.FELT_DMG, 16, 0.03671407542437232),
    ExpectedProbability(AttackStage.FELT_DMG, 18, 0.01398624707984404),
    ExpectedProbability(AttackStage.FELT_DMG, 20, 0.004395656432182202),
    ExpectedProbability(AttackStage.FELT_DMG, 22, 0.0011416942391383796),
    ExpectedProbability(AttackStage.FELT_DMG, 24, 0.0002446059632070943),
    ExpectedProbability(AttackStage.FELT_DMG, 26, 4.296628655833735e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 28, 6.096590231551701e-06),
    ExpectedProbability(AttackStage.FELT_DMG, 30, 5.787948192825044e-07),
    # MODELS_SLAIN
    ExpectedProbability(AttackStage.MODELS_SLAIN, 0, 0.5322685921200668),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 1, 0.46189829961666984),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 0.005831019511317565),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 5.787948192825044e-07),
]
    )
]

slow_test_cases = [
    TestCase(
        name="60 attacks damage 1d3 vs 2 wounds SUPER SLOW",
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
        expected=[
    # HITS
    ExpectedProbability(AttackStage.HITS, 0, 7.52316384526264e-37),
    ExpectedProbability(AttackStage.HITS, 1, 1.3541694921472752e-34),
    ExpectedProbability(AttackStage.HITS, 2, 1.1984400005503386e-32),
    ExpectedProbability(AttackStage.HITS, 3, 6.950952003191964e-31),
    ExpectedProbability(AttackStage.HITS, 4, 2.9715319813645645e-29),
    ExpectedProbability(AttackStage.HITS, 5, 9.984347457384937e-28),
    ExpectedProbability(AttackStage.HITS, 6, 2.7456955507808576e-26),
    ExpectedProbability(AttackStage.HITS, 7, 6.354323988949985e-25),
    ExpectedProbability(AttackStage.HITS, 8, 1.2629218928038094e-23),
    ExpectedProbability(AttackStage.HITS, 9, 2.1890646141932697e-22),
    ExpectedProbability(AttackStage.HITS, 10, 3.3492688597157027e-21),
    ExpectedProbability(AttackStage.HITS, 11, 4.567184808703231e-20),
    ExpectedProbability(AttackStage.HITS, 12, 5.594801390661457e-19),
    ExpectedProbability(AttackStage.HITS, 13, 6.197318463501923e-18),
    ExpectedProbability(AttackStage.HITS, 14, 6.241585023955509e-17),
    ExpectedProbability(AttackStage.HITS, 15, 5.742258222039067e-16),
    ExpectedProbability(AttackStage.HITS, 16, 4.845030374845463e-15),
    ExpectedProbability(AttackStage.HITS, 17, 3.762023585174124e-14),
    ExpectedProbability(AttackStage.HITS, 18, 2.696116902708122e-13),
    ExpectedProbability(AttackStage.HITS, 19, 1.7879512091643336e-12),
    ExpectedProbability(AttackStage.HITS, 20, 1.0995899936360652e-11),
    ExpectedProbability(AttackStage.HITS, 21, 6.283371392206086e-11),
    ExpectedProbability(AttackStage.HITS, 22, 3.3416111494914185e-10),
    ExpectedProbability(AttackStage.HITS, 23, 1.656276830617486e-09),
    ExpectedProbability(AttackStage.HITS, 24, 7.660280341605873e-09),
    ExpectedProbability(AttackStage.HITS, 25, 3.3092411075737356e-08),
    ExpectedProbability(AttackStage.HITS, 26, 1.33642429344324e-07),
    ExpectedProbability(AttackStage.HITS, 27, 5.04871399745224e-07),
    ExpectedProbability(AttackStage.HITS, 28, 1.7850810205277558e-06),
    ExpectedProbability(AttackStage.HITS, 29, 5.909233723126364e-06),
    ExpectedProbability(AttackStage.HITS, 30, 1.831862454169173e-05),
    ExpectedProbability(AttackStage.HITS, 31, 5.3183103508137285e-05),
    ExpectedProbability(AttackStage.HITS, 32, 0.00014459156266274822),
    ExpectedProbability(AttackStage.HITS, 33, 0.00036805125041426836),
    ExpectedProbability(AttackStage.HITS, 34, 0.0008768279789281096),
    ExpectedProbability(AttackStage.HITS, 35, 0.001954073781611215),
    ExpectedProbability(AttackStage.HITS, 36, 0.004070987045023366),
    ExpectedProbability(AttackStage.HITS, 37, 0.007921920736261686),
    ExpectedProbability(AttackStage.HITS, 38, 0.014384540284264643),
    ExpectedProbability(AttackStage.HITS, 39, 0.024343068173370928),
    ExpectedProbability(AttackStage.HITS, 40, 0.03834033237305921),
    ExpectedProbability(AttackStage.HITS, 41, 0.05610780347276957),
    ExpectedProbability(AttackStage.HITS, 42, 0.07614630471304441),
    ExpectedProbability(AttackStage.HITS, 43, 0.09562559196521857),
    ExpectedProbability(AttackStage.HITS, 44, 0.11083875432332152),
    ExpectedProbability(AttackStage.HITS, 45, 0.11822800461154297),
    ExpectedProbability(AttackStage.HITS, 46, 0.11565783059824855),
    ExpectedProbability(AttackStage.HITS, 47, 0.10335380606651999),
    ExpectedProbability(AttackStage.HITS, 48, 0.0839749674290475),
    ExpectedProbability(AttackStage.HITS, 49, 0.061695894437667535),
    ExpectedProbability(AttackStage.HITS, 50, 0.04071929032886058),
    ExpectedProbability(AttackStage.HITS, 51, 0.023952523722859165),
    ExpectedProbability(AttackStage.HITS, 52, 0.012436887317638413),
    ExpectedProbability(AttackStage.HITS, 53, 0.005631798030628718),
    ExpectedProbability(AttackStage.HITS, 54, 0.002190143678577833),
    ExpectedProbability(AttackStage.HITS, 55, 0.000716774294807291),
    ExpectedProbability(AttackStage.HITS, 56, 0.0001919931146805244),
    ExpectedProbability(AttackStage.HITS, 57, 4.04196030906367e-05),
    ExpectedProbability(AttackStage.HITS, 58, 6.272007376133284e-06),
    ExpectedProbability(AttackStage.HITS, 59, 6.378312585898255e-07),
    ExpectedProbability(AttackStage.HITS, 60, 3.189156292949126e-08),
    # WOUNDS
    ExpectedProbability(AttackStage.WOUNDS, 12, 1.533678425058973e-07),
    ExpectedProbability(AttackStage.WOUNDS, 13, 3.8544052048555785e-07),
    ExpectedProbability(AttackStage.WOUNDS, 14, 6.071620078452104e-07),
    ExpectedProbability(AttackStage.WOUNDS, 15, 7.681000458832175e-07),
    ExpectedProbability(AttackStage.WOUNDS, 16, 7.801709411457886e-07),
    ExpectedProbability(AttackStage.WOUNDS, 17, 6.525547259062358e-07),
    ExpectedProbability(AttackStage.WOUNDS, 18, 4.112213228569082e-07),
    ExpectedProbability(AttackStage.WOUNDS, 19, 6.278301602779863e-07),
    ExpectedProbability(AttackStage.WOUNDS, 20, 2.6901919757018328e-06),
    ExpectedProbability(AttackStage.WOUNDS, 21, 9.472330381207958e-06),
    ExpectedProbability(AttackStage.WOUNDS, 22, 2.9066710944373345e-05),
    ExpectedProbability(AttackStage.WOUNDS, 23, 8.126015155825908e-05),
    ExpectedProbability(AttackStage.WOUNDS, 24, 0.00020973267014306196),
    ExpectedProbability(AttackStage.WOUNDS, 25, 0.0005043799216294846),
    ExpectedProbability(AttackStage.WOUNDS, 26, 0.0011326003791103743),
    ExpectedProbability(AttackStage.WOUNDS, 27, 0.0023778479351192765),
    ExpectedProbability(AttackStage.WOUNDS, 28, 0.004671328818929535),
    ExpectedProbability(AttackStage.WOUNDS, 29, 0.008591510355974089),
    ExpectedProbability(AttackStage.WOUNDS, 30, 0.014797001815427762),
    ExpectedProbability(AttackStage.WOUNDS, 31, 0.02386651823284891),
    ExpectedProbability(AttackStage.WOUNDS, 32, 0.03604875316462472),
    ExpectedProbability(AttackStage.WOUNDS, 33, 0.05097827649039005),
    ExpectedProbability(AttackStage.WOUNDS, 34, 0.0674714687803639),
    ExpectedProbability(AttackStage.WOUNDS, 35, 0.0835362685049275),
    ExpectedProbability(AttackStage.WOUNDS, 36, 0.09668556921682755),
    ExpectedProbability(AttackStage.WOUNDS, 37, 0.10452502869501946),
    ExpectedProbability(AttackStage.WOUNDS, 38, 0.1054418190131337),
    ExpectedProbability(AttackStage.WOUNDS, 39, 0.09913325013867122),
    ExpectedProbability(AttackStage.WOUNDS, 40, 0.0867414222152652),
    ExpectedProbability(AttackStage.WOUNDS, 41, 0.07052116789179111),
    ExpectedProbability(AttackStage.WOUNDS, 42, 0.05317020052754018),
    ExpectedProbability(AttackStage.WOUNDS, 43, 0.0370947710371094),
    ExpectedProbability(AttackStage.WOUNDS, 44, 0.023885972185632487),
    ExpectedProbability(AttackStage.WOUNDS, 45, 0.014153638531223468),
    ExpectedProbability(AttackStage.WOUNDS, 46, 0.0076911639760678914),
    ExpectedProbability(AttackStage.WOUNDS, 47, 0.003817310456775552),
    ExpectedProbability(AttackStage.WOUNDS, 48, 0.0017222539491970538),
    ExpectedProbability(AttackStage.WOUNDS, 49, 0.0007020868825695191),
    ExpectedProbability(AttackStage.WOUNDS, 50, 0.0002567082010280349),
    ExpectedProbability(AttackStage.WOUNDS, 51, 8.309425298157014e-05),
    ExpectedProbability(AttackStage.WOUNDS, 52, 2.315399215666504e-05),
    ExpectedProbability(AttackStage.WOUNDS, 53, 4.905785181083438e-06),
    ExpectedProbability(AttackStage.WOUNDS, 54, 9.813679677463916e-08),
    # FAILED_SAVES
    ExpectedProbability(AttackStage.FAILED_SAVES, 0, 1.0845365466322954e-05),
    ExpectedProbability(AttackStage.FAILED_SAVES, 1, 0.00019941265764103472),
    ExpectedProbability(AttackStage.FAILED_SAVES, 2, 0.0012318017620918994),
    ExpectedProbability(AttackStage.FAILED_SAVES, 3, 0.00482272755132059),
    ExpectedProbability(AttackStage.FAILED_SAVES, 4, 0.013801384822978926),
    ExpectedProbability(AttackStage.FAILED_SAVES, 5, 0.03096343935282675),
    ExpectedProbability(AttackStage.FAILED_SAVES, 6, 0.05680523034157151),
    ExpectedProbability(AttackStage.FAILED_SAVES, 7, 0.0876702329196527),
    ExpectedProbability(AttackStage.FAILED_SAVES, 8, 0.11618202827863396),
    ExpectedProbability(AttackStage.FAILED_SAVES, 9, 0.1342664082943854),
    ExpectedProbability(AttackStage.FAILED_SAVES, 10, 0.13695397044540777),
    ExpectedProbability(AttackStage.FAILED_SAVES, 11, 0.12449676634456926),
    ExpectedProbability(AttackStage.FAILED_SAVES, 12, 0.10165985101345536),
    ExpectedProbability(AttackStage.FAILED_SAVES, 13, 0.07505624276921767),
    ExpectedProbability(AttackStage.FAILED_SAVES, 14, 0.0503761194569843),
    ExpectedProbability(AttackStage.FAILED_SAVES, 15, 0.030880567032561458),
    ExpectedProbability(AttackStage.FAILED_SAVES, 16, 0.017351670335906448),
    ExpectedProbability(AttackStage.FAILED_SAVES, 17, 0.00896268198921598),
    ExpectedProbability(AttackStage.FAILED_SAVES, 18, 0.004260479686401637),
    ExpectedProbability(AttackStage.FAILED_SAVES, 19, 0.0018599602570117664),
    ExpectedProbability(AttackStage.FAILED_SAVES, 20, 0.0007393186554005092),
    ExpectedProbability(AttackStage.FAILED_SAVES, 21, 0.0002603062660694905),
    ExpectedProbability(AttackStage.FAILED_SAVES, 22, 7.462412940719044e-05),
    ExpectedProbability(AttackStage.FAILED_SAVES, 23, 1.2027523931744535e-05),
    # MORTAL_WOUNDS
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 0, 0.00028451098978030803),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 1, 0.0027751607126170947),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 2, 0.011896708688554124),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 3, 0.032992370210609),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 4, 0.06724445521535671),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 5, 0.10764198017904701),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 6, 0.14098857735434342),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 7, 0.1553876579720751),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 8, 0.14706455492886433),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 9, 0.12138094000031996),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 10, 0.08842349204904175),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 11, 0.057403537275321465),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 12, 0.033469338444390706),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 13, 0.017635419658370354),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 14, 0.00843967652713267),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 15, 0.0036790987618916695),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 16, 0.001460516144506251),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 17, 0.0005247147976025358),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 18, 0.0001634800582817513),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 19, 3.8795248636546096e-05),
    ExpectedProbability(AttackStage.MORTAL_WOUNDS, 20, 3.1120353670640053e-06),
    # DAMAGE
    ExpectedProbability(AttackStage.DAMAGE, 9, 3.9679151932638244e-07),
    ExpectedProbability(AttackStage.DAMAGE, 12, 8.899715139968301e-06),
    ExpectedProbability(AttackStage.DAMAGE, 15, 5.708671163422127e-05),
    ExpectedProbability(AttackStage.DAMAGE, 18, 0.000238888436794727),
    ExpectedProbability(AttackStage.DAMAGE, 21, 0.0007842993920804634),
    ExpectedProbability(AttackStage.DAMAGE, 24, 0.002164572920057308),
    ExpectedProbability(AttackStage.DAMAGE, 27, 0.005173280267486394),
    ExpectedProbability(AttackStage.DAMAGE, 30, 0.010886982622853576),
    ExpectedProbability(AttackStage.DAMAGE, 33, 0.020398678671775083),
    ExpectedProbability(AttackStage.DAMAGE, 36, 0.03431562851891824),
    ExpectedProbability(AttackStage.DAMAGE, 39, 0.05218875862545987),
    ExpectedProbability(AttackStage.DAMAGE, 42, 0.07215659009047203),
    ExpectedProbability(AttackStage.DAMAGE, 45, 0.09112552302241786),
    ExpectedProbability(AttackStage.DAMAGE, 48, 0.10553714216380741),
    ExpectedProbability(AttackStage.DAMAGE, 51, 0.1124766988637104),
    ExpectedProbability(AttackStage.DAMAGE, 54, 0.1106386006078176),
    ExpectedProbability(AttackStage.DAMAGE, 57, 0.10069980979989337),
    ExpectedProbability(AttackStage.DAMAGE, 60, 0.08499418707091459),
    ExpectedProbability(AttackStage.DAMAGE, 63, 0.0666501363206756),
    ExpectedProbability(AttackStage.DAMAGE, 66, 0.04863690132573046),
    ExpectedProbability(AttackStage.DAMAGE, 69, 0.033072422552064315),
    ExpectedProbability(AttackStage.DAMAGE, 72, 0.020976225819733792),
    ExpectedProbability(AttackStage.DAMAGE, 75, 0.012419501844840487),
    ExpectedProbability(AttackStage.DAMAGE, 78, 0.006867564012073503),
    ExpectedProbability(AttackStage.DAMAGE, 81, 0.003543745739747168),
    ExpectedProbability(AttackStage.DAMAGE, 84, 0.001701536030326471),
    ExpectedProbability(AttackStage.DAMAGE, 87, 0.0007544346119960333),
    ExpectedProbability(AttackStage.DAMAGE, 90, 0.0003029427955205012),
    ExpectedProbability(AttackStage.DAMAGE, 93, 0.00010339029888653641),
    ExpectedProbability(AttackStage.DAMAGE, 96, 2.280884526630234e-05),
    ExpectedProbability(AttackStage.DAMAGE, 99, 4.627624959850008e-07),
    # FELT_DMG
    ExpectedProbability(AttackStage.FELT_DMG, 4, 4.268802344858463e-07),
    ExpectedProbability(AttackStage.FELT_DMG, 5, 2.4037360654591827e-06),
    ExpectedProbability(AttackStage.FELT_DMG, 6, 2.5271759257869557e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 7, 2.8151610893749e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 8, 0.00019576422821257318),
    ExpectedProbability(AttackStage.FELT_DMG, 9, 0.0001456759460690816),
    ExpectedProbability(AttackStage.FELT_DMG, 10, 0.0008853344632769773),
    ExpectedProbability(AttackStage.FELT_DMG, 11, 0.0005275057959832609),
    ExpectedProbability(AttackStage.FELT_DMG, 12, 0.002961853919455066),
    ExpectedProbability(AttackStage.FELT_DMG, 13, 0.0014908223672533106),
    ExpectedProbability(AttackStage.FELT_DMG, 14, 0.007891923930817332),
    ExpectedProbability(AttackStage.FELT_DMG, 15, 0.0034436371893417717),
    ExpectedProbability(AttackStage.FELT_DMG, 16, 0.01739405182995479),
    ExpectedProbability(AttackStage.FELT_DMG, 17, 0.006689231670199864),
    ExpectedProbability(AttackStage.FELT_DMG, 18, 0.03250166010877058),
    ExpectedProbability(AttackStage.FELT_DMG, 19, 0.01114825174234031),
    ExpectedProbability(AttackStage.FELT_DMG, 20, 0.05241838183601193),
    ExpectedProbability(AttackStage.FELT_DMG, 21, 0.016184485221602346),
    ExpectedProbability(AttackStage.FELT_DMG, 22, 0.073979967030096),
    ExpectedProbability(AttackStage.FELT_DMG, 23, 0.020712434033440374),
    ExpectedProbability(AttackStage.FELT_DMG, 24, 0.09237524050527259),
    ExpectedProbability(AttackStage.FELT_DMG, 25, 0.023592088552562545),
    ExpectedProbability(AttackStage.FELT_DMG, 26, 0.10296164765941611),
    ExpectedProbability(AttackStage.FELT_DMG, 27, 0.024106119172666286),
    ExpectedProbability(AttackStage.FELT_DMG, 28, 0.10319542335778228),
    ExpectedProbability(AttackStage.FELT_DMG, 29, 0.022240819058241233),
    ExpectedProbability(AttackStage.FELT_DMG, 30, 0.10153971503126324),
    ExpectedProbability(AttackStage.FELT_DMG, 31, 0.03661546110485278),
    ExpectedProbability(AttackStage.FELT_DMG, 32, 0.04888047184340871),
    ExpectedProbability(AttackStage.FELT_DMG, 33, 0.06609873328893807),
    ExpectedProbability(AttackStage.FELT_DMG, 34, 0.020702264557168507),
    ExpectedProbability(AttackStage.FELT_DMG, 35, 0.02675586006192705),
    ExpectedProbability(AttackStage.FELT_DMG, 36, 0.03295829384500293),
    ExpectedProbability(AttackStage.FELT_DMG, 37, 0.009073127043393974),
    ExpectedProbability(AttackStage.FELT_DMG, 38, 0.011413622044875798),
    ExpectedProbability(AttackStage.FELT_DMG, 39, 0.012926062595894277),
    ExpectedProbability(AttackStage.FELT_DMG, 40, 0.003154967134756405),
    ExpectedProbability(AttackStage.FELT_DMG, 41, 0.0038779439539426384),
    ExpectedProbability(AttackStage.FELT_DMG, 42, 0.0040645352147241),
    ExpectedProbability(AttackStage.FELT_DMG, 43, 0.0008837547596118491),
    ExpectedProbability(AttackStage.FELT_DMG, 44, 0.0010639207042890877),
    ExpectedProbability(AttackStage.FELT_DMG, 45, 0.0010343040008947825),
    ExpectedProbability(AttackStage.FELT_DMG, 46, 0.00019923244070022336),
    ExpectedProbability(AttackStage.FELT_DMG, 47, 0.0002348367110898132),
    ExpectedProbability(AttackStage.FELT_DMG, 48, 0.00020959054558589325),
    ExpectedProbability(AttackStage.FELT_DMG, 49, 3.437942359361212e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 50, 3.9337328360114035e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 51, 3.0121521550924215e-05),
    ExpectedProbability(AttackStage.FELT_DMG, 52, 3.3973548371627254e-06),
    ExpectedProbability(AttackStage.FELT_DMG, 53, 3.656301118224381e-06),
    ExpectedProbability(AttackStage.FELT_DMG, 54, 1.8431926803361142e-06),
    ExpectedProbability(AttackStage.FELT_DMG, 55, 3.927532761185222e-08),
    ExpectedProbability(AttackStage.FELT_DMG, 56, 3.927532761185222e-08),
    ExpectedProbability(AttackStage.FELT_DMG, 57, 1.3091775870617405e-08),
    # MODELS_SLAIN
    ExpectedProbability(AttackStage.MODELS_SLAIN, 2, 2.8306162999450283e-06),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 3, 5.342337015161854e-05),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 4, 0.00034144017428165495),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 5, 0.0014128402592602388),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 6, 0.004452676286708374),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 7, 0.011335561120159111),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 8, 0.024083283500154658),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 9, 0.04364991185111089),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 10, 0.06860286705761431),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 11, 0.09469240106353638),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 12, 0.11596732905783504),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 13, 0.12706776683208224),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 14, 0.12543624241602347),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 15, 0.1870356479795251),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 16, 0.11355685790803359),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 17, 0.053445042933272686),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 18, 0.01995897368459331),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 19, 0.006012210678625042),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 20, 0.0014683731526848201),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 21, 0.00028330729753961947),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 22, 3.717517750631132e-05),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 23, 1.9217433355598184e-06),
    ExpectedProbability(AttackStage.MODELS_SLAIN, 24, 1.3091775870617405e-08),
]
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
        if abs(actual - exp.probability) > ExpectedProbability.tolerance:
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

def start_profiling() -> Any:
    import cProfile
    profiler = cProfile.Profile()
    profiler.enable()
    return profiler

def stop_profiling(profiler: Any) -> None:
    import pstats

    profiler.disable()
    stats = pstats.Stats(profiler)
    stats.sort_stats(pstats.SortKey.CUMULATIVE)
    stats.print_stats(50) 
    stats.sort_stats(pstats.SortKey.TIME)
    stats.print_stats(20)

def run_in_monte_carlo_mode() -> None:
    AttackConfig.monte_carlo = True
    AttackConfig.multiprocessing = False
    ExpectedProbability.tolerance = 0.012



def run_test_suite() -> None:
    profiler_context = start_profiling()

    all_passed = True
    all_tests = []
    # all_tests.extend(test_cases)
    all_tests.extend(slow_test_cases)

    failed_tests = []
    passed_tests = []

    # run_in_monte_carlo_mode()

    
    for test in all_tests:
        if not run_test_case(test):
            all_passed = False
            failed_tests.append(test)
        else:
            passed_tests.append(test)
    
    stop_profiling(profiler_context)


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