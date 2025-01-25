from dataclasses import dataclass
from typing import Union, List
from .distribution import Distribution, d6, d3

@dataclass(frozen=True)
class DiceFormula:
    num_dice: int
    sides: int
    modifier: int = 0

    @classmethod
    def constant(cls, value: int) -> "DiceFormula":
        return cls(0, 0, value)
    
    def __str__(self) -> str:
        base = f"{self.num_dice if self.num_dice > 1 else ''}D{self.sides}"
        if self.modifier > 0:
            return f"{base}+{self.modifier}"
        elif self.modifier < 0:
            return f"{base}{self.modifier}"
        return base
    
    def roll(self) -> Distribution[int]:
        # Start with single die distribution
        if self.sides == 6:
            base_dist = d6
        elif self.sides == 3:
            base_dist = d3
        elif self.sides == 1:
            base_dist = Distribution.singleton(self.modifier)
        else:
            base_dist = Distribution.uniform(list(range(1, self.sides + 1)))
            
        # Combine dice
        result = Distribution.singleton(0)
        for _ in range(self.num_dice):
            result = result.combine(base_dist, lambda x, y: x + y)
            
        # Add modifier
        if self.modifier != 0:
            result = result.map(lambda x: x + self.modifier)
            
        return result

# Common formulas
D6 = DiceFormula(1, 6)
D3 = DiceFormula(1, 3)
TWO_D6 = DiceFormula(2, 6) 