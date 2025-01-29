from dataclasses import dataclass
from typing import Union, List, Dict
from .distribution import Distribution, d6, d3

@dataclass(frozen=True)
class DiceFormula:
    dice: int  # Number of dice
    sides: int  # Number of sides per die
    modifier: int  # Fixed modifier to add

    @staticmethod
    def constant(value: int) -> 'DiceFormula':
        """Create a formula for a constant value"""
        return DiceFormula(0, 1, value)

    def __str__(self) -> str:
        base = f"{self.dice if self.dice > 1 else ''}D{self.sides}"
        if self.modifier > 0:
            return f"{base}+{self.modifier}"
        elif self.modifier < 0:
            return f"{base}{self.modifier}"
        return base
    
    def roll(self) -> Distribution[int]:
        """Roll the dice and return a distribution of results"""
        if self.dice == 0:
            return Distribution.singleton(self.modifier)
        
        # Roll one die
        single_die = Distribution.uniform(list(range(1, self.sides + 1)))
        
        # Combine multiple dice
        result = single_die
        for _ in range(self.dice - 1):
            result = result.bind(lambda x: single_die.map(lambda y: x + y))
            
        # Add modifier
        if self.modifier != 0:
            result = result.map(lambda x: x + self.modifier)
            
        return result

    def is_variable(self) -> bool:
        """Return True if this formula involves dice rolls"""
        return self.dice > 0

    def to_dict(self) -> Dict[str, int]:
        """Convert to JSON-serializable dictionary"""
        return {
            "dice": self.dice,
            "sides": self.sides,
            "modifier": self.modifier
        }

    def max_possible(self) -> int:
        """Return the maximum possible value for this formula"""
        return self.dice * self.sides + self.modifier

    def min_possible(self) -> int:
        """Return the minimum possible value for this formula"""
        return self.dice + self.modifier

    @staticmethod
    def from_dict(data: Dict[str, int]) -> 'DiceFormula':
        """Create from JSON-serializable dictionary"""
        return DiceFormula(
            dice=data["dice"],
            sides=data["sides"],
            modifier=data["modifier"]
        )

# Common formulas
D6 = DiceFormula(1, 6, 0)
D3 = DiceFormula(1, 3, 0)
TWO_D6 = DiceFormula(2, 6, 0) 
