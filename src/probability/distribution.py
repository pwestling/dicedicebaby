from dataclasses import dataclass
from typing import Dict, TypeVar, Callable, List, Generic
from fractions import Fraction

T = TypeVar('T')
U = TypeVar('U')
K = TypeVar('K')
V = TypeVar('V')
J = TypeVar('J')

@dataclass
class Distribution(Generic[T]):
    probabilities: Dict[T, Fraction]
    
    def __str__(self) -> str:
        items = [f"{str(k)}:{float(v)}" for k, v in self.probabilities.items()]
        return f"Dist[{', '.join(items)}]"
    
    @classmethod
    def singleton(cls, x: T) -> 'Distribution[T]':
        return cls({x: Fraction(1)})

    @classmethod
    def empty(cls) -> 'Distribution[T]':
        return cls({})
    
    @classmethod
    def uniform(cls, xs: List[T]) -> 'Distribution[T]':
        n = len(xs)
        return cls({x: Fraction(1, n) for x in xs})
    
    def map(self, f: Callable[[T], U]) -> 'Distribution[U]':
        result: Dict[U, Fraction] = {}
        for x, p in self.probabilities.items():
            y = f(x)
            result[y] = result.get(y, Fraction(0)) + p
        return Distribution(result)
    
    def filter(self, pred: Callable[[T], bool], 
               if_true: Callable[[T], U], 
               if_false: Callable[[T], U]) -> 'Distribution[U]':
        """Filter a distribution, mapping values to different results based on the predicate.
        
        Args:
            pred: Predicate to test values against
            if_true: Function to apply to values that satisfy the predicate
            if_false: Function to apply to values that don't satisfy the predicate
        """
        result: Dict[U, Fraction] = {}
        for x, p in self.probabilities.items():
            y = if_true(x) if pred(x) else if_false(x)
            result[y] = result.get(y, Fraction(0)) + p
        return Distribution(result)
    
    def repeated(self, n: int) -> 'Distribution[T]':
        if n == 0:
            raise ValueError("n must be greater than 0")
        accum = self
        for _ in range(n - 1):
            accum = accum.combine(self, lambda x, y: x + y)
        return accum


    def combine(self, other: 'Distribution[J]', f: Callable[[T, J], U]) -> 'Distribution[U]':
        result: Dict[U, Fraction] = {}
        for (x, p1) in self.probabilities.items():
            for (y, p2) in other.probabilities.items():
                z = f(x, y)
                result[z] = result.get(z, Fraction(0)) + p1 * p2
        return Distribution(result)

    def merge(self, other: 'Distribution[T]') -> 'Distribution[T]':
        result: Dict[T, Fraction] = {}
        for (x, p1) in self.probabilities.items():
            if x in other.probabilities:
                result[x] = p1 + other.probabilities[x]
            else:
                result[x] = p1
        for (x, p2) in other.probabilities.items():
            if x not in self.probabilities:
                result[x] = p2
        return Distribution(result)

    def normalize(self) -> 'Distribution[T]':
        total = sum(self.probabilities.values())
        return Distribution({x: p / total for x, p in self.probabilities.items()})


    def scale(self, scale: Fraction) -> 'Distribution[T]':
        return Distribution({x: p * scale for x, p in self.probabilities.items()})

    def bind(self, f: Callable[[T], 'Distribution[U]']) -> 'Distribution[U]':
        """Monadic bind (>>=) for distributions.
        Maps each value to a new distribution and combines the results.
        
        Args:
            f: Function that maps a value to a new distribution
        """
        result: Dict[U, Fraction] = {}
        for x, p1 in self.probabilities.items():
            # Get new distribution from f
            dist = f(x)
            # Scale its probabilities by p1 and merge into result
            for y, p2 in dist.probabilities.items():
                result[y] = result.get(y, Fraction(0)) + p1 * p2
        # print("bind result", Distribution(result))
        return Distribution(result)

    def bind_on_match(self, key: T, f: Callable[[T], 'Distribution[T]']) -> 'Distribution[T]':
        
        def bind_fn(some_key: T) -> Distribution[T]:
            
            if some_key == key:
                return f(some_key)
            return Distribution.singleton(some_key)
        
        return self.bind(bind_fn)

# Common distributions
d6 = Distribution.uniform(list(range(1, 7)))
d3 = Distribution.uniform(list(range(1, 4))) 

if __name__ == "__main__":
    print(d6.repeated(2))
