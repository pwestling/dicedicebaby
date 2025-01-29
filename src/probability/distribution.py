from dataclasses import dataclass
from typing import Dict, TypeVar, Callable, List, Generic, Tuple, Any, Protocol, Literal, ClassVar
from fractions import Fraction
from multiprocessing import Pool
from itertools import product
from functools import partial, wraps
import random
import os
import multiprocessing.pool
import dill
import sys


T = TypeVar('T')
U = TypeVar('U')
K = TypeVar('K')
V = TypeVar('V')
J = TypeVar('J')

R = TypeVar('R')  # Return type
F = TypeVar('F', bound=Callable[..., Any])  # Function type

repeat_cache: Dict[Tuple['Distribution', int], 'Distribution'] = {}

PARALLEL_THRESHOLD = 1000000000000  # Threshold for parallel processing
CHUNK_SIZE = 1000  # Size of chunks for parallel processing
NUM_PROCESSES = os.cpu_count() or 1  # Number of CPU cores to use

class ExceptionWrapper(object):

    def __init__(self, ee):
        self.ee = ee
        __, __, self.tb = sys.exc_info()

    def re_raise(self):
        raise self.ee.with_traceback(self.tb)

class Sortable(Protocol):
    def __lt__(self: Any, other: Any) -> bool: ...

S = TypeVar('S', bound=Sortable)

class Box(Generic[T]):
    def __init__(self, value: T):
        self.value = value

MULTIPROCESSING_POOL: multiprocessing.pool.Pool | None = None

def close_multiprocessing_pool():
    global MULTIPROCESSING_POOL
    if MULTIPROCESSING_POOL is not None:
        MULTIPROCESSING_POOL.close()
        MULTIPROCESSING_POOL = None

@dataclass
class Distribution(Generic[T]):

    EPSILON : ClassVar[Fraction] = Fraction(1, 100000000000000)
    PRUNE_FACTOR : ClassVar[Fraction | None] = Fraction(1, 1000000)
    MAX_ENTRIES_ACHIEVED : ClassVar[int] = 0
    MAX_ENTRIES_ALLOWED : ClassVar[int] = 1000
    PROBABILITY_TO_DISCARD : ClassVar[Fraction] = Fraction(1, 10000)


    probabilities: Dict[T, Fraction]

    def __str__(self) -> str:
        joiner = ",\n"

        try:
            # Try to sort keys, fall back to unsorted if not comparable
            items = sorted(
                [(k, float(v)) for k, v in self.probabilities.items()],
                key=lambda x: x[0]  # type: ignore
            )
            return f"Dist[\n{joiner.join(f'{str(k)}:{v}' for k, v in items)}]"
        except TypeError:
            # If keys can't be sorted, use original unsorted format
            items = [f"{str(k)}:{float(v)}" for k, v in self.probabilities.items()]
            return f"Dist[{joiner.join(items)}]"
    
    def limit_entries(self) -> 'Dict[T, Fraction]':
        # keep only the top n most probable entries
        sorted_items = sorted(self.probabilities.items(), key=lambda x: x[1], reverse=True)
        limited_items = {}
        total_prob = Fraction(0)
        for x, p in sorted_items:
            limited_items[x] = p
            total_prob += p
            if 1 - total_prob < Distribution.PROBABILITY_TO_DISCARD:
                break
        print(f"Discarding {len(sorted_items) - len(limited_items)} items out of {len(sorted_items)} with prob mass {float(1 - total_prob)}")
        return {x: p / total_prob for x, p in limited_items.items()}

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
    
    def get_singleton(self) -> T:
        if len(self.probabilities) != 1:
            raise ValueError("Distribution is not a singleton")
        return list(self.probabilities.keys())[0]

    def map(self, f: Callable[[T], U]) -> 'Distribution[U]':
        result: Dict[U, Fraction] = {}
        for x, p in self.probabilities.items():
            y = f(x)
            result[y] = result.get(y, Fraction(0)) + p
        return Distribution(result)

    def map_probabilities(self, f: Callable[[Fraction], Fraction]) -> 'Distribution[T]':
        return Distribution({x: f(p) for x, p in self.probabilities.items()})
    
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
        if n == 1:
            return self
        if (self, n) in repeat_cache:
            return repeat_cache[(self, n)] # type: ignore
        recur: Distribution[T] = self.repeated(n-1)
        result = self.combine(recur, lambda x, y: x + y) # type: ignore
        repeat_cache[(self, n)] = result
        return result

    def repeated2(self, n: int) -> 'Distribution[T]':
        if n == 0:
            raise ValueError("n must be greater than 0")
        accum = self
        for _ in range(n - 1):
            accum = accum.combine(self, lambda x, y: x + y)
        return accum

    def choose(self, always_choose: bool = False) -> T:
        return random.choices(list(self.probabilities.keys()), weights=list(self.probabilities.values()))[0]

    def collapse_to_choice(self) -> 'Distribution[T]':
        return Distribution.singleton(self.choose())


    def combine(self, other: 'Distribution[J]', f: Callable[[T, J], U]) -> 'Distribution[U]':
        # If distributions are large enough, use parallel processing
        if len(self.probabilities) * len(other.probabilities) > PARALLEL_THRESHOLD:
            return self._parallel_combine(other, f)
        return self._sequential_combine(other, f).prune()

    def _sequential_combine(self, other: 'Distribution[J]', f: Callable[[T, J], U]) -> 'Distribution[U]':
        result: Dict[U, Fraction] = {}
        for (x, p1) in self.probabilities.items():
            for (y, p2) in other.probabilities.items():
                z = f(x, y)
                result[z] = result.get(z, Fraction(0)) + p1 * p2
        return Distribution(result)

    def _get_multiprocessing_pool(self) -> multiprocessing.pool.Pool:
        global MULTIPROCESSING_POOL
        if MULTIPROCESSING_POOL is None:
            MULTIPROCESSING_POOL = DillPool(NUM_PROCESSES)
        return MULTIPROCESSING_POOL

    def _parallel_combine(self, other: 'Distribution[J]', f: Callable[[T, J], U]) -> 'Distribution[U]':
        items1 = list(self.probabilities.items())
        chunks = [items1[i:i + CHUNK_SIZE] for i in range(0, len(items1), CHUNK_SIZE)]
        pool = self._get_multiprocessing_pool()

        process_chunk = partial(_process_chunk_combine, 
                                other_probs=other.probabilities,
                                f=f)
        results = pool.map(process_chunk, chunks)
        
        # Merge all partial results
        final_result: Dict[U, Fraction] = {}
        for partial_result in results:
            for k, v in partial_result.items():
                final_result[k] = final_result.get(k, Fraction(0)) + v
        
        return Distribution(final_result)

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

    def prune(self) -> 'Distribution[T]':

        if Distribution.PRUNE_FACTOR is None:
            return self
        
        max_prob = max(self.probabilities.values())
        # eliminate items with probability less than max_prob * PRUNE_FACTOR
        result = Distribution({x: p for x, p in self.probabilities.items() if p > max_prob * Distribution.PRUNE_FACTOR})
        
        return result
        
    def bind(self, f: Callable[[T], 'Distribution[U]']) -> 'Distribution[U]':
        # If distribution is large enough, use parallel processing
        if len(self.probabilities) > PARALLEL_THRESHOLD:
            return self._parallel_bind(f)
        return self._sequential_bind(f)

    def _sequential_bind(self, f: Callable[[T], 'Distribution[U]']) -> 'Distribution[U]':
        """Monadic bind (>>=) for distributions.
        Maps each value to a new distribution and combines the results."""
        result: Dict[U, Fraction] = {}
        for x, p1 in self.probabilities.items():
            # Get new distribution from f
            dist = f(x)
            # Scale its probabilities by p1 and merge into result
            for y, p2 in dist.probabilities.items():
                result[y] = result.get(y, Fraction(0)) + p1 * p2
        return Distribution(result).prune()

    def _parallel_bind(self, f: Callable[[T], 'Distribution[U]']) -> 'Distribution[U]':
        items = list(self.probabilities.items())
        chunks = [items[i:i + CHUNK_SIZE] for i in range(0, len(items), CHUNK_SIZE)]
        pool = self._get_multiprocessing_pool()
        process_chunk = partial(_process_chunk_bind, f=f)
        results = pool.map(process_chunk, chunks)
        
        # Merge all partial results
        final_result: Dict[U, Fraction] = {}
        for partial_result in results:
            if isinstance(partial_result, ExceptionWrapper):
                partial_result.re_raise()
            else:
                for k, v in partial_result.items():
                    final_result[k] = final_result.get(k, Fraction(0)) + v
        
        return Distribution(final_result)

    def bind_on_match(self, key_pred: Callable[[T], bool], f: Callable[[T], 'Distribution[T]']) -> 'Distribution[T]':
        
        def bind_fn(some_key: T) -> Distribution[T]:
            
            if key_pred(some_key):
                return f(some_key)
            return Distribution.singleton(some_key)
        
        return self.bind(bind_fn)

    def __hash__(self) -> int:
        return hash(frozenset(self.probabilities.items()))
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Distribution):
            return False
        return self.probabilities == other.probabilities

# Common distributions
d6 = Distribution.uniform(list(range(1, 7)))
d3 = Distribution.uniform(list(range(1, 4))) 

def memoize(f: F) -> F:
    """Decorator that memoizes a function's return value based on its arguments.
    
    Works with both regular functions and methods. For methods, the instance id is 
    included in the cache key to avoid sharing cache between instances.
    """
    cache: Dict[Tuple[int, Tuple[Any, ...], Tuple[Tuple[str, Any], ...]], Any] = {}
    
    @wraps(f)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        # For methods, use id of first arg (self) as part of key
        instance_id = id(args[0]) if args and hasattr(args[0], '__dict__') else 0
        
        # Create cache key from instance id, positional args, and sorted keyword args
        key = (instance_id, args[1:] if instance_id else args, tuple(sorted(kwargs.items())))
        
        if key not in cache:
            cache[key] = f(*args, **kwargs)

        return cache[key]
        
    return wrapper  # type: ignore


def lift(f: Callable[[T], U]) -> Callable[['Distribution[T]'], 'Distribution[U]']:
    def lifted(dist: Distribution[T]) -> Distribution[U]:
        return dist.map(f)
    return lifted

def liftM(f: Callable[[T], 'Distribution[U]']) -> Callable[['Distribution[T]'], 'Distribution[U]']:
    """Lift a function T -> Distribution[U] to Distribution[T] -> Distribution[U].
    
    This is equivalent to Kleisli composition with return/singleton.
    In other words, lift(f) = bind(singleton . f)
    """
    def lifted(dist: Distribution[T]) -> Distribution[U]:
        return dist.bind(f)
    return lifted


def _process_chunk_combine(chunk: List[Tuple[T, Fraction]], 
                          other_probs: Dict[J, Fraction],
                          f: Callable[[T, J], U]) -> Dict[U, Fraction]:
    partial_result: Dict[U, Fraction] = {}
    for (x, p1) in chunk:
        for (y, p2) in other_probs.items():
            z = f(x, y)
            partial_result[z] = partial_result.get(z, Fraction(0)) + p1 * p2
    return partial_result

def _process_chunk_bind(chunk: List[Tuple[T, Fraction]], 
                       f: Callable[[T], 'Distribution[U]'],) -> Dict[U, Fraction] | ExceptionWrapper:
    try:
        partial_result: Dict[U, Fraction] = {}
        for x, p1 in chunk:
            dist = f(x)
            for y, p2 in dist.probabilities.items():
                partial_result[y] = partial_result.get(y, Fraction(0)) + p1 * p2
        return partial_result
    except Exception as e:
        import traceback
        print(f"Error in chunk processing: {str(e)}\n{traceback.format_exc()}", file=sys.stderr)
        return ExceptionWrapper(e)


# Create a Pool subclass that uses dill for serialization
class DillPool(multiprocessing.pool.Pool):
    def __init__(self, *args, **kwargs):
        kwargs['context'] = multiprocessing.get_context()
        super().__init__(*args, **kwargs)
        self._ctx = kwargs['context']

    def _setup_queues(self):
        self._inqueue = self._ctx.Queue()
        self._outqueue = self._ctx.Queue()
        self._quick_put = self._inqueue._writer.send # type: ignore
        self._quick_get = self._outqueue._reader.recv # type: ignore

    def _Popen(self, process_obj):
        return self._ctx.Process(
            target=process_obj.run,
            args=(self._inqueue, self._outqueue, dill.dumps, dill.loads)
        )


if __name__ == "__main__":
    print(d6.repeated(2))
