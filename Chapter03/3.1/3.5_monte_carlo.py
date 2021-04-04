from typing import Callable
from random import random


def monte_carlo(trials: int, experiment: Callable[[], bool]) -> float:
    passed = 0
    for _ in range(trials):
        if experiment():
            passed += 1
    return passed / trials


def pi_test() -> bool:
    return ((random() ** 2) + (random() ** 2) < 1.0)


print(4 * monte_carlo(100000, pi_test))
