from itertools import groupby, chain
from typing import Iterator, TypeVar, Callable

T = TypeVar("T")


def looksay(s: Iterator[int]) -> Iterator[int]:
    return flatten(map(look_group, group(s)))


def group(s: Iterator[T]) -> Iterator[list[T]]:
    return (list(v) for k, v in groupby(s))


def look_group(xs: list[int]) -> list[int]:
    return [len(xs), xs[0]]

# from https://docs.python.org/3/library/itertools.html#itertools-recipes
def flatten(list_of_lists):
    "Flatten one level of nesting"
    return chain.from_iterable(list_of_lists)


def call_n(f: Callable[[T], T], x: T, n: int) -> T:
    for _ in range(n):
        x = f(x)
    return x


def ilen(s: Iterator[T]) -> int:
    return sum(1 for _ in s)


def part1(xs):
    ys = map(int, xs)
    return ilen(call_n(looksay, ys, 40))


def part2(xs):
    ys = map(int, xs)
    return ilen(call_n(looksay, ys, 50))


def main():
    import time

    with open(directory + "10.txt", "r") as f:
        d = f.read()

    a = time.time()
    print("part1", part1(d))
    b = time.time()
    print("part2", part2(d))
    c = time.time()
    print(b - a, c - b, c - a)


directory = "E:/important/programming/advent-code-2015/data/"
main()
