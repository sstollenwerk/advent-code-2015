from itertools import count
from functools import partial
from hashlib import md5


def digest(s):
    return str(md5(bytes(s, encoding="ascii")).hexdigest())


def part1(key):
    return next(
        filter(lambda k: (digest(key+k)).startswith("00000"), map(str, count(1)))
    )

def part2(key):
    return next(
        filter(lambda k: (digest(key+k)).startswith("000000"), map(str, count(1)))
    )



def main():
    with open("../data/04.txt") as f:
        w = f.read()
        print(part1(w))
        print(part2(w))


if __name__ == "__main__":
    main()
