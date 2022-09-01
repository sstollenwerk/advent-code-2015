import string

from itertools import groupby

import json

def is_numeric(s:string) -> bool:
    return s in {'-'} | set(string.digits)

def to_nums(s:str) -> list[int]:
    return [int(''.join(v)) for k,v in groupby(s, key=is_numeric) if k]

def part1(s:str) -> int:
    return sum(to_nums(s))

def part2(s:str) -> int:
    r = [json.loads(s)]
    r2 = go_through(r)
    return part1(str(r2))

def go_through(vals):
    if not isinstance(vals,(list, dict)):
        return vals
    parts = enumerate(vals) if type(vals) == list else list(vals.items())

    for i,el in parts:
        if to_delete(el):
            vals[i] = None
        else:
            vals[i] = go_through(el)
    return vals


def to_delete(vals):
    if type(vals) == dict:
        return "red" in vals.values()
    return False

def main():

    with open(directory + "12.txt", "r") as f:
        d = f.read()
    print(f"{part1(d)=}")
    print(f"{part2(d)=}")


directory = "E:/important/programming/advent-code-2015/data/"
main()
