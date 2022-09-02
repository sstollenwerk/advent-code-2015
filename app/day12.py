import string

from itertools import groupby, accumulate

import json

def is_numeric(s:string) -> bool:
    return s in {'-'} | set(string.digits)

def to_nums(s:str) -> list[int]:
    return [int(''.join(v)) for k,v in groupby(s, key=is_numeric) if k]



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


def find_match(s:str) -> int:
    conv = lambda x: {'{':1, '}':-1}.get(x,0)
    vals = accumulate(map(conv, s))
    return list(vals).index(-1)

def remove_section(s:str, n:int) -> str:
    start = s.rfind('{',0, n)
    end = find_match(s[start+1:]) + start+2

    r = s[:start] + s[end:]
    return r

def part2alt(s:str) -> int:
    to_search = ":\"red\""
    # want to ignore the possiblity of red being a key. Not that that's a thing in the input
    # also ensures that every element checked is in a dict and not a list which makes code easier.
    if to_search not in s:
        return part1(s)
    p1 = s.index(to_search)
    r = remove_section(s, p1)

    return part2alt(r)

def part1(s:str) -> int:
    return sum(to_nums(s))

def main():

    with open(directory + "12.txt", "r") as f:
        d = f.read()
    print(f"{part1(d)=}")
    print(f"{part2(d)=}")
    print(f"{part2alt(d)=}")


directory = "E:/important/programming/advent-code-2015/data/"
main()
