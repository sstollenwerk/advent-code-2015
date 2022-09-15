
from collections import deque
from itertools import groupby
from typing import Callable, TypeVar

from more_itertools import grouper

Node = TypeVar("Node")
Depth = int

Replaces = tuple[str, str]


def parse(s) -> tuple[Replaces, str]:
    sections = s.split('\n')
    molecule = sections[-1]
    repl = [tuple(r.split(" => ") ) for r in sections[:-2]]

    return repl, molecule

def replacements(s:str, r:Replaces) -> list[str]:
    a,b = r
    parts = s.split(a)
    res = []
    for i in range(1, len(parts)):
        x,y = parts[:i], parts[i:]
        res.append ( a.join(x) + b + a.join(y))
    return res

def all_replacements(transitions:list[Replaces], s:str) -> set[str]:
    parts = (replacements(s,i) for i in transitions)
    return set.union(*map(set, parts))


def part1(s):
    rx, s = parse(s)
    rx.sort(key = lambda x: len(x[1])*-1)
    return len(  all_replacements(rx, s ))

def bfs(start:Node, 
    children: Callable[[Node], list[Node]],
    success: Callable[[Node], bool]) -> int:
    
    vals:deque[tuple[Node,Depth]]  = deque( [ (start, 0) ])
    
    while vals:
        (n, cost) = vals.popleft()
        if success(n):
            return cost
        childs = ( (i, cost+1) for i in children(n) )
        vals.extend(childs)
        
def dfs(start:Node, 
    children: Callable[[Node], list[Node]],
    success: Callable[[Node], bool]) -> int:
    
    costs = {}
    
    best  = float('inf')
    
    i = 0
    def inner(node, cost):
        nonlocal best
        if costs.get(node, float('inf')) <= cost:
            return
        if node in costs and best  == float('inf'):
            return
        
        costs[node] = cost
        
        if best <= cost:
            return
        if success(node):
            best = min(best, cost)
            return

        [inner(n, cost+1) for n in children(node)]
        
    inner(start, 0)
    return best
    

def find_best_path(start:str, end:str, transitions:list[Replaces] ) -> int:
    
    success = lambda x: x == end
    children = lambda x: sorted( all_replacements(transitions, x), key=( lambda x:(len(x), x)))
    # bfs didn't seem to work well
    return dfs(start, children, success)

def flatten(c):return (a for b in c for a in b)

def find_reduction(rx, a):
    posses = {a}
    i = 0
    while posses:
        r = set.union(*(all_replacements(rx, i) for i in posses))
        if len(r) == 0:
            assert len(posses) == 1
            break
        i+=1
        posses = r
    res = posses.pop()
    best_steps = find_best_path(a, res, rx)
    assert i == best_steps
    return res, best_steps

def part2(s):
    rx, s = parse(s)
    rx = [i[::-1] for i in rx]

    a,b = list(map(set,map(flatten,  zip(*rx))))
    keep = (a-b)
    # items that aren't in any keys
    chunks = [ ''.join(v) for k,v in groupby(s, key = lambda x: x in keep)]
    total = 0
    while chunks:
        a = chunks.pop()
        res, part_steps = find_reduction(rx, a)
        total += part_steps
        if chunks:
            b = chunks.pop()
            chunks.append(b+res)
    return total



def main():

    with open(directory + "19.txt", "r") as f:
        d = f.read().strip()
    print(f"{part1(d)=}")
    print(f"{part2(d)=}")


directory = "E:/important/programming/advent-code-2015/data/"
main()
