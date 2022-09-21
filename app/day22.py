from typing import Callable

from dataclasses import dataclass, replace
import functools
from enum import Enum


@dataclass(frozen=True)
class Player:
    hp: int
    mp: int


@dataclass(frozen=True)
class Boss:
    hp: int
    damage: int


class Attack(Enum):
    MagicMissile = 53
    Drain = 73
    Shield = 113
    Poison = 173
    Recharge = 229


EffectStatus = frozenset[tuple[Attack, int]]
# emulate dict - need hashable

Node = tuple[Player, Boss, EffectStatus, bool, bool]

Edge = tuple[Node, int]


def weighted_graph_search(
    start: Node,
    neighbours: Callable[[Node], list[Edge]],
    success: Callable[[Node], bool],
    end_early: bool = False,
) -> dict[Node, int]:
    to_check = {start}

    seen = set()

    costs = {start: 0}

    res = {}
    while to_check:
        p = min(to_check, key=costs.get)
        if success(p):
            res[p] = costs[p]

        seen.add(p)
        to_check -= seen
        to_check -= {p}
        if res and end_early and min(res.values()) < costs[p]:
            break

        for (n, cost) in neighbours(p):
            costs[n] = min(costs[p] + cost, costs.get(n, float("inf")))
            if costs[n] == costs[p] + cost:
                states[n] = states[p] + [n]
            to_check.add(n)

        to_check -= seen
        to_check -= {p}
    return res


def success(state: Node):
    boss = state[1]
    return boss.hp <= 0


def apply_effects(
    player: Player, boss: Boss, status: dict[Attack, int]
) -> tuple[Player, Boss]:
    if status[Attack.Poison]:
        boss = replace(boss, hp=boss.hp - 3)
    if status[Attack.Recharge]:
        player = replace(player, mp=player.mp + 101)

    return player, boss


def player_move(
    player: Player, boss: Boss, status: dict[Attack, int], attack: Attack
) -> tuple[Player, Boss, EffectStatus,]:
    status = status.copy()
    if attack == Attack.MagicMissile:
        boss = replace(boss, hp=boss.hp - 4)
    elif attack == Attack.Drain:
        boss = replace(boss, hp=boss.hp - 2)
        player = replace(player, hp=player.hp + 2)
    elif attack == Attack.Shield:
        status[attack] = 6
    elif attack == Attack.Poison:
        status[attack] = 6
    elif attack == Attack.Recharge:
        status[attack] = 5

    else:
        raise ValueError(f"{attack=}")

    status_ = frozenset(status.items())
    return (player, boss, status_)


@functools.cache
def neighbours(state: Node):
    player, boss, status_, player_turn, hard = state
    assert player.mp >= 0
    status = dict(status_)
    player, boss = apply_effects(player, boss, status)
    new_status = {k: max(0, v - 1) for k, v in status.items()}

    if not player_turn:
        damage = max(1, boss.damage - 7 * bool(status[Attack.Shield]))
        hp = player.hp - damage
        player = replace(player, hp=hp)
        new_status_ = frozenset(new_status.items())
        return [((player, boss, new_status_, True, hard), 0)]
    else:
        if hard:
            player = replace(player, hp=player.hp - 1)
        poss_attacks = [
            i
            for i in Attack
            if i.value <= player.mp and new_status.get(i, 0) == 0 and player.hp > 0
        ]

        return [
            (
                player_move(
                    replace(player, mp=player.mp - a.value), boss, new_status, a
                )
                + (False, hard),
                a.value,
            )
            for a in poss_attacks
        ]


def part1(s: str) -> int:
    info = [int(L.split()[-1]) for L in s.split("\n")]
    boss = Boss(*info)
    player = Player(50, 500)

    return find_best(player, boss)


def find_best(player: Player, boss: Boss, hard: bool = False) -> int:
    status = {Attack.Poison: 0, Attack.Recharge: 0, Attack.Shield: 0}
    status_ = frozenset(status.items())

    r = weighted_graph_search(
        (player, boss, status_, True, hard), neighbours, success, True
    )

    print(r)
    return min(r.values())


def part2(s: str) -> int:
    info = [int(L.split()[-1]) for L in s.split("\n")]
    boss = Boss(*info)
    player = Player(50, 500)

    return find_best(player, boss, True)


def main():

    with open(directory + "22.txt", "r") as f:
        d = f.read().strip()
    print(f"{part1(d)=}")
    print(f"{part2(d)=}")


directory = "E:/important/programming/advent-code-2015/data/"
main()
