#!/usr/bin/env python3
from __future__ import annotations

import sys

from enum import Enum
from dataclasses import dataclass
from typing import NamedTuple, List, Dict, Set, Tuple, Optional


class Team(Enum):
    ELF: int = 0
    GOBLIN: int = 1


class Point(NamedTuple):
    x: int
    y: int

    @property
    def neighbors(self) -> List[Point]:
        return [self + p for p in {Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0)}]

    def __add__(self, o) -> Point:
        return Point(self.x + o.x, self.y + o.y)


@dataclass
class Unit:
    team: Team
    loc: Point
    hp: int = 200
    ap: int = 3

    @property
    def alive(self):
        return self.hp > 0


class Game:
    units: List[Unit]
    walls: Dict[Point, bool]
    elfs_cant_die: bool

    class ElfDeath(Exception):
        def __init__(self, unit: Unit) -> None:
            super().__init__(f"Elf died, need more AP: {unit}")

    def __init__(self, gmap_input: List[str], elf_ap: Optional[int] = None, elfs_cant_die: bool = False) -> None:
        self.units = []
        self.walls = {}
        self.elfs_cant_die = elfs_cant_die

        for x, line in enumerate(gmap_input):
            for y, ch in enumerate(line):
                self.walls[Point(x, y)] = ch == '#'

                if ch in 'EG':
                    u = Unit(
                        team=Team.ELF if ch == 'E' else Team.GOBLIN,
                        loc=Point(x, y)
                    )

                    if elf_ap and u.team == Team.ELF:
                        u.ap = elf_ap

                    self.units.append(u)

    def run(self) -> int:
        """
        Run the game until one team wins!

        Returns the outcome: number of full rounds that were
        completed (not counting the round in which combat ends)
        multiplied by the sum of the hit points of all remaining
        units at the moment combat ends.
        """

        r = 0

        while self._round():
            r += 1

        return r * sum(u.hp for u in self.units if u.alive)

    def _round(self) -> bool:
        """
        Runs a round of the game.

        Returns True if more rounds are neeeded to win.
        """

        for u in sorted(self.units, key=lambda u: u.loc):
            if u.alive and self._action(u):
                return False

        return True

    def _action(self, unit: Unit) -> bool:
        """
        Performs actions for one unit (movement and attack).

        Returns True if the the unit team won.
        """

        enemies = [e for e in self.units if unit.team != e.team and e.alive]

        # If there are no enemies we won
        if not enemies:
            return True

        # Check if there is any enemy in the unit neighbors
        enemy_neighbors = [e for e in enemies if e.loc in unit.loc.neighbors]

        # If there is no enemy near try to move towards one
        if not enemy_neighbors:
            # Get available possitions near enemies
            enemies_neighbors = set(p for e in enemies for p in e.loc.neighbors)
            other_unit_locs = set(u.loc for u in self.units if u.alive and unit != u)
            target_locs = set(p for p in enemies_neighbors if not self.walls[p] and p not in other_unit_locs)

            movement = self._move(unit.loc, target_locs)
            if movement:
                unit.loc = movement
                enemy_neighbors = [e for e in enemies if e.loc in unit.loc.neighbors]

        # If there is at least one enemy in the neighbors attack him
        if enemy_neighbors:
            # Get the enemy with the less hp, in case of tie use the minimum loc
            enemy = min(enemy_neighbors, key=lambda e: (e.hp, e.loc))
            # Attack the enemy
            enemy.hp -= unit.ap

            # If we are in a elfs_cant_die game raise an exception if an Elf dies
            if self.elfs_cant_die and enemy.team == Team.ELF and not enemy.alive:
                raise Game.ElfDeath(enemy)

        return False

    def _move(self, origin_loc: Point, target_locs: Set[Point]) -> Optional[Point]:
        """
        Calcs a movement, if possible, from an origin to any target location.

        Returns the new location if possible.
        """
        # Get locs for all alive units
        unit_locs = set(u.loc for u in self.units if u.alive)
        # Dictionary of possible movements
        possibles: Dict[Optional[Point], Tuple[int, Optional[Point]]]
        possibles = {origin_loc: (0, None)}
        # List of visits to perform
        visits = [(origin_loc, 0)]
        # Set of seen locs
        seen: Set[Point] = set()

        new_loc = None

        while visits:
            p, d = visits.pop(0)
            d += 1

            for n in p.neighbors:
                # We don't want to suffocate in a wall, right?
                if self.walls[n] or n in unit_locs:
                    continue

                # Save the possible movement if is not already listed or is nearer
                if n not in possibles or possibles[n] > (d, p):
                    possibles[n] = (d, p)

                # Add the neighbor to the visit list if not seen and not already in the list
                if n not in seen and not any(n == v[0] for v in visits):
                    visits.append((n, d))

            # Mark point as seen
            seen.add(p)

        # Get all the loc possible locations that are also target locs
        possible_targets = set((d, p) for p, (d, _) in possibles.items() if p in target_locs)

        # If there is any possible target then perform the movement
        if possible_targets:
            _, new_loc = min(possible_targets)

            while possibles[new_loc][0] > 1:
                new_loc = possibles[new_loc][1]

        return new_loc


def solve(d):
    part1 = Game(d).run()

    part2 = None
    p2_elf_ap = 4
    while not part2:
        try:
            part2 = Game(d, elf_ap=p2_elf_ap, elfs_cant_die=True).run()
        except Game.ElfDeath as e:
            p2_elf_ap += 1

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
