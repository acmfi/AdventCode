#!/usr/bin/env python3
import sys

import enum
from dataclasses import dataclass
from typing import List, Set, Tuple, Optional


class Team(enum.Enum):
    IMMUNESYSTEM: int = enum.auto()
    INFECTION: int = enum.auto()


@dataclass
class Unit:
    uid: int
    team: Team
    num: int
    hp: int
    ad: int
    attk_type: str
    initiative: int
    immune: Set[str]
    weak: Set[str]
    target: Optional['Unit'] = None

    @property
    def pwr(self) -> int:
        return self.num * self.ad

    def deal_dmg(self, enemy: 'Unit') -> int:
        dmg = self.pwr

        if self.attk_type in enemy.immune:
            dmg = 0

        elif self.attk_type in enemy.weak:
            dmg *= 2

        return dmg

    def __hash__(self) -> int:
        return hash((self.uid, self.team, self.ad,
                     self.attk_type, self.initiative))


class Game:
    units: List[Unit]

    def __init__(self, lines: List[str], immune_system_boost: int = 0) -> None:
        self.units = []

        uid = 0
        team = Team.IMMUNESYSTEM

        for line in lines:
            if 'Immune System' in line:
                team = Team.IMMUNESYSTEM

            elif 'Infection' in line:
                team = Team.INFECTION

            elif line:
                desc = line.split()
                num = int(desc[0])
                hp = int(desc[4])
                ad = int(desc[-6])
                attk_type = desc[-5]
                initiative = int(desc[-1])

                immune = set()
                weak = set()

                if desc[7][0] == '(':
                    modifiers = line.split('(')[1].split(')')[0]

                    for mgroup in modifiers.split(';'):
                        mdesc = mgroup.split()
                        mtype = mdesc[0]

                        for d in mdesc[2:]:
                            if d[-1] == ',':
                                d = d[:-1]

                            if mtype == 'immune':
                                immune.add(d)
                            else:
                                weak.add(d)

                if team == Team.IMMUNESYSTEM:
                    ad += immune_system_boost

                self.units.append(Unit(uid, team, num, hp, ad, attk_type,
                                       initiative, immune, weak))

                uid += 1

    def run(self) -> Tuple[Team, int]:
        while True:
            # Target selection phase
            self._target_selection()

            # Attacking phase
            units_killed = self._attack()

            # Get number of alive units for each team
            n_immunesystem = sum([u.num for u in self.units if u.team == Team.IMMUNESYSTEM])
            n_infection = sum([u.num for u in self.units if u.team == Team.INFECTION])

            # We can end in an infinite loop if no units killed so
            # we exit giving the victory to the infection.
            if not units_killed or n_immunesystem == 0:
                return Team.INFECTION, n_infection

            elif n_infection == 0:
                return Team.IMMUNESYSTEM, n_immunesystem

    def _target_selection(self) -> None:
        # Sort units by power and then initiative
        self.units.sort(key=lambda u: (u.pwr, u.initiative), reverse=True)

        # Adquire targets for all units
        targeted: Set[Unit] = set()

        for u in self.units:
            # Clean previous target
            u.target = None

            # Get enemy units
            enemies = [e for e in self.units if e.team != u.team]

            # Get possible targets (the unit can deal damage to them)
            # ordered by the damage we can deal, its power and its initative.
            targets = sorted([e for e in enemies if e not in targeted and u.deal_dmg(e) > 0],
                             key=lambda x: (u.deal_dmg(x), x.pwr, x.initiative),
                             reverse=True)

            # If there are possible targets adquire the first one
            if targets:
                u.target = targets[0]
                targeted.add(targets[0])

    def _attack(self) -> bool:
        # Sort units by initiative
        self.units.sort(key=lambda u: u.initiative, reverse=True)

        # Perform the attacks
        units_killed = False
        for u in self.units:
            if u.target:
                # Grab the damage dealt
                dmg = u.deal_dmg(u.target)

                # Calc the number of kills
                kills = min(u.target.num, dmg // u.target.hp)

                if kills > 0:
                    units_killed = True
                    u.target.num -= kills

        # Remove extinguished units
        self.units = [u for u in self.units if u.num > 0]

        return units_killed


def solve(d):
    part1 = Game(d).run()[1]

    part2 = None
    boost = 0

    while not part2:
        winner, alive = Game(d, boost).run()

        if winner == Team.IMMUNESYSTEM:
            part2 = alive
        else:
            boost += 1

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
