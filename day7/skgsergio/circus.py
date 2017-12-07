#!/usr/bin/env python3

import re
import sys


def part_1(nodes):
    # This is better solved by using graphviz or NetworkX :D
    all_subs = []

    for n in nodes:
        if nodes[n]['subs']:
            all_subs.extend(nodes[n]['subs'])

    return list(nodes.keys() - all_subs)[0]


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    nodes = {}
    with open(sys.argv[1], 'r') as f:
        for l in f:
            # Regex? WTF u are drunk.
            r = re.search(r"^(?P<node>\w+) \((?P<weight>\d+)\)(?: -> (?P<subs>[\w, ]+))?$", l)

            nodes[r.group('node')] = {
                'weight': r.group('weight'),
                'subs': r.group('subs').split(', ') if r.group('subs') else None
            }

    print("Part 1: {}".format(part_1(nodes)))
