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


def get_weights(nodes, node):
    weights = []

    for child in nodes[node]['subs']:
        value = nodes[child]['weight']

        if nodes[child]['subs']:
            sub_nodes, sub_weigths = get_weights(nodes, child)

            if len(set(sub_weigths)) != 1:
                return sub_nodes, sub_weigths

            value += sum(sub_weigths)

        weights.append(value)

    return nodes[node]['subs'], weights


def part_2(nodes):
    root = part_1(nodes)

    sub_nodes, sub_weights = get_weights(nodes, root)

    if len(set(sub_weights)) != 1:
        balanced_weight = max(set(sub_weights), key=sub_weights.count)

        unbalanced_idx = [idx
                          for idx, weight in enumerate(sub_weights)
                          if weight != balanced_weight][0]

        n = sub_nodes[unbalanced_idx]
        w = sub_weights[unbalanced_idx]

        good_w = nodes[n]['weight'] + (balanced_weight - w)

        return "Unbalanced node '{}', weight {} must be {}.".format(
            n,
            nodes[n]['weight'],
            good_w
        )

    else:
        return "It is balanced"


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
                'weight': int(r.group('weight')),
                'subs': r.group('subs').split(', ') if r.group('subs') else None
            }

    print("Part 1: {}".format(part_1(nodes)))
    print("Part 2: {}".format(part_2(nodes)))
