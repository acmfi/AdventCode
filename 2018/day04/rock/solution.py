#!/usr/bin/env python3

from functools import reduce
from collections import defaultdict
from itertools import zip_longest


# From https://docs.python.org/3/library/itertools.html#itertools-recipes
def grouper(iterable, n, fillvalue=None):
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)


def investigate(state, l):
  action = l.split(']')[1][1:]
  da = l.split(']')[0][1:].split()
  minute = int(da[1].split(':')[-1])
  date = da[0]
  if 'Guard #' in action:
    id = action.split('#')[1].split()[0]
    state['active'] = id
  elif 'falls asleep' in action:
    state[state['active']][date].append(minute)
  elif 'wakes up' in action:
    id = state['active']
    last_asleep = state[id][date][-1]
    state[id][date].append(minute)
  return state


def analyze_guard(x):
  id = x[0]
  dates = x[1]
  total = 0
  max_asleep = 0
  minutes = defaultdict(int)
  for _, times in dates.items():
    for s, e in grouper(times, 2):
      shift = e - s
      total += shift
      for i in range(s, e):
        minutes[i] += 1

  max_minute = max(minutes.items(), key=lambda x: x[1])
  return {"id": id, "total": total, 'max_minute': max_minute[0]}


def which_guard(results, key):
  guard = max(map(analyze_guard, results.items()), key=lambda x: x[key])
  print(guard)
  print(int(guard['id']) * guard['max_minute'])
  return guard


def solution1(results):
  which_guard(results, 'total')


def solution2(results):
  which_guard(results, 'max_minute')


path = './input'
lines = open(path, 'r').readlines()

results = reduce(investigate, sorted(lines), defaultdict(lambda: defaultdict(list)))

results.pop('active')

print("Solution 1")
solution1(results)

print("Solution 2")
solution2(results)
