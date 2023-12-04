#!/usr/bin/env python3


multipliers = [1] * 1000000

s = 0
for line in open(0):
    multiplier = multipliers.pop(0)
    l, _, r = line.split(": ")[1].partition(" | ")
    winners = set(l.split()) & set(r.split())
    winners = [int(a) for a in winners]
    for i in range(len(winners)):
        multipliers[i] += multiplier
    s += multiplier

print(s)
