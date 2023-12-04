#!/usr/bin/env python3

cards = {}
s = 0
for i, line in enumerate(open(0), start=1):
    l, _, r = line.split(": ")[1].partition(" | ")
    a = set(l.split()) & set(r.split())
    if len(a):
        s += 1 << (len(a) - 1)
print(s)
