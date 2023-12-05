#!/usr/bin/env python3


def intersect(test, r):
    tests, testl = test
    rs, rl = r
    left = None
    right = None
    if tests < rs:
        left = (tests, min(rs - tests, testl))
        if left[1] == testl:
            return left, None, None
        testl -= rs - tests
        tests = rs
    if not (rs <= tests < rs + rl):
        return None, None, test

    if tests + testl > rs + rl:
        right = (rs + rl, tests + testl - (rs + rl))
        testl -= right[1]

    return left, (tests, testl), right


def get_val(ipt, m):
    rvs = []
    vals = [ipt]
    while vals:
        v = vals.pop()
        for mapping in m:
            dst, src, ll = mapping
            l, intersection, r = intersect(v, (src, ll))
            if l == v or r == v:
                continue
            if l:
                vals.append(l)
            if r:
                vals.append(r)
            if intersection:
                rvs.append((intersection[0] - src + dst, intersection[1]))
                break

        else:
            rvs.append(v)

    return rvs



a = open(0)
seeds = [int(s) for s in next(a).split(": ")[1].split()]
next(a)

s = []
for start, ll in zip(seeds[::2], seeds[1::2]):
    s.append((start, ll))

seeds = s

try:
    while True:
        next(a)
        m = []
        while (line := next(a)) != "\n":
            m.append([int(s) for s in line.split()])

        new_nums = []
        for seed in seeds:
            new_nums += get_val(seed, m)
        seeds = new_nums

except StopIteration:
    print(min(seeds))
