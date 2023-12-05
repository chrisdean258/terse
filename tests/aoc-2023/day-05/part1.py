#!/usr/bin/env python3

def get_val(ipt, m):
    for (dest, src, ll) in m:
        if src <= ipt < src + ll:
            return ipt - src + dest
    return ipt


a = open(0)
seeds = [int(s) for s in next(a).split(": ")[1].split()]
next(a)

try:
    while True:
        print(seeds)
        next(a)
        m = []
        while (line := next(a)) != "\n":
            m.append([int(s) for s in line.split()])
        # print(m)

        new_nums = []
        for seed in seeds:
            new_nums.append(get_val(seed, m))
        seeds = new_nums
except StopIteration:
    print(min(seeds))
