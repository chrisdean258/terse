#!/usr/bin/env python3

import sys


def neighbors(i, j, w, le):
    for di in range(-1, 2):
        for dj in range(-1, 2):
            if 0 <= di+i < le and 0 <= dj+j < w:
                yield (di + i, dj + j)


def is_symbol(c):
    return c != '.' and not '0' <= c <= '9'


def is_digit(c):
    return '0' <= c <= '9'


def find_num(line, i, j):
    if is_digit(line[j]):
        while j > 0 and is_digit(line[j-1]):
            j -= 1
        ll = j
        while ll < len(line) and is_digit(line[ll]):
            ll += 1
        rtn = int("".join(line[j:ll]))
        line[j:ll] = ['.'] * (ll-j)
        return rtn


lines = [[a for a in ll] for ll in sys.stdin.read().splitlines()]

s = 0
for i, line in enumerate(lines):
    for j, char in enumerate(line):
        if is_symbol(char):
            count = 0
            tot = 1
            for neighbor in neighbors(i, j, len(lines), len(line)):
                num = find_num(lines[neighbor[0]], *neighbor)
                if num is not None:
                    count += 1
                    tot *= num
            if count == 2:
                s += tot
print(s)
