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


ok = set()


def mark_digits(line, i, j):
    if not 0 <= j < len(line):
        return
    if (i, j) in ok:
        return
    if is_digit(line[j]):
        ok.add((i, j))
        mark_digits(line, i, j - 1)
        mark_digits(line, i, j + 1)


lines = sys.stdin.read().splitlines()
orig = lines[:]

for i, line in enumerate(lines):
    for j, char in enumerate(line):
        if is_symbol(char):
            for neighbor in neighbors(i, j, len(lines), len(line)):
                mark_digits(lines[neighbor[0]], *neighbor)

s = 0
for i, line in enumerate(orig):
    j = 0
    while j < len(line):
        ll = 0
        while j+ll < len(line) and is_digit(line[j + ll]) and (i, j) in ok:
            ll += 1
        if ll > 0:
            print(int(line[j:j+ll]))
            s += int(line[j:j+ll])
        j += ll or 1
print(s)
