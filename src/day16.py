#!/usr/bin/env pypy3

import sys

with open(sys.argv[1]) as f:
    l = f.readlines()
l = l[0].strip()

offs = int(l[:7])

print(len(l))

n1 = [int(i) for i in l]
n = n1 * 10000# 10000
print(len(n), n[:40])

for it in range(100):
    print(it)
    n2 = [0] * len(n)

    # Compute partial sums
    s = 0
    for i in range(len(n) - 1, -1, -1):
        s += n[i]
        n2[i] = s

    # For each ith digit, cycle S_i - S_(i*2) - S_(3i) + S_(4i) + S_(5i) - S_6i ... etc.
    cs = [1, -1, -1, 1]
    for ix in range(len(n)):
        i = ix
        s = 0
        c = 0
        for i in range(ix, len(n), ix + 1):
            s += cs[c] * n2[i]
            c = (c + 1) % 4
        n[ix] = abs(s) % 10

    print(n[:40])

print(n[:8])

print(offs, n[offs:offs+8])
