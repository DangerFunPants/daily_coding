#!/usr/bin/env python3

from functools import *
import operator
import numpy as np

def count_ways(n, opts):
    heap = [[0]]
    counts = 0
    while len(heap) > 0:
        shortest = min(heap, key=lambda h_i : sum(h_i))
        heap.remove(shortest)
        for o in opts:
            sum_v = sum(shortest + [o])
            if sum_v < n:
                heap.append(shortest + [o])
            elif sum_v == n:
                counts = counts + 1
    return counts

def count_ways2(n, opts):
    if n == 0:
        return 1
    else:
        return sum([ count_ways2(n - o, opts ) for o in opts if (n - o) >= 0 ])

def graph_theoretic(n, opts):
    links = opts
    num_steps = n + 1
    mat = [ [ 1 if (i - j) in links else 0 for i in range(num_steps) ] for j in range(num_steps) ]
    mat = np.matrix(mat)
    sum_mat = np.matrix([ [ 0 for _ in range(num_steps) ] for _ in range(num_steps) ])
    for i in range(1, num_steps):
        pow_mat = mat**i
        sum_mat = sum_mat + pow_mat
    return sum_mat[0, n]

def test_driver(n, opts):
    print('---------------------------------------------------------------------')
    print('                         Pathfinding                                 ')
    print('---------------------------------------------------------------------')
    print('Links: %s, Steps: %d, Result: %d' % (opts, n, count_ways(n, opts)))
    print('\n')

    print('---------------------------------------------------------------------')
    print('                         Graph Theoretic                             ')
    print('---------------------------------------------------------------------')
    print('Links: %s, Steps: %d, Result: %d' % (opts, n, graph_theoretic(n, opts)))
    print('\n')

    print('---------------------------------------------------------------------')
    print('                         Discrete                                    ')
    print('---------------------------------------------------------------------')
    print('Links: %s, Steps: %d, Result: %d' % (opts, n, count_ways2(n, opts)))
    print('\n')

def main():
    test_driver(4, [1, 2])

if __name__ == '__main__':
    main()