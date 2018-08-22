#!/usr/bin/env python3

# This problem was asked by Google.
#
# You are given an M by N matrix consisting of booleans that represents a 
# board. Each True boolean represents a wall. Each False boolean represents a 
# tile you can walk on.
#
# Given this matrix, a start coordinate, and an end coordinate, return the 
# minimum number of steps required to reach the end coordinate from the start. 
# If there is no possible path, then return null. You can move up, left, down, 
# and right. You cannot move through walls. You cannot wrap around the edges 
# of the board.
#
# For example, given the following board:
#
# [[f, f, f, f],
# [t, t, f, t],
# [f, f, f, f],
# [f, f, f, f]]
#
# and start = (3, 0) (bottom left) and end = (0, 0) (top left), the minimum 
# number of steps required to reach the end is 7, since we would need to go 
# through (1, 2) because there is a wall everywhere else on the second row.
from functools import *
import operator as op

def build_graph(mat):
    nodes = { (i, j) : [] for i, row in enumerate(mat) for (j, b_val) in enumerate(row) if not b_val }
    for i, row in enumerate(mat):
        for j, t in enumerate(row):
            if not t:
                n = nodes[(i, j)]
                try:
                    if not mat[i][j + 1]:
                        n.append((i, j+1))
                except IndexError:
                    pass
                try:
                    if j-1 >= 0 and not mat[i][j-1]:
                        n.append((i, j-1))
                except IndexError:
                    pass
                try:
                    if not mat[i+1][j]:
                        n.append((i+1, j))
                except IndexError:
                    pass
                try:
                    if i-1 >= 0 and not mat[i-1][j]:
                        n.append((i-1, j))
                except IndexError:
                    pass
    return nodes

def bfs(paths, end, adj_mat):
    end_paths = [ p for p in paths if p[-1] == end]
    if len(end_paths) > 0:
        return end_paths
    next_set = []
    for p in paths:
        for adj in adj_mat[p[-1]]:
            if adj not in p:
                next_set.append(p + [adj])
    
    if not next_set:
        return None
    return bfs(next_set, end, adj_mat)

def shortest(mat, start, end):
    g = build_graph(mat)
    ps = bfs([[start]], end, g)
    # count hops not nodes. 
    return len(min(ps, key=lambda ls : len(ls)))-1

def test1():
    mat = [ [False, False, False, False]
          , [True, True, False, True]
          , [False, False, False, False]
          , [False, False, False, False]
          ]
    res = shortest(mat, (3, 0), (0, 0))
    print(res)

def main():
    test1()

if __name__ == '__main__':
    main()    


