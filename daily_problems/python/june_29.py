#!/usr/bin/env python3

# This problem was asked by Facebook.
#
# A builder is looking to build a row of N houses that can be of K different 
# colors. He has a goal of minimizing cost while ensuring that no two 
# neighboring houses are of the same color.
#
# Given an N by K matrix where the nth row and kth column represents the cost 
# to build the nth house with kth color, return the minimum cost which achieves 
# this goal

def min_cost(mat, current=-1, last=None, memo={}):
    current = current + 1
    if current == len(mat):
        return 0
    mins = []
    for color, cost in enumerate(mat[current]):
        if color != last:
            if (current, color) in memo:
                mins = mins + [memo[(current, color)]]
            else:
                c = cost + min_cost(mat, current, last=color, memo=memo)
                mins = mins + [c]
                memo[(current, color)] = c
    return min(mins)
    
def main():
    mat = [[1, 3, 5], [2, 20, 20], [8, 4, 6]]
    c = min_cost(mat)
    print(c)

if __name__ == '__main__':
    main()