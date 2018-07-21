#!/usr/bin/env python3

# This problem was asked by Google.
# 
# Given an array of strictly the characters 'R', 'G', and 'B', segregate the 
# values of the array so that all the Rs come first, the Gs come second, and 
# the Bs come last. You can only swap elements of the array.
# 
# Do this in linear time and in-place.
# 
# For example, given the array ['G', 'B', 'R', 'R', 'B', 'R', 'G'], it should 
# become ['R', 'R', 'R', 'G', 'G', 'B', 'B'].

from collections import defaultdict

def sort_colors(arr):
    d = defaultdict(int)
    for v in arr:
        d[v] = d[v] + 1
    
    i = 0
    for _ in range(d['R']):
        arr[i] = 'R'
        i += 1
    for _ in range(d['G']):
        arr[i] = 'G'
        i += 1
    for _ in range(d['B']):
        arr[i] = 'B'
        i += 1
    return arr

def test1():
    test_val = [ 'G', 'B', 'R', 'R', 'B', 'R', 'G']
    res = sort_colors(test_val)
    print(res)

def main():
    test1()

if __name__ == '__main__':
    main()
