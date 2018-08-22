#!/usr/bin/env python3

# This problem was asked by Stripe.
#
# Given an array of integers, find the first missing positive integer in linear 
# time and constant space. In other words, find the lowest positive integer that
#  does not exist in the array. The array can contain duplicates and negative 
# numbers as well.
#
# For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
#
# You can modify the input array in-place.

from functools import *
import operator

def lowest_non_existent(xs):
    minimum = 1
    for x in xs:
        if x == minimum:
            minimum += 1
    return minimum

def main():
    lst = [x for x in range(20) ] + [21]
    v = lowest_non_existent(lst)
    print(v)
    
if __name__ == '__main__':
    main()