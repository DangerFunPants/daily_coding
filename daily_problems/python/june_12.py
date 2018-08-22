#!/usr/bin/python3

# This problem was asked by Uber.
#
# Given an array of integers, return a new array such that each element at index
#  i of the new array is the product of all the numbers in the original array 
# except the one at i.
#
# For example, if our input was [1, 2, 3, 4, 5], the expected output would be 
# [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].
#
# Follow-up: what if you can't use division?

from functools import *
import operator

def main():
    lst = [i for i in range(1, 6)]
    sum_val = reduce(lambda l1, l2 : l1 * l2, lst)
    res = [sum_val * float(i)**-1 for i in lst]
    print(res)

if __name__ == '__main__':
    main()