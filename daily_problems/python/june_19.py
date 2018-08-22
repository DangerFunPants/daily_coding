#!/usr/bin/env python3

# This problem was asked by Airbnb.
#
# Given a list of integers, write a function that returns the largest sum of 
# non-adjacent numbers. Numbers can be 0 or negative.
#
# For example, [2, 4, 6, 2, 5] should return 13, since we pick 2, 6, and 5. 
# [5, 1, 1, 5] should return 10, since we pick 5 and 5.
#
# Follow-up: Can you do this in O(N) time and constant space?

def max_sum(lst):
    sums = 0
    prev = None
    for i, v in enumerate(lst):
        if prev is None and v > 0:
            sums += v
            prev = ([v], [])
        else:
            if sum(prev[1]) + v > sum(prev[0]):
                sums = sums - sum(prev[0]) + sum(prev[1]) + v
                prev = (prev[1] + [v], prev[0])
            else:
                prev = None

    return sums

def main():
    test1 = [2, 4, 6, 2, 5]
    test2 = [5, 1, 1, 5]
    test3 = [2, 4, 6, 9, 5]
    for t in [test1, test2, test3]:
        res = max_sum(t)
        print('%s -> %s' % (repr(t), repr(res)))

if __name__ == '__main__':
    main()