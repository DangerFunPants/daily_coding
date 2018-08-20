# This problem was asked by Facebook.
# 
# Given a list of integers, return the largest product that can be made by 
# multiplying any three integers.
# 
# For example, if the list is [-10, -10, 5, 2], we should return 500, since 
# that's -10 * -10 * 5.

from functools import reduce
import operator as op

def maxProd(s):
    p = reduce(op.mul, s[:3])
    for i in range(len(s) - 2):
        for j in range(i+1, len(s) - 1):
            for k in range(j+1, len(s)):
                if (s[i] * s[j] * s[k]) > p:
                    p = s[i] * s[j] * s[k]
    return p

def test1():
    ts = [ -10, -10, 5, 2 ]
    res = maxProd(ts)
    print(res)

def main():
    test1()

if __name__ == '__main__':
    main()
