#!/usr/bin/env python3

# This problem was asked by Snapchat.
#
# Given an array of time intervals (start, end) for classroom lectures 
# (possibly overlapping), find the minimum number of rooms required.
#
# For example, given [(30, 75), (0, 50), (60, 150)], you should return 2.

def intersect(t1, t2):
    a, b = t1
    c, d = t2
    inter = min(b, d) - max(a, c)
    return True if inter > 0 else False

def intervals(lst):
    available = []
    count = 0
    lst = sorted(lst, key=lambda t : t[0])
    for e in lst:
        avs = list(filter(lambda i : i <= e[0], available))
        if not avs:
            count = count + 1
            available = available + [e[1]]
            continue
        i = available.index(avs[0])
        avs[i] = e[1]
    return count

def test1():
    cs = [ (30, 75), (0, 50), (60, 150) ]
    res = intervals(cs)
    print('Test1: %d' % res)

def test2():
    cs = [(0, 5), (4, 9), (8, 14)]
    for i in cs:
        for j in cs:
            print('%s %s %s' % (i, j, intersect(i, j)))
    res = intervals(cs)
    print('Test2: %d' % res)

def test3():
    cs = [(0, 10), (4, 9), (8, 14)]
    res = intervals(cs)
    print('Test3: %d' % res)

def main():
    test1()
    test2()
    test3()

if __name__ == '__main__':
    main()