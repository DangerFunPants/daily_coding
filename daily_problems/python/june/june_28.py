#!/usr/bin/env python3

# Given an array of integers and a number k, where 1 <= k <= length of the 
# array, compute the maximum values of each subarray of length k.
#
# For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: 
# [10, 7, 8, 8], since:
#
# 10 = max(10, 5, 2)
# 7  = max(5, 2, 7)
# 8  = max(2, 7, 8)
# 8  = max(7, 8, 7)
#
# Do this in O(n) time and O(k) space. You can modify the input array in-place 
# and you do not need to store the results. You can simply print them out as 
# you compute them.

from collections import deque

def maxs(arr, k):
    q = deque()
    m = arr[k-1] - 1
    for i in range(k-1, -1, -1):
        if arr[i] > m:
            q.append((i))
            m = arr[i]
    q = deque(sorted(q, key=lambda v : arr[v]))
    print(q)

    for i in range(k, len(arr)):
        print(arr[q[-1]])

        while q and q[-1] <= i - k:
            q.pop()

        while q and arr[i] > arr[q[0]]:
            q.popleft()
        q.appendleft(i)
    
    print(arr[q[-1]])

def test1():
    arr = [10, 5, 2, 7, 8, 7]
    k = 3
    maxs(arr, k)

def main():
    test1()

if __name__ == '__main__':
    main()