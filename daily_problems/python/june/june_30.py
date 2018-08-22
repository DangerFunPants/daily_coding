#!/usr/bin/env python3

# This problem was asked by Google.
#
# Given two singly linked lists that intersect at some point, find the 
# intersecting node. The lists are non-cyclical.
#
# For example, given A = 3 -> 7 -> 8 -> 10 and B = 99 -> 1 -> 8 -> 10, return 
# the node with value 8.
#
# In this example, assume nodes with the same value are the exact same node 
# objects.
#
# Do this in O(M + N) time (where M and N are the lengths of the lists) and 
# constant space.

class LL:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

    def __str__(self):
        return '(Node %s)' % str(self.val)

def find_intersect(l1, l2):
    vs = set() # std::hash_set
    n1, n2 = l1, l2
    while n1 != None and n2 != None:
        vs.add(n1.val)
        if n2.val in vs:
            return n1
        n1 = n1.next
        n2 = n2.next

def test1():
    a = LL(3, LL(7, LL(8, LL(10))))
    b = LL(99, LL(1, LL(8, LL(10))))
    res = find_intersect(a, b)
    print(res)

def main():
    test1()

if __name__ == '__main__':
    main()