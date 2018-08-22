#!/usr/bin/env python3

# This problem was asked by Google.
#
# Given a singly linked list and an integer k, remove the kth last element
# from the list. k is guaranteed to be smaller than the length of the list.
#
# The list is very long, so making more than one pass is prohibitively
# expensive.
#
# Do this in constant space and in one pass.

class LL:
    def __init__(self, val, next=None):
        self.val = val
        self.next = next

    def kth_last(self, k):
        hi, lo = self, self
        for i in range(k):
            hi = hi.next
        while hi != None:
            hi = hi.next
            lo = lo.next
        return lo
    
    def __str__(self):
        return '(Node %s)' % str(self.val)

def test1():
    head = LL(0, LL(1, LL(2, LL(3, LL(4, LL(5))))))
    res = head.kth_last(1)
    print(res)

def main():
    test1()

if __name__ == '__main__':
    main()
    
