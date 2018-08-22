#!/usr/bin/env python3

import random as rand

# You run an e-commerce website and want to record the last N order ids in a 
# log. Implement a data structure to accomplish this, with the following API:
#
#     record(order_id): adds the order_id to the log
#     get_last(i): gets the ith last element from the log. i is guaranteed to 
# be smaller than or equal to N.
#
# You should be as efficient with time and space as possible.

# This incredibly obvious solution takes O(N) space, practically constant time 
# insertion (I think python lists are essentially C++'esque vectors that 
# use list doubling to grow their size dynamically) and constant time lookups. 
class Log:
    def __init__(self):
        self.logs = []

    def record(self, order_id):
        self.logs.append(order_id)

    def get_last(self, i):
        if i > len(self.logs):
            raise ValueError('Invalid Index')
        return self.logs[len(self.logs) - 1 - i]

def main():
    l = Log()
    for i in range(20):
        l.record(rand.uniform(0, 1000))

    for i in range(20):
        print(l.get_last(i))


if __name__ == '__main__':
    main()