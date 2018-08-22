#!/usr/bin/env python3

# This problem was asked by Facebook.
#
# Given the mapping a = 1, b = 2, ... z = 26, and an encoded message, count the
# number of ways it can be decoded.
#
# For example, the message '111' would give 3, since it could be decoded as 'aaa', 'ka', and 'ak'.
#
# You can assume that the messages are decodable. For example, '001' is not allowed.

from functools import *
import operator

def is_decodable(cipher):
    decodables = [ str(i) for i in range(1, 27) ]
    return (cipher in decodables)

def count_messages(cipher):
    messages = [[cipher.pop(0)]]
    while cipher:
        new_messages = []
        current = cipher.pop(0)
        for msg in messages:
            # msg is [Str]
            # check if we can append the <current> to the last symbol in the 
            # list
            if is_decodable(msg[-1] + current):
                # in this case we create a 'branch' in the tree
                branch = [msg[:-1] + [msg[-1] + current]]
                new_messages = new_messages + branch
            new_messages = new_messages + [msg + [current]]
        messages = new_messages
    return messages

def count_messages2(msg, last=''):
    if not msg:
        return 1
    if is_decodable(last + msg[0]) and last is not '':
        return count_messages2(msg[1:], msg[0]) + count_messages2(msg[2:], last + msg[0])
    else:
        return count_messages2(msg[1:], msg[0])
        
def main():
    msg = list("111")
    res = count_messages2(msg)
    print(res)

if __name__ == '__main__':
    main()