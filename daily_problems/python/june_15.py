#!/usr/bin/env python3

# This problem was asked by Jane Street.
#
# cons(a, b) constructs a pair, and car(pair) and cdr(pair) returns the first 
# and last element of that pair. For example, car(cons(3, 4)) returns 3, 
# and cdr(cons(3, 4)) returns 4.
#
# Given this implementation of cons:
#
# def cons(a, b):
#     def pair(f):
#         return f(a, b)
#     return pair
#
# Implement car and cdr.

def cons(a, b):
    def pair(f):
        return f(a, b)
    return pair

# Left projection
def car(p):
    return p(lambda a, _ : a)
    
# Right projection
def cdr(p):
    return p(lambda _, b : b)

def main():
    p = cons(1, 2)
    fst = car(p)
    snd = cdr(p)
    print('(%d, %d)' % (fst, snd))

if __name__ == '__main__':
    main()