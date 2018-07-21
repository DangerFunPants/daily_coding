#!/usr/bin/env python3

# This problem was asked by Amazon.
#
# Given an integer k and a string s, find the length of the longest substring 
# that contains at most k distinct characters.
#
# For example, given s = "abcba" and k = 2, the longest substring with k 
# distinct characters is "bcb".

# O(n^2) in the worst case.
def longest_distinct(str_val, k):
    max_len = 0
    for i in range(len(str_val)):
        sub = str_val[i:]
        ns = set()
        count = 0
        while sub:
            ns.add(sub[0])
            if (len(ns) <= k):
                count = count + 1
                sub = sub[1:]
            else:
                break
        if count > max_len:
            max_len = count
    return max_len

def longest_distinct_lin(str_val, k):
    longest = ''
    k_sub1 = {}
    order = []
    ks = set()
    w = ''
    for i, read in enumerate(str_val):
        k_len = len(ks)
        ks.add(read)
        if len(ks) > k_len:
            order = [read] + order
        k_sub1[read] = i
        if len(ks) > k:
            to_remove = order[-1]
            order = order[:-1]
            k_sub = k_sub1[to_remove] + 1
            if len(w) > len(longest):
                longest = w
            w = w[k_sub:]
            ks.remove(to_remove)
            print(w)
        w = w + read

    return w if not longest else longest

def main():
    res = longest_distinct_lin('abcba', 2)
    print('Res is: %s' % res)
    print('Hola Mundas!')

if __name__ == '__main__':
    main()