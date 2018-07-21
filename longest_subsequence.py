#!/usr/bin/env python3

# This problem was asked by Microsoft.
#
# Given an array of numbers, find the length of the longest increasing 
# subsequence in the array. The subsequence does not necessarily have to be contiguous.
#
# For example, given the array [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15], 
# the longest increasing subsequence has length 6: it is 0, 2, 6, 9, 11, 15.

# Brute-force solution: 
#   Compute the powerset of the xs and then filter based on the maintanence
#   of the ordering condition. Take the longest subset. O(2^n) due to powerset
#   construction.
# More nuanced solution:
#   consider each possible starting position for the sub-sequence, i.
#   for each i construct a tree whose root is the starting position and
#   whose children each represent a possible next element of the subsequence. 
#   
#   This construction can be accomplished (for each i) by iterating over the 
#   list and constructing a list of lists to which every element is appended to 
#   each list only if its value is greater than the last value in the list.
#
#   The approach described above runs in O(2^(n-1)) in the worst case?
#   
# 
def longest_sequence(xs):
    seq_len = 0
    longest = []
    for seq in [ xs[i:] for i in range(len(xs)) ]:
        if len(longest) > len(seq):
            return longest

        possible_seq = [[seq[0]]]
        for v in seq:
            next_possible = []
            for sub_seq in possible_seq:
                if sub_seq[-1] < v:
                    next_possible.append(sub_seq + [v])
                next_possible.append(sub_seq)
            possible_seq = next_possible

        longest_possible = sorted(possible_seq, key=len)[-1]
        if len(longest) < len(longest_possible):
            longest = longest_possible

    return longest

# Index the memo by a pair (pos, max)
def longest_sequence(xs, memo={}):
    
    

def main():
    v = longest_sequence([i for i in range(100)])
    print(v)
    print('Hola!')

if __name__ == '__main__':
    main()