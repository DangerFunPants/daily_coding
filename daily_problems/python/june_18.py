#!/usr/bin/env python3

# This problem was asked by Google.

# A unival tree (which stands for "universal value") is a tree where all nodes
# under it have the same value.

# Given the root to a binary tree, count the number of unival subtrees.

# For example, the following tree has 5 unival subtrees:

#    0
#   / \
#  1   0
#     / \
#    1   0
#   / \
#  1   1

class Node:
    def __init__(self, val, lc=None, rc=None):
        self.val = val
        self.left = lc
        self.right = rc

def count_subtrees(root):
    if not (root.left or root.right):
        return (root.val, 1)
    
    left_st, left_count = count_subtrees(root.left)
    right_st, right_count = count_subtrees(root.right)
    if left_st == right_st == root.val:
        return (root.val, left_count + right_count + 1)
    else:
        return (root.val, left_count + right_count)

def main():
    test_tree = Node(0, Node(1), Node(0, Node(1, Node(1), Node(1)), Node(0)))
    res = count_subtrees(test_tree)
    print('Result is: %d' % res[1])

if __name__ == '__main__':
    main()