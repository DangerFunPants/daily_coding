#!/usr/bin/env python3

class Node:
    def __init__(self, val, lc=None, rc=None):
        self.val = val
        self.lc = lc
        self.rc = rc

def insert(tree, n, selector=lambda i : i):
    mag = selector(n)
    if mag < selector(tree.val):
        if tree.lc is None:
            tree.lc = Node(n)
        else:
            insert(tree.lc, n, selector)
    else:
        if tree.rc is None:
            tree.rc = Node(n)
        else:
            insert(tree.rc, n, selector)

def min(tree, selector=lambda i : i):
    if tree.lc is None:
        return tree.val
    else:
        return min(tree.lc, selector)

def max(tree, selector=lambda i : i):
    if tree.rc is None:
        return tree.val
    else:
        return max(tree.rc, selector)

def test_driver():
    root = Node([i for i in range(7)])
    for i in range(15):
        insert(root, [i] * i, len)

    print('Min: %s' % (min(root)))
    print('Max: %s' % (max(root)))

    for i in range(15, 30):
        insert(root, [i] * i, len)

    print('Min: %s' % (min(root)))
    print('Max: %s' % (max(root)))

def main():
    test_driver()

if __name__ == '__main__':
    main()
