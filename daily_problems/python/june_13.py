#!/usr/bin/env python

# This problem was asked by Google.
#
# Given the root to a binary tree, implement serialize(root), which serializes 
# the tree into a string, and deserialize(s), which deserializes the string back into the tree.
#
# For example, given the following Node class
#
# class Node:
#     def __init__(self, val, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
#
# The following test should pass:
#
# node = Node('root', Node('left', Node('left.left')), Node('right'))
# assert deserialize(serialize(node)).left.left.val == 'left.left'


from functools import *
import operator

class Node:
    def __init__(self, val, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    # Serialized representation of the B-Tree will be a list of 
    # 4-tuples \langle id, lc_id, rc_id, val \rangle 
    def serialize__(self, c_id=0, id_v=1):
        chk_none = lambda v, id_v : (0, id_v) if v is None else (id_v, id_v + 1)
        l_id, id_v = chk_none(self.left, id_v)
        r_id, id_v = chk_none(self.right, id_v)
        ser_str = '(%d, %d, %d, %s), ' % (c_id, l_id, r_id, self.val)
        if self.left is not None:
            app_str, id_v = self.left.serialize__(l_id, id_v)
            ser_str += app_str
        if self.right is not None:
            app_str, id_v = self.right.serialize__(r_id, id_v)
            ser_str += app_str
        return (ser_str, id_v)

    def serialize(self):
        return '[' + self.serialize__()[0][:-1] + ']'

    @staticmethod
    def deserialize(node_string):
        tuples = eval(node_string)
        return Node.deserialize__(tuples, tuples[0])

    @staticmethod
    def deserialize__(tuples, root):
        root_node = Node(root[3])
        if root[1] != 0:
            root_node.left = Node.deserialize__(tuples, find_t(root[1], tuples))
        if root[2] != 0:
            root_node.right = Node.deserialize__(tuples, find_t(root[2], tuples))
        return root_node

    def draw(self, ticks = 0, whitespace=-3, back_t='', bar=''):
        # 4 things that potentially need doing at each level: 
        #   (1) print appropriate number of whitespace charecters
        #   (2) print vertical lines
        #   (3) print dashes
        #   (4) print values
        ws = ' ' * whitespace
        ts = '-' * ticks
        val_str = '(%d)' % self.val
        
        print_str = ws + bar + back_t + ts + val_str
        print(print_str)
        if self.left is not None:
            self.left.draw(2, whitespace+4, back_t='`')
        if self.right is not None:
            self.right.draw(2, whitespace+4, back_t='`')
        
def find_t(v, ts):
    return [ t for t in ts if t[0] == v][0]

def main():
    head = Node(1, Node(2, Node(3, Node(8), Node(9)), Node(4, Node(10), Node(5))), Node(5, Node(6, Node(12), Node(13)), Node(7, Node(14), Node(15))))
    print('before serialization: ')
    head.draw()
    ser_str = head.serialize()
    deser = Node.deserialize(ser_str)
    print('after serialization: ')
    deser.draw()

if __name__ == '__main__':
    main()