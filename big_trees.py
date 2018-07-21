#!/usr/bin/env python3

import random

class Node:
    def __init__(self, v, lc=None, rc=None):
        self.val = v
        self.lc = lc
        self.rc = rc

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
        if self.lc is not None:
            self.lc.draw(2, whitespace+4, back_t='`')
        if self.rc is not None:
            self.rc.draw(2, whitespace+4, back_t='`')

def random_tree():
    root = Node(0)
    if random.random() < 0.5:
        root.lc = random_tree()
    if random.random() < 0.5:
        root.rc = random_tree()
    return root
    
def main():
    tree = random_tree()
    tree.draw()

if __name__ == '__main__':
    main()