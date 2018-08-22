#!/usr/bin/env python3

# This problem was asked by Google.
#
# Implement locking in a binary tree. A binary tree node can be locked or 
# unlocked only if all of its descendants or ancestors are not locked.
#
# Design a binary tree node class with the following methods:
#
#     * is_locked, which returns whether the node is locked
#     * lock, which attempts to lock the node. If it cannot be locked, then it 
#       should return false. Otherwise, it should lock it and return true.
#     * unlock, which unlocks the node. If it cannot be unlocked, then it should 
#       return false. Otherwise, it should unlock it and return true.
#
# You may augment the node to add parent pointers or any other property you 
# would like. You may assume the class is used in a single-threaded program, 
# so there is no need for actual locks or mutexes. Each method should run in 
# O(h), where h is the height of the tree.

class Node:
    def __init__(self, lc=None, rc=None, parent=None, locked=False):
        self.lc = lc
        self.rc = rc
        self.parent = parent
        self.locked = locked
        self.lock_count = 0

    def is_locked(self):
        return self.locked

    def inc_parent_lc(self):
        if self.parent:
            self.parent.lock_count = self.parent.lock_count + 1
            self.parent.inc_parent_lc()
        else:
            return

    def lock(self):
        if self.all_ps_unlocked() and self.all_cs_unlocked():
            self.locked = True
        return self.locked

    def unlock(self):
        if self.all_ps_unlocked and self.all_cs_unlocked():
            self.locked = False
        return not self.locked

    def all_cs_unlocked(self):
        return self.lock_count == 0 
        

    def all_ps_unlocked(self):
        if not self.parent:
            return True
        if self.parent.is_locked():
            return False
        else:
            return self.parent.all_ps_unlocked()

def set_parents(bt):
    if bt.lc and bt.rc:
        bt.lc.parent = bt
        bt.rc.parent = bt
        set_parents(bt.lc)
        set_parents(bt.rc)
    

def test1():
    tr = Node(Node(Node(), Node()), Node(Node(), Node()))
    set_parents(tr)
    print(tr.all_cs_unlocked())
    print(tr.rc.lc.all_ps_unlocked())
    print(tr.lock())
    print(tr.rc.lock())
    print(tr.unlock())
    print(tr.rc.lock())


def main():
    test1()

if __name__ == '__main__':
    main()
