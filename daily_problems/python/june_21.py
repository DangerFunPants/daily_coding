#!/usr/bin/env python3

# This problem was asked by Twitter.
#
# Implement an autocomplete system. That is, given a query string s and a set
# of all possible query strings, return all strings in the set that have s as a prefix.
#
# For example, given the query string de and the set of strings 
# [dog, deer, deal], return [deer, deal].
#
# Hint: Try preprocessing the dictionary into a more efficient data
# structure to speed up queries.

from functools import *

class Tree:
    def __init__(self, val, cs=[]):
        self.val = val
        self.cs = cs

def construct_tree(root, search_space):
    firsts = set([ a[0] for a in search_space if a ])
    groups = [ [ a for a in search_space if a[0] == l ] for l in firsts ]
    for g in groups:
        c = construct_tree(Tree(g[0][0]), [ v[1:] for v in g ] )
        root.cs = root.cs + [c]
    return root

def traverse(root, indent=""):
    print(indent + root.val)
    for c in root.cs:
        traverse(c, indent + "  ")

def autocomplete_simple(query_str, search_space):
    pairs = [ s for s in search_space if len([ a for (a, b) in zip(query_str, s) if a is b]) == len(query_str)]
    return list(pairs)

def possible_strs(root):
    if not root.cs:
        return [root.val]
    vs = []
    for c in root.cs:
        possibles = possible_strs(c)
        vs = vs + possibles
    return [ root.val + p for p in vs ]

def autocomplete_graph(query_str, root):
    if not query_str:
        return possible_strs(root)
    
    for c in root.cs:
        if c.val == query_str[0]:
            return autocomplete_graph(query_str[1:], c)
    
    return None

def main():
    # res = autocomplete("de", [ "dog", "deer", "deal" ])
    root = construct_tree(Tree(""), [ "dog", "deer", "deal" ])
    query_str = "de"
    res = autocomplete_graph(query_str, root)
    print([ query_str[:-1] + p for p in res])

    

if __name__ == '__main__':
    main()