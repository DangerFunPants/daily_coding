#!/usr/bin/env python3

class Node:
    def __init__(self, id):
        self.id = id
        self.parent = self

    def __str__(self):
        s = '(%d, %d)' % (self.id, self.parent)
        return s
    
    def find(self):
        if self.parent != self:
            return self.parent.find()
        return self.parent

    def union(self, rhs):
        lhs_parent = self.find()
        rhs_parent = rhs.find()

        if lhs_parent == rhs_parent:
            return False
        else:
            rhs_parent.parent = lhs_parent
            return True

class Graph:
    def __init__(self):
        self.edges = []
        self.nodes = []

    def add_node(self, n):
        if n in self.nodes:
            raise ValueError('Cannot add the same node to the graph twice.')
        self.nodes.append(n)
        # self.edges.append((n, n))

    def add_edge(self, n1, n2):
        if n1 not in self.nodes or n2 not in self.nodes:
            raise ValueError('Cannot add an edge between two non-existent nodes')
        if (n1, n2) in self.edges or (n2, n1) in self.edges:
            raise ValueError('Cannot insert the same edge into the graph twice.')
            
        self.edges.append((n1, n2))
        # self.edges.append((n2, n1))
    
    def get_adj_mat(self):
        adj = [ [ 1 if (i, j) in self.edges else 0 for i in self.nodes ] for j in self.nodes ]
        return adj

    def __str__(self):
        str_rep = ''
        adj = self.get_adj_mat()
        str_rep = str(self.nodes) + '\n'
        str_rep += '-' * (3 * len(self.nodes)) + '\n'
        for r in adj:
            str_rep += str(r) + '\n'
        return str_rep

def main():
    g = Graph()
    for i in range(4):
        g.add_node(i)

    g.add_edge(0, 1)
    g.add_edge(1, 2)
    g.add_edge(2, 3)
    g.add_edge(3, 0)
    print(g)

    ns = { n :  Node(n) for n in g.nodes }
    cycle = False
    for (u, v) in g.edges:
        u_root = ns[u].find()
        v_root = ns[v].find()
        if u_root == v_root:
            cycle = True
            break
        ns[u].union(ns[v])

    if cycle:
        print('Graph contains a cycle.')

if __name__ == '__main__':
    main()


