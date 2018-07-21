#!/usr/bin/env python3

def bellman_ford(verts, edges, src):
    dist = { v : 1000000 for v in verts }
    pred = { v : None for v in verts }
    dist[src] = 0

    for v in verts:
        for (u,v,w) in edges:
            if (dist[u] + w) < dist[v]:
                dist[v] = dist[u] + w
                pred[v] = u

    for (u, v, w) in edges:
        if (dist[u] + w) < dist[v]:
            print("Negative weight cycle")

    return (dist, pred)

def bellman_ford_max_prod(verts, edges, src):
    dist = { v : 1000000000 for v in verts }
    pred = { v : None for v in verts }
    dist[src] = 1.0
    
    for v in verts:
        for (u,v,w) in edges:
            if (dist[u]*w) > dist[v]:
                dist[v] = dist[u]*w
                pred[v] = u

    for (u,v,w) in edges:
        if (dist[u] * w) > dist[v]:
            print("Negative weight cycle")

    return (dist, pred)


def add_edge(es, e):
    es = es + [ e, (e[1], e[0], e[2]) ]
    return es

def test1():
    vs = [ 0, 1, 2, 3 ]
    es = []
    es = add_edge(es, (1,3,3))
    es = add_edge(es, (1,0,2))
    es = add_edge(es, (1,2,2))
    es = add_edge(es, (0,2,1))
    es = add_edge(es, (2,3,1))
    bf = bellman_ford(vs, es, 0)
    print(bf[0])
    print(bf[1])

def test2():
    vs = [ 0, 1, 2, 3 ]
    es = []
    es = add_edge(es, (1,3,3))
    es = add_edge(es, (1,0,2))
    es = add_edge(es, (1,2,2))
    es = add_edge(es, (0,2,1))
    es = add_edge(es, (2,3,1))
    bf = bellman_ford_max_prod(vs, es, 0)
    print(bf[0])
    print(bf[1])




def main():
    test2()

if __name__ == '__main__':
    main()

