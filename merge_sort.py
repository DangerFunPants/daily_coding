#!/usr/bin/env python3

def decompose(lst):
    if len(lst) < 2:
        return lst[0]

    mid_point = int(len(lst) / 2)
    sorted_left = decompose(lst[:mid_point])
    sorted_right = decompose(lst[mid_point:])
    return merge(sorted_left, sorted_right)

# Merge
def merge(lst1, lst2):
    merged = []
    while len(lst1) > 0 or len(lst2) > 0:
        if len(lst1) == 0:
            merged = merged + lst2
            lst2 = []
            continue
        if len(lst2) == 0:
            merged = merged + lst1
            lst1 = []
            continue
        if lst1[0] < lst2[0]:
            merged.append(lst1.pop(0))
        else:
            merged.append(lst2.pop(0))

    return merged

# Decomposition
def trad_merge(lst):
    if len(lst) < 2:
        return lst
    mid_point = int(len(lst) / 2)
    sorted_left = trad_merge(lst[:mid_point])
    sorted_right = trad_merge(lst[mid_point:])
    return merge(sorted_left, sorted_right)

def main():
    test1 = [[10, 15, 30], [12, 15, 20], [17, 20, 32]]
    res = decompose(test1)
    print(res)

def test_merge():
    test1 = [ i for i in range(10, 0, -1) ]
    print('Presort: %s' % test1)
    res = trad_merge(test1)
    print('Postsort: %s' % res)

if __name__ == '__main__':
    # main()
    test_merge()