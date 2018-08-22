#!/usr/bin/env python3
#
# This problem was asked by Microsoft.
#
# Given a dictionary of words and a string made up of those words (no spaces), 
# return the original sentence in a list. If there is more than one possible 
# reconstruction, return any of them. If there is no possible reconstruction, 
# then return null.
#
# For example, given the set of words 'quick', 'brown', 'the', 'fox', and the 
# string "thequickbrownfox", you should return ['the', 'quick', 'brown', 
# 'fox'].
#
# Given the set of words 'bed', 'bath', 'bedbath', 'and', 'beyond', and the 
# string "bedbathandbeyond", return either ['bed', 'bath', 'and', 'beyond] or 
# ['bedbath', 'and', 'beyond'].

def reconstruct_words(words, string):
    if not string:
        return []

    for w in words:
        if w == string[:len(w)]:
            return [w] + reconstruct_words(words, string[len(w):])

    return None

def test1():
    words = ['quick', 'brown', 'the', 'fox']
    string = 'thequickbrownfox'
    res = reconstruct_words(words, string)
    print(res)

def test2():
    words = ['bed', 'bath', 'bedbath', 'and', 'beyond']
    string = 'bedbathandbeyond'
    res = reconstruct_words(words, string)
    print(res)

def test3():
    words = ['bedbath', 'bed', 'bath', 'and', 'beyond']
    string = 'bedbathandbeyond'
    res = reconstruct_words(words, string)
    print(res)

def main():
    test1()
    test2()
    test3()

if __name__ == '__main__':
    main()