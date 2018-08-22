#!/usr/bin/env python

import random as rand
from functools import *

def is_circle(p):
    return (p[0]**2 + p[1]**2) <= 1.0

def approx_pi(num_samples):
    samples = [ (rand.random(), rand.random()) for _ in range(num_samples) ]
    c = len(filter(is_circle, samples))
    area = float(c) / len(samples)
    return area * 4.0

def main():
    num_samples = 50000
    res = approx_pi(num_samples)
    print(res)

if __name__ == '__main__':
    main()