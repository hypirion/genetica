#! /usr/bin/env python

import sys, math

def read_file(loch):
    vector = []
    with open(loch) as f:
        for line in f:
            line = line.strip()
            l = [float(x) for x in line.split()]
            vector.append(l)
    return vector

if __name__ == '__main__':
    prefix = sys.argv[1]
    file_count = int(sys.argv[2])
    N = file_count
    fdata = []
    for i in xrange(1, file_count+1):
        fdata.append(read_file(prefix + str(i)))
    fdata = [zip(*r) for r in zip(*fdata)]
    for row in fdata:
        space = ''
        for cell in row:
            s = sum(cell)
            avg = s*1.0/N
            dev = sum([(x - avg) ** 2 for x in cell])
            std_dev = math.sqrt(dev/N)
            print avg, std_dev,
        print
