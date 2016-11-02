import unittest
from operator import add

INPUT = open("day02_input.txt").read().strip()

def present_dim(spec):
    return map(int, spec.split("x"))

def paper_for_present(spec):
    d = present_dim(spec)
    sides = [
        d[0] * d[1],
        d[1] * d[2],
        d[2] * d[0]
    ]
    return reduce(add, map(lambda n: 2 * n, sides)) + min(sides)

def paper_for_presents(specs):
    return reduce(lambda a, n: a + n,
                  map(paper_for_present,
                      specs.split("\n")))

class TestPresentDim(unittest.TestCase):
    
    def testOne(self):
        self.assertEqual(present_dim('1x2x3'), [1, 2, 3])

class TestPaperForPresent(unittest.TestCase):

    def testPresentOne(self):
        self.assertEqual(paper_for_present("2x3x4"), 58)

    def testPresentTwo(self):
        self.assertEqual(paper_for_present("1x1x10"), 43)

class TestPaperForPresents(unittest.TestCase):

    def testListOfOne(self):
        self.assertEqual(paper_for_presents("1x1x10"), 43)

    def testListOfTwo(self):
        self.assertEqual(paper_for_presents("2x3x4\n1x1x10"), 58 + 43)

class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(paper_for_presents(INPUT), 1586300)

unittest.main()
