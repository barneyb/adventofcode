import unittest
from operator import add, mul

INPUT = open("day02_input.txt").read().strip()

def present_dim(spec):
    return sorted(map(int, spec.split("x")))

def sides(d):
    return [
        d[0] * d[1],
        d[1] * d[2],
        d[2] * d[0]
    ]

def paper_for_present(d):
    return reduce(add, map(lambda n: 2 * n, sides(d))) + min(sides(d))

def ribbon_for_present(d):
    return 2 * d[0] + 2 * d[1] + reduce(mul, d)

def paper_for_presents(specs):
    return total_for_presents(paper_for_present, specs)

def ribbon_for_presents(specs):
    return total_for_presents(ribbon_for_present, specs)

def total_for_presents(to_num, specs):
    return reduce(add, map(to_num, map(present_dim, specs.split("\n"))))

class TestPresentDim(unittest.TestCase):

    def testOne(self):
        self.assertEqual(present_dim('1x2x3'), [1, 2, 3])

class TestPaperForPresent(unittest.TestCase):

    def testPresentOne(self):
        self.assertEqual(paper_for_present([2, 3, 4]), 58)

    def testPresentTwo(self):
        self.assertEqual(paper_for_present([1, 1, 10]), 43)

class TestPaperForPresents(unittest.TestCase):

    def testListOfOne(self):
        self.assertEqual(paper_for_presents("1x1x10"), 43)

    def testListOfTwo(self):
        self.assertEqual(paper_for_presents("2x3x4\n1x1x10"), 58 + 43)

class TestRibbonForPresent(unittest.TestCase):

    def testPresentOne(self):
        self.assertEqual(ribbon_for_present([2, 3, 4]), 34)

    def testPresentTwo(self):
        self.assertEqual(ribbon_for_present([1, 1, 10]), 14)

class TestRibbonForPresents(unittest.TestCase):

    def testListOfOne(self):
        self.assertEqual(ribbon_for_presents("1x10x1"), 14)

    def testListOfTwo(self):
        self.assertEqual(ribbon_for_presents("2x3x4\n1x1x10"), 34 + 14)

class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(paper_for_presents(INPUT), 1586300)

    def testPartTwo(self):
        self.assertEqual(ribbon_for_presents(INPUT), 3737498)

unittest.main()
