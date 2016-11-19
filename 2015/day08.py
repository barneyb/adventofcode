import unittest

INPUT = open("day08_input.txt").read().strip()
TEST_INPUT = """
""
"abc"
"aaa\\"aaa"
"\\x27"
"""

def char_val(a, c):
    if a[0]: # escaped char
        if c is 'x':
            return (False, a[1] - 1)
        else:
            return (False, a[1] + 1)
    elif c == '\\':
        return (True, a[1])
    else:
        return (False, a[1] + 1)

def lit_size(lit):
    return len(lit), reduce(char_val,
        lit[1:len(lit) - 1], # strip quotes
        (False, 0))[1]

def part_one(lines):
    return sum(map(
        lambda s: s[0] - s[1],
        map(
            lambda l: lit_size(l),
            lines.strip().split("\n"))))

class TestExamples(unittest.TestCase):

    def testOne(self):
        self.assertEqual(lit_size('""'), (2, 0))

    def testTwo(self):
        self.assertEqual(lit_size('"abc"'), (5, 3))

    def testThree(self):
        self.assertEqual(lit_size('"aaa\\"aaa"'), (10, 7))

    def testFour(self):
        self.assertEqual(lit_size('"\\x27"'), (6, 1))

    def testAll(self):
        self.assertEqual(part_one(TEST_INPUT), 12)

class TestParts(unittest.TestCase):
    def testPartOne(self):
        self.assertEqual(part_one(INPUT), 1333)

unittest.main()
