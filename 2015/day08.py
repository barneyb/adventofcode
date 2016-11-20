import unittest

INPUT = open("day08_input.txt").read().strip()
TEST_INPUT = """
""
"abc"
"aaa\\"aaa"
"\\x27"
"""

# str -> int
def lit_size(lit):
    # (bool, int) -> str -> (bool int):
    #   (True, n), 'x' -> (False, n - 1)
    #   (True, n), c -> (False, n + 1)
    #   (False, n), '\\' -> (True, n)
    #   (b, n), c -> (False, n + 1)
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

    return len(lit), reduce(char_val,
        lit[1:len(lit) - 1], # strip quotes
        (False, 0))[1]

# str -> int
def part_one(lines):
    return sum(map(
        lambda s: s[0] - s[1],
        map(
            lambda l: lit_size(l),
            lines.strip().split("\n"))))

# str -> int
def enc_size(lit):
    return len(lit), 2 + sum(map(
        lambda c: 2 if c == '\\' or c == '"' else 1,
        lit))

# str -> int
def part_two(lines):
    return sum(map(
        lambda s: s[1] - s[0],
        map(
            lambda l: enc_size(l),
            lines.strip().split("\n"))))

class TestExamples(unittest.TestCase):

    def testOneLit(self):
        self.assertEqual(lit_size('""'), (2, 0))

    def testTwoLit(self):
        self.assertEqual(lit_size('"abc"'), (5, 3))

    def testThreeLit(self):
        self.assertEqual(lit_size('"aaa\\"aaa"'), (10, 7))

    def testFourLit(self):
        self.assertEqual(lit_size('"\\x27"'), (6, 1))

    def testAllLit(self):
        self.assertEqual(part_one(TEST_INPUT), 12)

    def testOneEnc(self):
        self.assertEqual(enc_size('""'), (2, 6))

    def testTwoEnc(self):
        self.assertEqual(enc_size('"abc"'), (5, 9))

    def testThreeEnc(self):
        self.assertEqual(enc_size('"aaa\\"aaa"'), (10, 16))

    def testFourEnc(self):
        self.assertEqual(enc_size('"\\x27"'), (6, 11))

    def testAllEnc(self):
        self.assertEqual(part_two(TEST_INPUT), 19)

class TestParts(unittest.TestCase):
    def testPartOne(self):
        self.assertEqual(part_one(INPUT), 1333)
    def testPartTwo(self):
        self.assertEqual(part_two(INPUT), 2046)

unittest.main()
