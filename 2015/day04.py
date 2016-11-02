import unittest
import hashlib
from itertools import count, takewhile

INPUT = "ckczppom"

def md5(string):
    return hashlib.md5(string).hexdigest()

def mash(items):
    return reduce(lambda a, it: a + str(it), items, "")

def first_coin(key, prefix="00000"):
    return 1 + max(takewhile(
        lambda n: not md5(key + str(n)).startswith(prefix),
        count(1)))

class TestMd5(unittest.TestCase):

    def testPopsicles(self):
        self.assertEqual(
            md5("i eat popsicles"),
            "1122c07697c645c2602e7360f5ca5483")

class TestMash(unittest.TestCase):

    def testEmpty(self):
        self.assertEqual(mash([]), "")

    def testSingle(self):
        self.assertEqual(mash(["one"]), "one")

    def testMany(self):
        self.assertEqual(mash(["one", "two", "three"]), "onetwothree")

class TestExamples(unittest.TestCase):

    def testOne(self):
        self.assertEqual(first_coin("abcdef"), 609043)

    def testTwo(self):
        self.assertEqual(first_coin("pqrstuv"), 1048970)

class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(first_coin(INPUT), 117946)

    def testPartTwo(self):
        self.assertEqual(first_coin(INPUT, "000000"), 3938038)

unittest.main()
