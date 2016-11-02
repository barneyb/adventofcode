import unittest

INPUT = open("day05_input.txt").read().strip()

def vowel_count(s):
    return sum(map(lambda c: 1 if 'aeiou'.find(c) >= 0 else 0, list(s)))

def has_double_letter(s):
    return reduce(
        lambda a, c: {
            'last': c,
            'hit': a['hit'] or a['last'] == c
        },
        list(s),
        {'last': '', 'hit': False})['hit']

def has_any(s, ss):
    return any(map(lambda it: s.find(it) >= 0, ss))

def is_nice(s):
    return vowel_count(s) >= 3 and has_double_letter(s) and not has_any(s, [
        'ab', 'cd', 'pq', 'xy'
    ])

def has_dupe_letter_pair(s):
    return any(map(
        lambda i: s.find(s[i:i+2], i+2) >= i,
        xrange(0, len(s) - 2)))

def has_split_double(s):
    return any(map(
        lambda i: s[i] == s[i + 2],
        xrange(0, len(s) - 2)))

def is_nice2(s):
    return has_dupe_letter_pair(s) and has_split_double(s);

def count_nice(test, text):
    return len(filter(test, text.split("\n")))

class TestHasDupeLetterPair(unittest.TestCase):
    def testAbab(self):
        self.assertTrue(has_dupe_letter_pair("abab"))
    def testAbxab(self):
        self.assertTrue(has_dupe_letter_pair("abxab"))
    def testAaa(self):
        self.assertFalse(has_dupe_letter_pair("aaa"))
    def testAaaa(self):
        self.assertTrue(has_dupe_letter_pair("aaaa"))
    def testAaxaa(self):
        self.assertTrue(has_dupe_letter_pair("aaxaa"))

class TestExamples(unittest.TestCase):

    def testOne(self):
        self.assertTrue(is_nice("ugknbfddgicrmopn"))

    def testTwo(self):
        self.assertTrue(is_nice("aaa"))

    def testThree(self):
        self.assertFalse(is_nice("jchzalrnumimnmhp"))

    def testFour(self):
        self.assertFalse(is_nice("haegwjzuvuyypxyu"))

    def testFive(self):
        self.assertFalse(is_nice("dvszwmarrgswjxmb"))

    def testSix(self):
        self.assertTrue(is_nice2("qjhvhtzxzqqjkmpb"))

    def testSeven(self):
        self.assertTrue(is_nice2("xxyxx"))

    def testEight(self):
        self.assertFalse(is_nice2("uurcxstgmygtbstg"))

    def testNine(self):
        self.assertFalse(is_nice2("ieodomkazucvgmuy"))

    def testPathological(self):
        self.assertFalse(is_nice2("aaa"))
        self.assertTrue(is_nice2("xyxy"))

class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(count_nice(is_nice, INPUT), 258)

    def testPartTwo(self):
        self.assertEqual(count_nice(is_nice2, INPUT), 53)

unittest.main()
