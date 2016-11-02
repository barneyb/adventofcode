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

def count_nice(text):
    return len(filter(is_nice, text.split("\n")))

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

class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(count_nice(INPUT), 258)

unittest.main()
