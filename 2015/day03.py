import unittest

INPUT = open("day03_input.txt").read().strip()

step_to_num_map = {
    '^':  1j,
    '>':  1,
    'v': -1j,
    '<': -1,
}

def houses_visited(steps):
    return len(reduce(
        lambda a, s: {
            'curr': a['curr'] + s,
            'houses': a['houses'] | set([a['curr'] + s])
        },
        map(step_to_num_map.get,
            list(steps)),
        {
            'curr': 0j,
            'houses': set([0j])
        })['houses'])
    
class TestExamples(unittest.TestCase):

    def testOne(self):
        self.assertEqual(houses_visited(">"), 2)
    
    def testTwo(self):
        self.assertEqual(houses_visited("^>v<"), 4)
    
    def testThree(self):
        self.assertEqual(houses_visited("^v^v^v^v^v"), 2)
    
class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(houses_visited(INPUT), 2592)

unittest.main()
