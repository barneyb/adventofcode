import unittest

INPUT = open("day03_input.txt").read().strip()

step_to_num_map = {
    '^':  1j,
    '>':  1,
    'v': -1j,
    '<': -1,
}

def house_count(steps):
    return len(houses(steps))

def houses(steps):
    return reduce(
        lambda a, s: {
            'curr': a['curr'] + s,
            'houses': a['houses'] | set([a['curr'] + s])
        },
        map(step_to_num_map.get,
            list(steps)),
        {
            'curr': 0j,
            'houses': set([0j])
        })['houses']

def house_count_robo(steps):
    return len(houses(steps[0:len(steps):2]) | houses(steps[1:len(steps):2]))
    
class TestExamples(unittest.TestCase):

    def testOne(self):
        self.assertEqual(house_count(">"), 2)
    
    def testTwo(self):
        self.assertEqual(house_count("^>v<"), 4)
    
    def testThree(self):
        self.assertEqual(house_count("^v^v^v^v^v"), 2)

    def testFour(self):
        self.assertEqual(house_count_robo("^v"), 3)
    
    def testFive(self):
        self.assertEqual(house_count_robo("^>v<"), 3)
    
    def testSix(self):
        self.assertEqual(house_count_robo("^v^v^v^v^v"), 11)
    
class TestParts(unittest.TestCase):

    def testPartOne(self):
        self.assertEqual(house_count(INPUT), 2592)

    def testPartTwo(self):
        self.assertEqual(house_count_robo(INPUT), 2360)

unittest.main()
