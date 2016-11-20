import unittest

INPUT = open("day06_input.txt").read().strip()

# str -> complex
def parse_pair(pair):
    return complex(pair.replace(",", "+") + 'j')

# str[] -> str[]
def clean_ins(parts):
    return parts[1:] if parts[0] == "turn" else parts

# str -> str[]
def parse_ins(ins):
    parts = clean_ins(ins.split(" "))
    return parts[0], parse_pair(parts[1]), parse_pair(parts[3])

# str -> str[][]
def parse_ins_list(ins_list):
    return map(parse_ins, filter(
        lambda l: len(l.strip()) > 0,
        ins_list.split("\n")))

# str[] -> complex[]
def get_range(ins):
    return reduce(
        lambda r, i: r + i,
        map(
            lambda r: map(
                lambda c: complex(r, c),
                xrange(int(ins[1].imag), int(ins[2].imag) + 1)),
            xrange(int(ins[1].real), int(ins[2].real) + 1)),
        [])

# complex[] -> complex[] -> complex[]
def get_outside(base, target):
    return base - target

# complex[] -> complex[] -> complex[]
instructions_one = {
    'on': lambda lit, ins_range: lit | ins_range,
    'off': get_outside,
    'toggle': lambda lit, ins_range: frozenset(filter(
        lambda i: i != None,
        map(
            lambda i: None if i in lit else i,
            ins_range))) | get_outside(lit, ins_range)
}

# complex[] -> str[] -> complex[]
def do_ins(lit, ins):
    return instructions_one[ins[0]](lit, frozenset(get_range(ins)))

# str -> int
def count_lit(ins_list):
    return len(reduce(do_ins, parse_ins_list(ins_list), frozenset()))

# complex[] -> complex[] -> complex[]
def do_sum_on(grid, ins_range):
    for c in ins_range:
        grid[c] = grid.get(c, 0) + 1
    return grid

# complex[] -> complex[] -> complex[]
def do_sum_off(grid, ins_range):
    for c in ins_range:
        grid[c] = max(0, grid.get(c, 0) - 1)
    return grid

# complex[] -> complex[] -> complex[]
def do_sum_toggle(grid, ins_range):
    for c in ins_range:
        grid[c] = grid.get(c, 0) + 2
    return grid

# complex[] -> str[] -> complex[]
def do_sum(grid, ins):
    return (do_sum_on if ins[0] == 'on' else do_sum_off if ins[0] == 'off' else do_sum_toggle)(grid, get_range(ins))

# str -> int
def sum_brightness(ins_list):
    return sum(reduce(do_sum, parse_ins_list(ins_list), dict()).values())

class TestExamples(unittest.TestCase):

    def testZero(self):
        self.assertEqual(count_lit("turn off 0,0 through 1,1"), 0)

    def testOn(self):
        self.assertEqual(count_lit("turn on 1,1 through 5,5"), 25)

    def testToggle(self):
        self.assertEqual(count_lit(
            "turn on 1,1 through 5,5\n" + 
            "toggle 1,1 through 5,1"
        ), 25 - 5)

    def testOff(self):
        self.assertEqual(count_lit(
            "turn on 1,1 through 5,5\n" + 
            "turn off 2,2 through 3,3"
        ), 25 - 4)

    def testBigger(self):
        self.assertEqual(count_lit(
            "turn on 0,0 through 99,99\n" # 10,000 on
            "turn off 0,50 through 99,51\n" # 200 off
            "toggle 50,0 through 51,99\n" # 196 off, 4 on
        ), 10000 - 200 - 196 + 4);

class TestExamplesTwo(unittest.TestCase):

    def testZero(self):
        self.assertEqual(sum_brightness("turn off 0,0 through 0,0"), 0)

    def testOn(self):
        self.assertEqual(sum_brightness(
            "turn on 1,1 through 5,5\n" + 
            "turn on 1,1 through 5,5"
        ), 50)

    def testOff(self):
        self.assertEqual(sum_brightness(
            "turn on 1,1 through 5,5\n" + 
            "turn on 1,1 through 5,5\n" + 
            "turn off 1,1 through 5,5"
        ), 25)

    def testOffLowerBound(self):
        self.assertEqual(sum_brightness(
            "turn on 1,1 through 5,5\n" + 
            "turn off 1,1 through 5,5\n" + 
            "turn off 1,1 through 5,5\n" + 
            "turn off 1,1 through 5,5"
        ), 0)

    def testToggle(self):
        self.assertEqual(sum_brightness(
            "toggle 1,1 through 5,5\n" + 
            "toggle 1,1 through 5,5"
        ), 100)


#class TestParts(unittest.TestCase):
#    def testPartOne(self):
#        self.assertEqual(count_lit(INPUT), 569999)
#
#    def testPartTwo(self):
#        self.assertEqual(sum_brightness(INPUT), 17836115)

unittest.main()
