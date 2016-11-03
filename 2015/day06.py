import unittest

INPUT = open("day06_input.txt").read().strip()

def parse_pair(pair):
    return complex(pair.replace(",", "+") + 'j')

def clean_ins(parts):
    return parts[1:] if parts[0] == "turn" else parts

def parse_ins(ins):
    parts = clean_ins(ins.split(" "))
    return parts[0], parse_pair(parts[1]), parse_pair(parts[3])

def parse_ins_list(ins_list):
    return map(parse_ins, filter(
        lambda l: len(l.strip()) > 0,
        ins_list.split("\n")))

def get_range(ins):
    return frozenset(reduce(
        lambda r, i: r + i,
        map(
            lambda r: map(
                lambda c: complex(r, c),
                xrange(int(ins[1].imag), int(ins[2].imag) + 1)),
            xrange(int(ins[1].real), int(ins[2].real) + 1)),
        []))

def do_ins_on(lit, ins_range):
    return lit | ins_range;

def do_ins_off(lit, ins_range):
    return get_outside(lit, ins_range)

def get_outside(base, target):
    return base - target

def do_ins_toggle(lit, ins_range):
    return frozenset(filter(
        lambda i: i != None,
        map(
            lambda i: None if i in lit else i,
            ins_range))) | get_outside(lit, ins_range)

def do_ins(lit, ins):
    return (do_ins_on if ins[0] == 'on' else do_ins_off if ins[0] == 'off' else do_ins_toggle)(lit, get_range(ins))

def count_lit(ins_list):
    return len(reduce(do_ins, parse_ins_list(ins_list), frozenset()))

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

class TestParts(unittest.TestCase):
    def testPartOne(self):
        self.assertEqual(count_lit(INPUT), 569999)

unittest.main()
