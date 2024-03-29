import unittest
import operator as op

INPUT = open("day07_input.txt").read().strip()
TEST_INPUT = """
123 -> x
x LSHIFT 2 -> f
y RSHIFT 2 -> g
456 -> yy
yy -> y
x AND y -> d
x OR y -> e
NOT x -> h
NOT y -> i
"""

ops = {
    # int -> int
    'NOT': lambda n: 65536 + ~n,
    # int -> int -> int
    'AND': op.and_,
    'OR': op.or_,
    'LSHIFT': op.lshift,
    'RSHIFT': op.rshift
}

# str[] -> item
parsers = {
    1: lambda ps: wire_or_signal(ps[0]),
    2: lambda ps: UnaryGate(ops[ps[0]], wire_or_signal(ps[1])),
    3: lambda ps: BinaryGate(
        ops[ps[1]],
        wire_or_signal(ps[0]),
        wire_or_signal(ps[2]))
}

# str -> item
def wire_or_signal(name):
    try:
        return Signal(int(name))
    except ValueError:
        return Wire(name)

# Hashable a => dict[a,b] -> (a, b) -> dict
def _dict_plus_key(d, kvpair):
    # this is my "immutably extend a dict with a new key" operator
    d = dict(d)
    d[kvpair[0]] = kvpair[1]
    return d

# str[] -> item
def parse_src(src):
    return parsers[len(src)](src)

# str[] -> (str, item)
def parse_wire(parts):
    return parts[len(parts) - 1], parse_src(parts[:len(parts) - 2])

# str -> dict[str,item]
def wire_circuit(booklet):
    return reduce(
        lambda a, l: _dict_plus_key(a, parse_wire(l)),
        map(
            lambda l: l.split(" "),
            booklet.strip().split("\n")),
        {})

# dict[str,item] -> str -> int
def eval_src(circuit, source):
    return ((circuit, source) if isinstance(source, int)
            else (circuit, source.signal) if isinstance(source, Signal)
            else eval_src(circuit, circuit[source]) if isinstance(source, str)
            else source.eval(circuit))

# str -> int
def get_wire_value(booklet, name):
    return eval_src(wire_circuit(booklet), name)[1]

# str -> int
def part_two(booklet):
    return get_wire_value(booklet + "\n" + str(get_wire_value(booklet, 'a')) + " -> b", 'a')

class Wire:

    def __init__(self, ref):
        self.ref = ref

    def eval(self, circuit):
        v = eval_src(circuit, self.ref)
        return _dict_plus_key(v[0], (self.ref, v[1])), v[1]

class Signal:

    def __init__(self, signal):
        self.signal = signal

    def eval(self, circuit):
        return circuit, self.signal

class UnaryGate:

    def __init__(self, op, source):
        self.op = op
        self.source = source

    def eval(self, circuit):
        v = eval_src(circuit, self.source)
        return v[0], self.op(v[1])

class BinaryGate:

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def eval(self, circuit):
        l = eval_src(circuit, self.left)
        r = eval_src(l[0], self.right)
        return r[0], self.op(l[1], r[1])

class TestExamples(unittest.TestCase):

    def testX(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'x'), 123)

    def testY(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'y'), 456)

    def testYY(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'yy'), 456)

    def testD(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'd'), 72)

    def testE(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'e'), 507)

    def testF(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'f'), 492)

    def testG(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'g'), 114)

    def testH(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'h'), 65412)

    def testI(self):
        self.assertEqual(get_wire_value(TEST_INPUT, 'i'), 65079)

class TestParts(unittest.TestCase):
    def testPartOne(self):
        self.assertEqual(get_wire_value(INPUT, 'a'), 3176)
    def testPartTwo(self):
        self.assertEqual(part_two(INPUT), 14710)

unittest.main()
