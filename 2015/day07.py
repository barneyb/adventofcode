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
    'NOT': lambda n: 65536 + ~n,
    'AND': op.and_,
    'OR': op.or_,
    'LSHIFT': op.lshift,
    'RSHIFT': op.rshift
}
parsers = {
    1: lambda ps: wire_or_signal(ps[0]),
    2: lambda ps: UnaryGate(ops[ps[0]], wire_or_signal(ps[1])),
    3: lambda ps: BinaryGate(
        ops[ps[1]],
        wire_or_signal(ps[0]),
        wire_or_signal(ps[2]))
}

def wire_or_signal(name):
    try:
        return Signal(int(name))
    except ValueError:
        return WireRef(name)

def _dict_plus_key(d, kvpair):
    # this is my "immutably extend a dict with a new key" operator
    d[kvpair[0]] = kvpair[1]
    return d

def parse_src(src):
    return parsers[len(src)](src)

def parse_wire(parts):
    return parts[len(parts) - 1], parse_src(parts[:len(parts) - 2])

def wire_circuit(booklet):
    return reduce(
        lambda a, l: _dict_plus_key(a, parse_wire(l)),
        map(
            lambda l: l.split(" "),
            booklet.strip().split("\n")),
        {})

def eval_src(circuit, source):
    return (source if isinstance(source, int)
            else source.signal if isinstance(source, Signal)
            else eval_src(circuit, circuit[source]) if isinstance(source, str)
            else source.eval(circuit))

def eval_circuit(circuit):
    return reduce(
        lambda a, p: _dict_plus_key(a, (p[0], eval_src(a, p[1]))),
        circuit.items(),
        circuit)

def get_wire_value(booklet, name):
    return eval_circuit(wire_circuit(booklet))[name]

def part_two(booklet):
    return get_wire_value(booklet + "\n" + str(get_wire_value(booklet, 'a')) + " -> b", 'a')

class Wire:

    def __init__(self, name, source=None):
        self.name = name
        self.source = source

    def eval(self, circuit):
        return eval_src(circuit, self.source)

    def __repr__(self):
        return repr(self.source) + " -> " + name

class WireRef:

    def __init__(self, ref):
        self.ref = ref

    def eval(self, circuit):
        return eval_src(circuit, self.ref)

    def __repr__(self):
        return self.ref

class Signal:

    def __init__(self, signal):
        self.signal = signal

    def eval(self, circuit):
        return self.signal

    def __repr__(self):
        return "s(" + repr(self.signal) + ")"

class UnaryGate:

    def __init__(self, op, source):
        self.op = op
        self.source = source

    def eval(self, circuit):
        return self.op(eval_src(circuit, self.source))

    def __repr__(self):
        return repr(self.op) + " " + repr(self.source)

class BinaryGate:

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def eval(self, circuit):
        return self.op(
            eval_src(circuit, self.left),
            eval_src(circuit, self.right))

    def __repr__(self):
        return repr(self.left) + " " + repr(self.op) + " " + repr(self.right)

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

#class TestParts(unittest.TestCase):
#    def testPartOne(self):
#        self.assertEqual(get_wire_value(INPUT, 'a'), 3176)
#    def testPartTwo(self):
#        self.assertEqual(part_two(INPUT), 14710)

unittest.main()
