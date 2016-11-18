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
    3: lambda ps, ws: the_wire(ws, ps[0]),
    4: lambda ps, ws: UnaryGate(ops[ps[0]], the_wire(ws, ps[1])),
    5: lambda ps, ws: BinaryGate(
        ops[ps[1]],
        the_wire(ws, ps[0]),
        the_wire(ws, ps[2]))
}

def parse_wire(parts, wires):
    it = parsers[len(parts)](parts, wires)
    wire = the_wire(wires, parts[len(parts) - 1])
    wire.source = it
    return wire

def the_wire(wires, name):
    try:
        return Signal(int(name))
    except ValueError:
        if not wires.has_key(name):
            wires[name] = Wire(name)
        return wires[name]

def _dict_plus_key(d, pair):
    # this "should" duplicate and modify
    d[pair[0]] = pair[1]
    return d

def name_wire(w):
    return w.name, w

def wire_circuit(booklet):
    return reduce(
        lambda a, l: _dict_plus_key(a, name_wire(parse_wire(l, a))),
        map(
            lambda l: l.split(" "),
            booklet.strip().split("\n")),
        {})

def get_wire_value(booklet, name):
    return wire_circuit(booklet)[name].output()

def part_two(booklet):
    return get_wire_value(booklet + "\n" + str(get_wire_value(booklet, 'a')) + " -> b", 'a')

class Item:
    
    def __init__(self):
        self.output_value = None

    def output(self):
        if self.output_value is None:
            self.output_value = self.output_internal()
        return self.output_value

class Wire(Item):

    def __init__(self, name, source=None):
        Item.__init__(self)
        self.name = name
        self.source = source

    def output_internal(self):
        return self.source.output()

class Signal(Item):

    def __init__(self, signal):
        Item.__init__(self)
        self.signal = signal

    def output_internal(self):
        return self.signal

class UnaryGate(Item):

    def __init__(self, op, source):
        Item.__init__(self)
        self.op = op
        self.source = source

    def output_internal(self):
        return self.op(self.source.output())

class BinaryGate(Item):

    def __init__(self, op, left, right):
        Item.__init__(self)
        self.op = op
        self.left = left
        self.right = right

    def output_internal(self):
        return self.op(self.left.output(), self.right.output())

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
