import sys
sys.setrecursionlimit(8000)

INPUT = open("day01_input.txt").read().strip()

# char -> int
def delta_floor(c):
    return 1 if c == "(" else -1

# str -> int
def find_floor(input):
    return delta_floor(input[0]) + (find_floor(input[1:]) if len(input) > 1 else 0)

assert find_floor("(())") == 0
assert find_floor("()()") == 0
assert find_floor("(((") == 3
assert find_floor("(()(()(") == 3
assert find_floor("))(((((") == 3
assert find_floor("())") == -1
assert find_floor("))(") == -1
assert find_floor(")))") == -3
assert find_floor(")())())") == -3
print find_floor(INPUT)

def find_floor2(input):
    return reduce(lambda a, b: a + b,
                  map(delta_floor, list(input)))

assert find_floor2("(())") == 0
assert find_floor2("()()") == 0
assert find_floor2("(((") == 3
assert find_floor2("(()(()(") == 3
assert find_floor2("))(((((") == 3
assert find_floor2("())") == -1
assert find_floor2("))(") == -1
assert find_floor2(")))") == -3
assert find_floor2(")())())") == -3
print find_floor2(INPUT)

def pos_basement(input):
    return reduce(lambda a, d: {'hit': True, 'pos': a['pos'], 'floor': a['floor'] + d} if (a['hit'] or (a['floor'] == 0 and d == -1)) else {'hit': a['hit'], 'pos': a['pos'] + 1, 'floor': a['floor'] + d},
                  map(delta_floor, list(input)),
                  {'hit': False, 'pos': 1, 'floor': 0}
    )['pos']

assert pos_basement(")") == 1
assert pos_basement(")))((") == 1
assert pos_basement("()())") == 5
assert pos_basement("()())(())") == 5
print pos_basement(INPUT)
