
import sys
sys.setrecursionlimit(8000)

def delta_floor(c):
    return 1 if c == "(" else -1
    

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
print find_floor(open("day1_input.txt").read().strip())

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
print find_floor2(open("day1_input.txt").read().strip())
