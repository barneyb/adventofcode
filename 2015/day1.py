
import sys
sys.setrecursionlimit(8000)

def find_floor(input):
    return (1 if input[0] == "(" else -1) + (find_floor(input[1:]) if len(input) > 1 else 0)

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
