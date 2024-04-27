'''
This script is used to fix / set the proper equation numbering of my posts.
Sometimes I need to introduce a new equation in the middle of the post, and
then I need to update the equation numbering for subsequent equations manually.

This script automates that, assuming some specific pattern.
'''
import re
import sys
from collections import defaultdict

# Equation numbers
equation_id = r"^\(([0-9]+)?.?([A-z0-9]+)\)"

def find_equations_id_in_order(code):
    eq_ids_by_ns = defaultdict(list)

    in_eq = False
    eq_start = 0
    for i in range(len(code)):
        if code[i:i+2] == '$$':
            in_eq = not in_eq
            if in_eq:
                eq_start = i
            else:
                eq = code[eq_start+2:i].strip()
                matches = re.findall(equation_id, eq)
                if matches:
                    (ns, num) = matches[0]
                    eq_ids_by_ns[ns].append(num)
    return eq_ids_by_ns


def make_id(ns, num):
    if ns == "":
        return f"{num}"
    else:
        return f"{ns}.{num}"


def make_tmp_id(ns, num):
    return f"{make_id(ns, num)}__placeholder__"

def main():
    # Check if at least one argument was provided
    if len(sys.argv) < 2:
        raise Exception("No argument provided")

    filename = sys.argv[1]
    print(filename)

    code = ""
    with open(filename, "r") as f:
        code = f.read()

    eq_ids_by_ns = find_equations_id_in_order(code)

    # replace by an intermediate to avoid conflicts
    for ns in eq_ids_by_ns:
        eq_ids = eq_ids_by_ns[ns]
        for i in range(len(eq_ids)):
            eq_id = make_id(ns, eq_ids[i])
            temp_id = make_tmp_id(ns, i + 1)
            print(f"replacing ({eq_id}) with ({temp_id})")
            code = code.replace(f"({eq_id})", f"({temp_id})")

        # remove placeholders
        for i in range(len(eq_ids)):
            code = code.replace("__placeholder__", "")

    # write back
    with open(filename, "w") as f:
        f.write(code)


main()
