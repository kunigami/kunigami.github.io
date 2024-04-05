'''
This script is used to fix / set the proper equation numbering of my posts.
Sometimes I need to introduce a new equation in the middle of the post, and
then I need to update the equation numbering for subsequent equations manually.

This script automates that, assuming some specific pattern.
'''
import re
import sys

equation_id = r"^\(([A-z0-9]+)\)"  # Regular expression to match 4-letter words

def find_equations_id_in_order(code):
    eq_ids = []

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
                    print(eq)
                    print(matches)
                    eq_ids.append(matches[0])
    return eq_ids



def main():
    # Check if at least one argument was provided
    if len(sys.argv) < 2:
        raise Exception("No argument provided")

    filename = sys.argv[1]
    print(filename)

    code = ""
    with open(filename, "r") as f:
        code = f.read()

    eq_ids = find_equations_id_in_order(code)

    # replace by an intermediate to avoid conflicts
    for i in range(len(eq_ids)):
        eq_id = eq_ids[i]
        print(f"replacing ({eq_id}) with ({i+1}__placeholder__)")
        code = code.replace(f"({eq_id})", f"({i+1}__placeholder__)")

    # remove placeholders
    for i in range(len(eq_ids)):
        code = code.replace(f"({i+1}__placeholder__)", f"({i + 1})")

    # write back
    with open(filename, "w") as f:
        f.write(code)


main()
