"""
This script is used to fix / set the proper equation numbering of my posts.
Sometimes I need to introduce a new equation in the middle of the post, and
then I need to update the equation numbering for subsequent equations manually.

This script automates that, assuming some specific pattern.
"""

import re
import sys
from collections import defaultdict
from typing import Dict, List, Tuple

# Equation numbers e.g. (A) or (1.A)
EQUATION_ID_REGEX = r"^\(([0-9]+)?.?([A-z0-9]+)\)"

STATEMENT_KEYWORDS = {"Lemma", "Theorem", "Corollary"}

ID_REGEX = r"^[A-Z0-9]+"


def find_equations_id_in_order(code):
    eq_ids_by_ns = defaultdict(list)

    in_eq = False
    eq_start = 0
    for i in range(len(code)):
        if code[i : i + 2] == "$$":
            in_eq = not in_eq
            if in_eq:
                eq_start = i
            else:  # closing eq
                eq = code[eq_start + 2 : i].strip()
                matches = re.findall(EQUATION_ID_REGEX, eq)
                if matches:
                    (ns, num) = matches[0]
                    eq_ids_by_ns[ns].append(num)
    return eq_ids_by_ns


def is_uppercase_letter(c):
    return c.isalpha() and c.isupper()


def is_number(c):
    return c.isdigit()


def find_statements_id_in_order(code) -> Dict[Tuple[str, str], str]:
    stmts_ids_by_ns = {}

    i = 0
    while i < len(code):
        found = False
        for stmt in STATEMENT_KEYWORDS:
            stmt_regex = f"\\*\\*{stmt} ([A-Z0-9])(\\.?)\\*\\*"
            # magic number 10: big enough to enclose the statement
            chunk = code[i : i + len(stmt) + 10]
            matches = re.search(stmt_regex, chunk)
            if matches:
                needle = matches.group(0)
                id = matches.group(1)
                if id in stmts_ids_by_ns:
                    raise Exception(f'{chunk} failed: id {id} already in stmts_ids_by_ns: {stmts_ids_by_ns[id]}')
                stmts_ids_by_ns[(id,stmt)] = stmt
                i += len(needle)
                found = True
                break
        if found:
            continue

        i += 1
    return stmts_ids_by_ns


def make_id(ns, num):
    if ns == "":
        return f"{num}"
    else:
        return f"{ns}.{num}"


def make_tmp_id(ns, num):
    return f"{make_id(ns, num)}__placeholder__"


def fix_equations(code):

    stmts_by_id = find_statements_id_in_order(code)
    print(stmts_by_id)

    for cnt, id in enumerate(stmts_by_id):
        stmt_id, _ = id
        stmt = stmts_by_id[id]
        # Replace Theorem A with Theorem__placeholder__ 1
        old = f"{stmt} {stmt_id}"
        new = f"{stmt}__placeholder__ {cnt + 1}"
        code = code.replace(old, new)

        # Replace (A.1) with (1__placeholder__.1)
        old_eq = f"({stmt_id}."
        new_eq = f"({cnt + 1}__placeholder__."
        code = code.replace(old_eq, new_eq)

    code = code.replace("__placeholder__", "")

    eq_ids_by_ns = find_equations_id_in_order(code)
    for ns in eq_ids_by_ns:
        eq_ids = eq_ids_by_ns[ns]
        for i in range(len(eq_ids)):
            eq_id = make_id(ns, eq_ids[i])
            temp_id = make_tmp_id(ns, i + 1)
            # replace by an intermediate to avoid conflicts
            regex = f"([^A-z]+)\\({eq_id}\\)([^A-z]+)"
            code = re.sub(regex, f"\\1({temp_id})\\2", code)

    # remove placeholders
    code = code.replace("__placeholder__", "")
    return code


if __name__ == "__main__":
    # Check if at least one argument was provided
    if len(sys.argv) < 2:
        raise Exception("No argument provided")

    filename = sys.argv[1]
    print(filename)

    code = ""
    with open(filename, "r") as f:
        code = f.read()
    
    if code == "":
        print("No code found. Exiting...")
        exit(0)

    new_code = fix_equations(code)

    if new_code == code:
        print("No changes made.")
        exit(0)

    # write back
    print("Writing back to ", filename)
    with open(filename, "w") as f:
        f.write(new_code)
