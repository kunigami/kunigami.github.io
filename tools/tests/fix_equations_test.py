# Run with
# python3 -m unittest fix_equations_test.py
import os
import sys
import unittest

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, parent_dir)

# Import the file from the parent directory
from fix_equations import fix_equations


class FixEquationsTest(unittest.TestCase):

    def test_no_op(self):
        text = """
f(1)
f(A)
"""
        self.assertEqual(fix_equations(text), text)

    def test_eq(self):
        text = """
$$
(A) 1 + 1
$$

$$
(C) 2 + 2
$$

$$
(B) 3 + 3
$$

Hello (A). f(A)? (B), (C) and (D).
"""
        expected = """
$$
(1) 1 + 1
$$

$$
(2) 2 + 2
$$

$$
(3) 3 + 3
$$

Hello (1). f(A)? (3), (2) and (D).
"""
        self.assertEqual(fix_equations(text), expected)

    def test_sub_eq(self):
        text = """
$$
(1.A) 1 + 1
$$

$$
(1.B) 2 + 2
$$

$$
(2.C) 2 + 2
$$

Hello (1.A), (1.B), (2.C).
"""
        expected = """
$$
(1.1) 1 + 1
$$

$$
(1.2) 2 + 2
$$

$$
(2.1) 2 + 2
$$

Hello (1.1), (1.2), (2.1).
"""
        self.assertEqual(fix_equations(text), expected)

    def test_stmt(self):
        text = """
**Theorem A.** Let 

$$
(A.A) 1 + 1
$$

By *Lemma B*:

**Lemma B.** Let 

$$
(B.B) 2 + 2
$$

**Corollary C.** Pet *Lemma B* and *Theorem A*.
"""

        expected = """
**Theorem 1.** Let 

$$
(1.1) 1 + 1
$$

By *Lemma 2*:

**Lemma 2.** Let 

$$
(2.1) 2 + 2
$$

**Corollary 3.** Pet *Lemma 2* and *Theorem 1*.
"""

        self.assertEqual(fix_equations(text), expected)
