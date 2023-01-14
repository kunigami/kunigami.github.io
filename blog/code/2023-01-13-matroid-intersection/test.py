import unittest

from intersection import bipartite_matching

from unittest.mock import patch

class BipartiteMatchingTest(unittest.TestCase):

    # Assert this is a valid bipartite matching
    def assert_feasible_solution(self, input, output):
        edges_set = set(input)
        left = set()
        right = set()
        for edge in output:
            self.assertTrue(edge in edges_set)
            [l, r] = edge
            self.assertFalse(l in left)
            self.assertFalse(r in right)
            left.add(l)
            right.add(r)

    def test_empty_graph(self):
        edges = []
        actual = bipartite_matching(edges)
        self.assert_feasible_solution(edges, actual)
        self.assertEqual(len(actual), 0)

    def test_imperfect_match(self):
        edges = [(0, 2), (1, 2)]
        actual = bipartite_matching(edges)
        self.assert_feasible_solution(edges, actual)
        self.assertEqual(len(actual), 1)

    def test_complete_graph(self):
        edges = [(0, 2), (0, 3), (1, 2), (1, 3)]
        actual = bipartite_matching(edges)
        self.assert_feasible_solution(edges, actual)
        self.assertEqual(len(actual), 2)

    def test_switching_1(self):
        # (0, 2) is matched first, but needs rematching
        # to (0, 3) so (1, 2) is picked
        edges = [(0, 2), (0, 3), (1, 2)]
        actual = bipartite_matching(edges)
        self.assert_feasible_solution(edges, actual)
        print(actual)
        self.assertEqual(len(actual), 2)

    def test_switching_2(self):
        edges = [(0, 3), (0, 4), (0, 5), (1, 3), (1, 4), (1, 5), (2, 3)]
        actual = bipartite_matching(edges)
        self.assert_feasible_solution(edges, actual)
        self.assertEqual(len(actual), 3)

if __name__ == '__main__':
    unittest.main()
