from abc import ABC, abstractmethod
from enum import Enum
from queue import Queue

from typing import Dict, Generic, List, Optional, Set, Tuple, TypeVar

TElement = TypeVar('TElement')

class IndependentSet(Generic[TElement]):
    """
    Models an independent set of a matroid M(A, I).
    We assume TElement is hashable.
    """

    @abstractmethod
    def get_circuit(self, element: TElement) -> Optional[Set[TElement]]:
        """
        Determine if adding e forms a circuit in this matroid.
        If not, return None.
        Otherwise return a set with elements from the circuit.
        """

    @abstractmethod
    def add(self, element: TElement) -> None:
        """
        Add an element to this independent set.
        """

    @abstractmethod
    def remove(self, element: TElement) -> None:
        """
        Remove an element from this independent set.
        """

    @abstractmethod
    def get_elements(self) -> Set[TElement]:
        """
        Return all the elements from this independent set.
        """


    @abstractmethod
    def __len__(self) -> int:
        """
        Number of elements in this matroid.
        """

    @abstractmethod
    def __contains__(self, element: TElement) -> bool:
        """
        Whether element belongs to this matroid.
        """

TEdge = Tuple[int, int]
class Side(Enum):
  LEFT = 'left'
  RIGHT = 'right'


class BipartiteIndependentSet(IndependentSet[TEdge]):
    def __init__(self, side: Side):
        self._incidence: Dict[int, TEdge] = {}
        self._side = side

    def get_vertex(self, edge: TEdge):
        (l, r) = edge
        return l if self._side == Side.LEFT else r

    def add(self, element: TEdge) -> None:
        v = self.get_vertex(element)
        assert v not in self._incidence
        self._incidence[v] = element

    def get_circuit(self, element: TEdge) -> Optional[Set[TEdge]]:
        v = self.get_vertex(element)
        if v in self._incidence:
            return set([self._incidence[v], element])
        return None

    def get_elements(self) -> Set[TEdge]:
        return set(self._incidence.values())

    def remove(self, element: TEdge) -> None:
        v = self.get_vertex(element)
        del self._incidence[v]

    def __len__(self) -> int:
        return len(self._incidence)

    def __contains__(self, element: TEdge) -> bool:
        v = self.get_vertex(element)
        return v in self._incidence and element == self._incidence[v]


def find_augmenting_seq(
    elements: List[TElement],
    s1: IndependentSet[TElement],
    s2: IndependentSet[TElement],
):
    parents: Dict[TElement, TElement] = {}
    visited: Set[TElement] = set()
    candidates: Queue = Queue()
    left_adj: Dict[TElement, Set[TElement]] = {}
    right_adj: Dict[TElement, Set[TElement]] = {}

    def visit_neighbors(u, neighbors):
        for v in neighbors:
            if v not in visited:
                candidates.put(v)
                parents[v] = u

    def get_sequence(u):
        # backtrack to find sequence
        seq = []
        while u is not None:
            seq.append(u)
            u = parents.get(u, None)

        assert len(seq) % 2 == 1, f"Should have an odd size. Got {seq}"

        return seq

    def visit_element(u):
        visited.add(u)

        if u in s1: # right partition
            if u in left_adj:
                visit_neighbors(u, left_adj[u])

        else: # left partition
            if u not in right_adj: # found solution!
                return True
            else:
                visit_neighbors(u, right_adj[u])

        return False

    for e in elements:
        if e in s1:
            continue

        circuit = s1.get_circuit(e)
        # e can be added to s1
        if circuit is None:
            candidates.put(e)
        else: # can't be added
            for re in circuit:
                if re not in left_adj:
                    left_adj[re] = set()
                left_adj[re].add(e)

        circuit = s2.get_circuit(e)
        if circuit:
            right_adj[e] = circuit

    while not candidates.empty():
        u = candidates.get()
        found_solution = visit_element(u)
        if found_solution:
            return get_sequence(u)


def update_solution(
    s1: IndependentSet[TElement],
    s2: IndependentSet[TElement],
    seq: List[TElement]
):
    # remove odd positions first so
    # we keep s1 and s2 feasible
    for i in range(1, len(seq), 2):
        s1.remove(seq[i])
        s2.remove(seq[i])
    for i in range(0, len(seq), 2):
        s1.add(seq[i])
        s2.add(seq[i])

def intersect(
    elements: List[TElement],
    s1: IndependentSet[TElement],
    s2: IndependentSet[TElement]
):
    while True:
        initial_cardinality = len(s1)
        seq = find_augmenting_seq(elements, s1, s2)
        if seq is None:
            return s1
        update_solution(s1, s2, seq)
        assert len(s1) == initial_cardinality + 1

def bipartite_matching(edges):
    left = BipartiteIndependentSet(side=Side.LEFT)
    right = BipartiteIndependentSet(side=Side.RIGHT)
    result = intersect(edges, left, right)
    return result.get_elements()
