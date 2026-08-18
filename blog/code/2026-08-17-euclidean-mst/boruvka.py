import sys
from dataclasses import dataclass, field
from math import inf, sqrt
from collections import defaultdict
import numpy as np
from scipy.sparse.csgraph import minimum_spanning_tree
import time
import random
from tabulate import tabulate
from abc import ABC, abstractmethod

@dataclass(frozen=True)
class Dimension:
    n: int
    v: int = 0

    def next(self):
        next_v = (self.v + 1) % self.n
        return Dimension(self.n, next_v)

def norm_sq(p) -> foat:
    return sum(x*x for x in p)

@dataclass(frozen=True)
class Point:
    data: tuple[int, ...]

    def __getitem__(self, dim: Dimension):
        return self.data[dim.v]

    def __abs__(self) -> float:
        return sqrt(norm_sq(self))

    def __sub__(self, other: Point):
        sub = tuple(a-b for a,b in zip(self.data, other.data))
        return Point(sub)

    def __iter__(self):
          return iter(self.data)

@dataclass
class View:
    pts: list[Point]
    lo: int = 0
    hi: int | None = None

    def __post_init__(self):
        if self.hi is None:
            object.__setattr__(self, 'hi', len(self.pts) - 1)

    def range(self, lo, hi):
        return View(self.pts, lo, hi)

    def swap(self, i, j):
        self.pts[i], self.pts[j] = self.pts[j], self.pts[i]

    def __getitem__(self, i: int):
        return self.pts[i]

@dataclass
class BoundingBox:
    lo: Point
    hi: Point

    @staticmethod
    def for_point(p):
        return BoundingBox(p, p)

    def union(self, bb):
        self.lo =  Point(
            tuple(min(px, qx) for (px, qx) in zip(self.lo, bb.lo))
        )
        self.hi =  Point(
            tuple(max(px, qx) for (px, qx) in zip(self.hi, bb.hi))
        )

    def size(self):
        'Square length of the diagonal'
        return norm_sq(self.lo - self.hi)


class UnionFind:
    def __init__(self, n):
        self.p = list(range(n))
        self.s = [1]*n
        self.sz = n

    def find(self, x):
        p = self.p
        if p[x] != x:
            p[x] = self.find(p[x])
        return p[x]

    def union(self, x, y):
        px, py = self.find(x), self.find(y)
        if px == py:
            return

        s, p = self.s, self.p
        if s[px] < s[py]:
            px, py = py, px

        s[px] += s[py]
        p[py] = p[px]

        self.sz -= 1
        return s[px]

    def size(self):
        return self.sz


def partition(pts: View, p: int, dim: Dimension) -> int:
    """
    Lomuto partition. Rearrange pts[lo:hi] so that
    if i < p, then pts[i][dim] <= pts[p][dim]
    if i > p, then pts[i][dim] > pts[p][dim]
    """
    lo, hi = pts.lo, pts.hi
    pivot = pts[p]
    pts.swap(p, hi)
    store = lo

    for i in range(lo, hi):
        if pts[i][dim] <= pivot[dim]:
            pts.swap(store, i)
            store += 1

    pts.swap(store, hi)
    return store

def median_index(pts: View, dim: Dimension) -> int:
    lo, hi = pts.lo, pts.hi
    k = lo + (hi - lo) // 2
    while True:
        if lo == hi:
            return lo
        p = partition(pts.range(lo, hi), random.randint(lo, hi), dim)
        if p == k:
            return p
        elif p > k:
            hi = p - 1
        else:
            lo = p + 1



@dataclass
class KDNode:
    pivot: int
    left: KDNode | None
    right: KDNode | None
    bb: BoundingBox
    components: set[int] = field(default_factory=set)
    worst_component: int | None = None
    worst_distance: float = inf

    def is_leaf(self):
        return self.left is None and self.right is None


def build_kd_tree(pts, dim) -> KDNode:
    if pts.lo > pts.hi:
        return None

    mi = median_index(pts, dim)

    left = build_kd_tree(pts.range(pts.lo, mi - 1), dim.next())
    right = build_kd_tree(pts.range(mi + 1, pts.hi), dim.next())
    bb = BoundingBox.for_point(pts[mi])
    if left:
        bb.union(left.bb)
    if right:
        bb.union(right.bb)
    return KDNode(pivot=mi, left=left, right=right, bb=bb)

def component_best(best, component):
      edge = best.get(component)
      return inf if edge is None else edge[2]

def node_upper_bound(node, best):
    if len(node.components) == 1:
        component = next(iter(node.components))
        return component_best(best, component)

    if node.worst_component is not None:
        curr_v = component_best(best, node.worst_component)
        if curr_v == node.worst_distance: # no improvements
            return curr_v

    worst_component = None
    max_v = -1
    # improvement happened, need re-scan
    for component in node.components:
        curr_v = component_best(best, component)
        if curr_v > max_v:
            max_v = curr_v
            worst_component = component

    node.worst_component = worst_component
    node.worst_distance = max_v
    return max_v


def point_box_distance(p, bb):
    "Minimum distance between a point p and a bounding box bb"
    d = 0.0
    for px, lo, hi in zip(p, bb.lo, bb.hi):
        x = min(max(px, lo), hi)
        d += (px - x) ** 2
    return sqrt(d)

def box_box_distance_sq(bb_a, bb_b):
    "Minimum distance two bounding boxes (0 if they overlap)"
    d = 0.0
    for lo_a, hi_a, lo_b, hi_b in zip(bb_a.lo, bb_a.hi, bb_b.lo, bb_b.hi):
        gap = max(lo_a - hi_b, lo_b - hi_a, 0.0)
        d += gap * gap
    return d


def read_input():
    points = []
    for line in sys.stdin.buffer:
        if not line.strip():
            continue
        x, y, z = [float(v) for v in line.split(b",")]
        points.append(Point((x, y, z)))
    return points

def dist(p: Point, q: Point):
    if p is None or q is None:
        return inf

    return abs(p - q)

type Edge = tuple[int, int, float]

class EMST(ABC):
    @abstractmethod
    def desc(self) -> str:
        pass

    @abstractmethod
    def get_mst_edges(self, points: list[Point]) -> list[Edge]:
        pass

class Boruvka(EMST):
    def setup(self, points: list[Point]):
        pass

    @abstractmethod
    def fill_best_edge_by_component():
        pass

    def get_mst_edges(self, points: list[Point]) -> list[Edge]:
        n = len(points)
        uf = UnionFind(n)
        self._uf = uf

        self.setup(points)

        sol = []
        while uf.size() > 1:
            self._best_edges = {}
            self.fill_best_edge_by_component()
            for e in self._best_edges.values():
                uc, vc, _ = e
                if self.merge_components(uc, vc):
                    sol.append(e)
        return sol

    # Helper methods

    def merge_components(self, p, q) -> bool:
        'Combine components of p and q into one'
        if self._uf.find(p) == self._uf.find(q):
            return False
        self._uf.union(p, q)
        return True

    def get_component(self, p):
        return self._uf.find(p)

    def get_best_dist(self, p):
        pc = self.get_component(p)
        if pc not in self._best_edges:
            return inf
        return self._best_edges[pc][2]

    def process_edge_for_component(self, c, e):
        _, _, w = e
        if self.get_best_dist(c) > w:
            self._best_edges[c] = e

    def process_edge(self, e):
        u, v, w = e
        uc, vc = self.get_component(u), self.get_component(v)
        if uc == vc:
            return

        self.process_edge_for_component(uc, e)
        self.process_edge_for_component(vc, e)


class NaiveBoruvka(Boruvka):
    def desc(self) -> str:
        return "O(N^2) Boruvka"

    def setup(self, points: list[Point]):
        n = len(points)
        edges = []
        for i in range(n):
            for j in range(i + 1, n):
                edges.append((i, j, dist(points[i], points[j])))
        self._edges = edges


    def fill_best_edge_by_component(self):
        for e in self._edges:
            self.process_edge(e)

class KDTreeBoruvkaBased(Boruvka):
    def setup(self, points: list[Point]):
        dim = Dimension(n=3)
        view = View(points)
        self._kd = build_kd_tree(view, dim)
        self._points = points

    def update_kd_tree(self, node):
        "update components of each node"
        uf = self._uf

        if node is None:
            return

        c = uf.find(node.pivot)
        node.components = {c}
        node.worst_component = None
        node.worst_distance = inf

        if node.is_leaf():
            return

        l, r = node.left, node.right
        self.update_kd_tree(l)
        self.update_kd_tree(r)
        if l:
            node.components = node.components | l.components
        if r:
            node.components = node.components | r.components

    def get_point(self, p):
        return self._points[p]


class KDTreeBoruvka(KDTreeBoruvkaBased):
    def desc(self) -> str:
        return "KD-Tree Boruvka"

    def query_kd_tree(self, node, q, dim):
        if not node:
            return

        # prune: same component
        if node.components == {self.get_component(q)}:
            return

        p = node.pivot
        Q, P = self.get_point(q), self.get_point(p)

        # pivot is a candidate
        self.process_edge((q, p, dist(Q, P)))

        if node.is_leaf():
            return

        if Q[dim] <= P[dim]:
            main = node.left
            other = node.right
        else:
            main = node.right
            other = node.left

        # main branch
        self.query_kd_tree(main, q, dim.next())

        if other:

            lb = point_box_distance(Q, other.bb)

            # minimum distance found so far
            ub = self.get_best_dist(q)

            # needs to search in the other tree
            if lb < ub:
                self.query_kd_tree(other, q, dim.next())


    def fill_best_edge_by_component(self):
        kd = self._kd
        points = self._points
        dim = Dimension(n=3)

        self.update_kd_tree(kd)

        for i in range(len(points)):
            self.query_kd_tree(kd, i, dim)

class KDTreeDualBoruvka(KDTreeBoruvkaBased):
    def desc(self) -> str:
        return "KD-Tree Boruvka Dual"

    def parts(self, node):
        p = node.pivot
        yield KDNode(
            pivot=p,
            left=None,
            right=None,
            bb=BoundingBox.for_point(self.get_point(p)),
            components={self.get_component(p)},
        )

        if node.left is not None:
            yield node.left

        if node.right is not None:
            yield node.right

    def query_kd_tree_dual_same(self, node):
        children = list(self.parts(node))

        for child in children:
            if not child.is_leaf():
                self.query_kd_tree_dual_same(child)

        for i, a in enumerate(children):
            for b in children[i+1:]:
                self.query_kd_tree_dual(a, b)

    def query_kd_tree_dual(self, node_a, node_b):
        # all points in the subtress belong to the same component
        if len(node_a.components) == 1 and node_a.components == node_b.components:
            return

        ub = max(
            node_upper_bound(node_a, self._best_edges),
            node_upper_bound(node_b, self._best_edges),
        )
        lb = box_box_distance_sq(node_a.bb, node_b.bb)
        if lb >= ub**2:
            return

        if node_a.is_leaf() and node_b.is_leaf():
            p, q = node_a.pivot, node_b.pivot
            w = dist(self.get_point(q), self.get_point(p))
            self.process_edge((p, q, w))
            return

        if node_a.bb.size() >= node_b.bb.size():
            for child in self.parts(node_a):
                self.query_kd_tree_dual(child, node_b)
        else:
            for child in self.parts(node_b):
                self.query_kd_tree_dual(node_a, child)

    def fill_best_edge_by_component(self):
        self.update_kd_tree(self._kd)
        self.query_kd_tree_dual_same(self._kd)


class SciPyMST(EMST):
    def desc(self):
        return "SciPy's MST [benchmark]"

    def get_mst_edges(self, points: list[Point]) -> list[Edge]:
        n = len(points)
        adj = [[0]*n for _ in range(n)]
        for i in range(n):
            for j in range(n):
                adj[i][j] = dist(points[i], points[j])
        graph = np.array(adj)
        mst = minimum_spanning_tree(graph)
        coo = mst.tocoo()
        edges = list(zip(
            coo.row.tolist(),
            coo.col.tolist(),
            coo.data.tolist(),
        ))
        return edges



class _DualKDTreeOpt:
    """Flat KD-tree representation used by boruvka_emst_dual_opt."""

    __slots__ = (
        "coords",
        "dimension",
        "point",
        "left",
        "right",
        "box_min",
        "box_max",
        "box_size",
        "subtree_size",
        "postorder",
        "component_mask",
    )

    def __init__(self, points):
        self.coords = [p.data for p in points]
        self.dimension = len(self.coords[0]) if self.coords else 0
        self.point = []
        self.left = []
        self.right = []
        self.box_min = []
        self.box_max = []
        self.box_size = []
        self.subtree_size = []
        self.postorder = []
        self.component_mask = []

        def build(indices, depth):
            if not indices:
                return -1

            axis = depth % self.dimension
            indices.sort(key=lambda i: self.coords[i][axis])
            middle = len(indices) // 2
            pivot = indices[middle]

            node = len(self.point)
            self.point.append(pivot)
            self.left.append(-1)
            self.right.append(-1)
            self.box_min.append(None)
            self.box_max.append(None)
            self.box_size.append(0.0)
            self.subtree_size.append(len(indices))
            self.component_mask.append(0)

            left = build(indices[:middle], depth + 1)
            right = build(indices[middle + 1:], depth + 1)
            self.left[node] = left
            self.right[node] = right

            minimum = list(self.coords[pivot])
            maximum = list(self.coords[pivot])
            for child in (left, right):
                if child < 0:
                    continue
                child_min = self.box_min[child]
                child_max = self.box_max[child]
                for d in range(self.dimension):
                    minimum[d] = min(minimum[d], child_min[d])
                    maximum[d] = max(maximum[d], child_max[d])

            self.box_min[node] = tuple(minimum)
            self.box_max[node] = tuple(maximum)
            self.box_size[node] = sum(
                (maximum[d] - minimum[d]) ** 2
                for d in range(self.dimension)
            )
            self.postorder.append(node)
            return node

        build(list(range(len(points))), 0)


def _dual_opt_find_best(tree, uf):
    """Find one minimum outgoing edge for every current component."""

    n = len(tree.coords)
    component = [uf.find(i) for i in range(n)]
    point_mask = [1 << c for c in component]

    # A Python integer is used as a compact set of component IDs. Bitwise OR
    # is considerably cheaper here than allocating sets for every KD node.
    component_mask = tree.component_mask
    for node in tree.postorder:
        mask = point_mask[tree.point[node]]
        left = tree.left[node]
        right = tree.right[node]
        if left >= 0:
            mask |= component_mask[left]
        if right >= 0:
            mask |= component_mask[right]
        component_mask[node] = mask

    best_sq = [inf] * n
    best_u = [-1] * n
    best_v = [-1] * n
    bound_value = [inf] * len(tree.point)
    bound_component = [-1] * len(tree.point)

    coords = tree.coords
    dimension = tree.dimension
    point = tree.point
    left = tree.left
    right = tree.right
    box_min = tree.box_min
    box_max = tree.box_max
    box_size = tree.box_size
    subtree_size = tree.subtree_size

    # Negative parts represent a pivot as a singleton bounding box. Encoding
    # point p as ~p avoids allocating a temporary KDNode for every expansion.
    def part_mask(part):
        if part < 0:
            return point_mask[~part]
        return component_mask[part]

    def is_atomic(part):
        return part < 0 or (left[part] < 0 and right[part] < 0)

    def part_point(part):
        return ~part if part < 0 else point[part]

    def part_upper_bound(part):
        if part < 0:
            return best_sq[component[~part]]

        # best_sq values only decrease. The cached maximum stays exact until
        # its witness component improves; only then do we rescan this node's
        # component mask to find the new maximum.
        witness = bound_component[part]
        if witness >= 0 and best_sq[witness] == bound_value[part]:
            return bound_value[part]

        candidates = component_mask[part]
        maximum = -1.0
        witness = -1
        while candidates:
            bit = candidates & -candidates
            c = bit.bit_length() - 1
            value = best_sq[c]
            if value > maximum:
                maximum = value
                witness = c
            candidates ^= bit

        bound_value[part] = maximum
        bound_component[part] = witness
        return maximum

    def box_distance_sq(part_a, part_b):
        if part_a < 0:
            min_a = max_a = coords[~part_a]
        else:
            min_a = box_min[part_a]
            max_a = box_max[part_a]

        if part_b < 0:
            min_b = max_b = coords[~part_b]
        else:
            min_b = box_min[part_b]
            max_b = box_max[part_b]

        distance = 0.0
        for d in range(dimension):
            gap = max(min_a[d] - max_b[d], min_b[d] - max_a[d], 0.0)
            distance += gap * gap
        return distance

    def push_children(stack, split_part, other_part, split_a):
        children = [~point[split_part]]
        child = left[split_part]
        if child >= 0:
            children.append(child)
        child = right[split_part]
        if child >= 0:
            children.append(child)

        entries = []
        for child in children:
            if split_a:
                a, b = child, other_part
            else:
                a, b = other_part, child
            entries.append((box_distance_sq(a, b), a, b))

        # The stack pops from the end, so reverse sorting visits the closest
        # child pair first and establishes useful upper bounds earlier.
        entries.sort(reverse=True)
        stack.extend(entries)

    stack = []

    # Processing nodes in postorder finds local edges before examining pairs
    # between larger subtrees, which improves the initial upper bounds.
    for node in tree.postorder:
        parts = [~point[node]]
        if left[node] >= 0:
            parts.append(left[node])
        if right[node] >= 0:
            parts.append(right[node])

        entries = []
        for i, part_a in enumerate(parts):
            for part_b in parts[i + 1:]:
                entries.append((box_distance_sq(part_a, part_b), part_a, part_b))
        entries.sort(reverse=True)
        stack.extend(entries)

        while stack:
            lower_bound, part_a, part_b = stack.pop()
            mask_a = part_mask(part_a)
            mask_b = part_mask(part_b)

            # Both subtrees are homogeneous and belong to the same component.
            if mask_a == mask_b and mask_a & (mask_a - 1) == 0:
                continue

            # max(A union B) == max(max(A), max(B)). Node maxima are cached
            # using a witness component, avoiding a component-set scan for
            # nearly every node-pair comparison.
            upper_bound = max(
                part_upper_bound(part_a),
                part_upper_bound(part_b),
            )

            if lower_bound >= upper_bound:
                continue

            atomic_a = is_atomic(part_a)
            atomic_b = is_atomic(part_b)
            if atomic_a and atomic_b:
                p = part_point(part_a)
                q = part_point(part_b)
                component_p = component[p]
                component_q = component[q]
                if component_p == component_q:
                    continue

                # For two singleton boxes the lower bound is the exact squared
                # point-to-point distance, so no additional distance call is needed.
                distance_sq = lower_bound
                if distance_sq < best_sq[component_p]:
                    best_sq[component_p] = distance_sq
                    best_u[component_p] = p
                    best_v[component_p] = q
                if distance_sq < best_sq[component_q]:
                    best_sq[component_q] = distance_sq
                    best_u[component_q] = q
                    best_v[component_q] = p
                continue

            size_a = 0.0 if atomic_a else box_size[part_a]
            size_b = 0.0 if atomic_b else box_size[part_b]
            count_a = 1 if atomic_a else subtree_size[part_a]
            count_b = 1 if atomic_b else subtree_size[part_b]

            split_a = atomic_b or (
                not atomic_a
                and (
                    size_a > size_b
                    or (size_a == size_b and count_a >= count_b)
                )
            )
            if split_a:
                push_children(stack, part_a, part_b, True)
            else:
                push_children(stack, part_b, part_a, False)

    return best_sq, best_u, best_v


def boruvka_emst_dual_opt(points):
    """Optimized dual-KD-tree Boruvka EMST implementation.

    This keeps the algorithm used by boruvka_emst_dual, but replaces the hot
    object-heavy traversal with flat arrays, component bitmasks, squared
    distances, and an explicit stack.
    """

    n = len(points)
    if n < 2:
        return []

    tree = _DualKDTreeOpt(points)
    uf = UnionFind(n)
    solution = []

    while uf.size() > 1:
        best_sq, best_u, best_v = _dual_opt_find_best(tree, uf)
        before = uf.size()

        for component in range(n):
            u = best_u[component]
            if u < 0:
                continue
            v = best_v[component]
            if uf.find(u) == uf.find(v):
                continue
            uf.union(u, v)
            solution.append((u, v, sqrt(best_sq[component])))

        assert uf.size() < before, "dual KD-tree traversal must merge a component"

    return solution


class KDTreeBoruvkaOpt(EMST):
    def desc(self) -> str:
        return "Optimized version written by Codex"

    def get_mst_edges(self, points: list[Point]) -> list[Edge]:
        return boruvka_emst_dual_opt(points)

# ------------------
# Validation methods
# ------------------


def check_cycles(edges):
    adj = defaultdict(list)
    for e in edges:
        u, v, _ = e
        adj[u].append(v)
        adj[v].append(u)

    vis = {}
    def dfs(v, p, adj):
        vis[v] = True

        for c in adj[v]:
            if c == p:
                continue
            if c in vis and vis[c]:
                raise Exception('cycle found')
            dfs(c, v, adj)
    dfs(0, -1, adj)


def get_cost(edges):
    cost = 0
    for u, v, w in edges:
        cost += w
    return cost


def main():
    points = read_input()
    n = len(points)

    print('Starting solvers...')
    solvers = [
        NaiveBoruvka(),
        KDTreeBoruvkaOpt(),
        KDTreeBoruvka(),
        KDTreeDualBoruvka(),
        SciPyMST()
    ]

    rows = []
    for solver in solvers:
        print(f"Running {solver.desc()}...")
        start = time.perf_counter()
        edges = solver.get_mst_edges(points)
        elapsed = time.perf_counter() - start

        rows.append([
            solver.desc(),
            elapsed,
            get_cost(edges),
        ])

        assert len(edges) == n - 1
        check_cycles(edges)

    print(tabulate(
      rows,
      headers=["Algorithm", "Time (s)", "Cost"],
      tablefmt="rounded_grid",
      floatfmt=".3f",
    ))

main()
