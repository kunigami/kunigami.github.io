from __future__ import annotations
from dataclasses import dataclass
from math import inf, sqrt
import random

cnt = [0]

@dataclass(frozen=True)
class Dimension:
    n: int
    v: int = 0

    def next(self):
        next_v = (self.v + 1) % self.n
        return Dimension(self.n, next_v)

@dataclass(frozen=True)
class Point:
    data: tuple[int, ...]

    def __getitem__(self, dim: Dimension):
        return self.data[dim.v]

    def __len__(self) -> int:
        return len(self.data)

    def __abs__(self) -> float:
        return sqrt(sum(x*x for x in self.data))

    def __sub__(self, other: Point):
        sub = tuple(a-b for a,b in zip(self.data, other.data))
        return Point(sub)

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

    def __setitem__(self, i: int, p: Point):
        self.pts[i] = p

def random_point(dim, lb, ub) -> Point :
    return Point(tuple([random.randint(lb, ub) for _ in range(dim)]))

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
    pivot: Point
    left: KDNode
    right: KDNode

    def is_leaf(self):
        return self.left is None and self.right is None


def validate_kd_tree(pts, kd_tree):

    visited = set()
    def visit(node):
        if node is None:
            return
        visited.add(node.pivot)
        visit(node.left)
        visit(node.right)

    visit(kd_tree)

    pts_set = set(pts)
    assert pts_set == visited


def build_kd_tree(pts, dim):
    if pts.lo > pts.hi:
        return None

    mi = median_index(pts, dim)

    left = build_kd_tree(pts.range(pts.lo, mi - 1), dim.next())
    right = build_kd_tree(pts.range(mi + 1, pts.hi), dim.next())

    return KDNode(pivot=pts[mi], left=left, right=right)

def query_kd_tree(node, q, dim, ub = inf):
    if not node:
        return None

    cnt[0] += 1
    p = node.pivot

    if node.is_leaf():
        return p

    if q[dim] <= p[dim]:
        main = node.left
        other = node.right
    else:
        main = node.right
        other = node.left

    c = None

    # pivot is a candidate
    if dist(q, p) < ub:
        c = p
        ub = dist(q, p)

    # main branch
    c1 = query_kd_tree(main, q, dim.next(), ub)
    if dist(q, c1) < ub:
        c = c1
        ub = dist(q, c1)

    # check if the other branch is better

    # minimum distance we can expect to find
    # on the other branch
    lb = abs(q[dim] - p[dim])
    if lb < ub:
        c2 = query_kd_tree(other, q, dim.next(), ub)
        if dist(q, c2) < ub:
            c = c2
    else:
        print(f'lb = {lb}, ub = {ub}, diff = {abs(ub - lb)}')

    return c


def dist(p: Point, q: Point):
    if p is None or q is None:
        return inf

    return abs(p - q)


def golden(points, q):
    '''easy to prove correct, O(n) implementation'''

    min_d = inf
    closest = None
    for p in points:
        d = dist(p, q)
        if d < min_d:
            closest = p
            min_d = d
    return closest

def benchmark(points, queries):

    dim = Dimension(len(points[0]))
    view = View(points)
    kd_tree = build_kd_tree(view, dim)
    validate_kd_tree(points, kd_tree)

    for i, q in enumerate(queries):
        print(f'case #{i}:')
        cnt[0] = 0
        closest = golden(points, q)
        closest2 = query_kd_tree(kd_tree, q, dim)
        assert closest == closest2, f'{closest} (d={dist(closest, q)}) vs {closest2} (d={dist(closest2, q)})'

        d = dist(closest, q)
        print(f'd = {d}, visited = {cnt[0]}')


DIM = 3
LB = -100
UB = 100
points = [random_point(DIM, LB, UB) for _ in range(1000)]
queries = [random_point(DIM, LB, UB) for _ in range(10)]

benchmark(points, queries)
