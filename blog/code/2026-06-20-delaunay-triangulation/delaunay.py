import math
import random
from collections import defaultdict
from scipy.spatial import ConvexHull

# Value >10x any coordinate in any points
MAXV = 1000

def generate_random_2d_points(
    N, min_coord: int = 0, max_coord: int = 100, seed=None
) -> list[tuple[int, int]]:
    rng = random.Random(seed)

    def slope(a, b):
        # Direction from a to b as a reduced (dy, dx) fraction with a canonical sign,
        # so collinear directions hash equal. Integer-exact (no floats).
        dx, dy = b[0] - a[0], b[1] - a[1]
        g = math.gcd(dx, dy)
        dx, dy = dx // g, dy // g
        if dx < 0 or (dx == 0 and dy < 0):
            dx, dy = -dx, -dy
        return (dy, dx)

    points: list[tuple[int, int]] = []
    # For each accepted point, the set of slopes from it to every other accepted point.
    slopes: list[set[tuple[int, int]]] = []
    while len(points) < N:
        candidate = (
            rng.randint(min_coord, max_coord),
            rng.randint(min_coord, max_coord),
        )
        if candidate in points:
            continue
        # Candidate is collinear with two existing points iff, from some existing point p,
        # the slope p->candidate already appears among p's recorded slopes. O(N) per check.
        cand_slopes = [slope(p, candidate) for p in points]
        if any(s in slopes[i] for i, s in enumerate(cand_slopes)):
            continue
        # Accept: register the new slope on each existing point and seed the candidate's set.
        for i, s in enumerate(cand_slopes):
            slopes[i].add(s)
        slopes.append({slope(candidate, p) for p in points})
        points.append(candidate)
    return points

def orient(a, b, c) -> int:
    # >0 if a, b, c are counterclockwise, <0 if clockwise, 0 if collinear.
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])


def in_circumcircle(t, p) -> bool:
    a, b, c = t
    # True if p lies strictly inside the circumcircle of triangle a, b, c.
    # Exact for integer coordinates (all-integer arithmetic, no division).
    if orient(a, b, c) < 0:
        b, c = c, b  # normalize to counterclockwise
    ax, ay = a[0] - p[0], a[1] - p[1]
    bx, by = b[0] - p[0], b[1] - p[1]
    cx, cy = c[0] - p[0], c[1] - p[1]
    det = (
        (ax * ax + ay * ay) * (bx * cy - cx * by)
        - (bx * bx + by * by) * (ax * cy - cx * ay)
        + (cx * cx + cy * cy) * (ax * by - bx * ay)
    )
    return det > 0


def bounding_triangle(P):
    "rectangle triangle enough to contain points of P"
    a = (-MAXV, -MAXV)
    b = (-MAXV, 2*MAXV)
    c = (2*MAXV, -MAXV)
    return (a, b, c)

def edge(p1, p2):
    "normalized edge"
    return (p1, p2) if p1 < p2 else (p2, p1)


def edge_freq(tris):
    freq = defaultdict(int)
    for (a, b, c) in tris:
        freq[edge(a, b)] += 1
        freq[edge(a, c)] += 1
        freq[edge(b, c)] += 1
    return freq

def edges_used_once(tris):
    freq = edge_freq(tris)
    uniq_edges = []
    for e, cnt in freq.items():
        if cnt == 1:
            uniq_edges.append(e)
    return uniq_edges

def has_any_vertex(t, vs):
    for v in vs:
        if v in t:
            return True
    return False

def is_delaunay(P, tris):
    for p in P:
        for t in tris:
           if in_circumcircle(t, p):
            return False
    return True

P = generate_random_2d_points(10, seed=42)

def compute_delaunay(P):
    t0 = bounding_triangle(P)
    tris = [t0]
    for p in P:
        bad_tris = []
        good_tris = []
        for t in tris:
            if in_circumcircle(t, p):
                bad_tris.append(t)
            else:
                good_tris.append(t)
        boundary = edges_used_once(bad_tris)
        for (a, b) in boundary:
            good_tris.append((a, b, p))
        tris = good_tris

    a, b, c = t0
    tris = [
        t for t in tris if not has_any_vertex(t, [a, b, c])
    ]
    return tris

tris = compute_delaunay(P)
assert is_delaunay(P, tris)

hull = ConvexHull(P)
h = len(hull.vertices)

e = len(edge_freq(tris))
n = len(P)
assert e == 3 * n - 3 - h, "Euler's equation"
