---
layout: post
title: "Delaunay Triangulation"
tags: [computational geometry]
excerpt_separator: <!--more-->
vanity: "2026-06-20-delaunay-triangulation"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/delaunay.png" alt="Delaunay triangulation." />
</figure>

Boris Nikolayevich Delaunay was a Russian mathematician (1890 – 1980), who is best known for inventing the Delaunay triangulation.

Boris was the descendant of a French army officer de Launay, who was captured in Russia during Napoleon's failed attempt to invade Russia. After his release, De Launay stayed back in Russia and married into a noble Russian family.

In this post we'll study the Delaunay triangulation and the Bowyer–Watson algorithm for finding a Delaunay triangulation in $O(n^2)$.

<!--more-->

## Triangulation

Let $P$ be a set of points in the plane and such that no 3 points are collinear. An **edge** is a segment of line connecting a pair of points. Two edges intersect if their corresponding segments intersect. A **triangulation** can be defined as any maximal set of edges that do not intersect.

One way of getting an intuition about this property is to imagine we're constructing a triangulation randomly: choose any pair of points that are not yet connected and try adding an edge. If this edge doesn't intersect any existing edge, keep it, else choose the next pair. If we cannot find any edge to add, we have a maximal set and thus a triangulation.

A more visual way to define a triangulation is that all the faces (a bounded region not containing any points except its vertices) are triangles and the outer face (external edges) is the convex hull of $P$.

The advantage of this second definition is that it generalizes to higher dimensions. In 3D a triangulation is a set of edges such that all faces are tetrahedra and the outer face is the 3D convex hull of $P$.

### Properties

**Lemma 1.** Let $P$ be a set of $n$ points in 2D and $h$ the number of points in its convex hull. Then the number of edges in any triangulation is:

$$
e = 3n - 3 - h
$$

<proof>
Because the graph induced by the triangulation is planar, we can use Euler's formula:

$$
n - e + f = 2
$$

Where $n$, $e$ and $f$ are the number of vertices, edges and faces respectively. Euler's formula counts the outer unbounded face too, so if $t$ is the number of triangles, then $f = t + 1$ so:

$$
(1.1) \quad n - e + t + 1 = 2
$$


The triangles add up $3t$ edges, but inner edges are counted twice. Let $e_i$ and $e_e$ be the inner and outer edges of the triangulation, so we have $e = e_i + e_e$ and $3t = e + e_i$. The number of outer edges is equal to the number of points in the convex hull, so $e_i = e - h$ and $3t = 2e - h$. Replacing in (1.1) we have:

$$
3n - 3e + 3t + 3 = 3n - 3e + 2e - h + 3 = 6
$$

so

$$
e = 3n - 3 - h
$$

</proof>

This means that for any fixed set of points, since its convex hull is unique, the number of edges is the same for any triangulation and also $O(n)$.

## Delaunay Triangulation

The intuition about the Delaunay triangulation is that it avoids "skinny" triangles. It achieves that by requiring that for every triangle in the triangulation its circumcircle contains no other points in its interior. When a triangle is skinny, their vertices are closer to be collinear and the circumcircle blows out, increasing the likelihood of including other vertices.

It's not obvious why it's always possible to find a triangulation with this property. To prove that, we first need to introduce a technique called **lifting**. It consists in mapping each 2D point $(x, y)$ to a 3D one $(x, y, x^2 + y^2)$. Geometrically this is lifting the points onto the surface of the paraboloid $z = x^2 + y^2$. We denote by $p_i$ a point in the original set $P$ and by $p'_i$ the corresponding lifted point.

<figure class="center_children">
  <img src="{{resources_path}}/parabolloid.png" alt="See caption." />
  <figcaption>Figure 1: Points mapped to the surface of a paraboloid. Screenshot from <a href="https://compgeo.github.io/voronoi-3d/src/index.html">compgeo.github.io</a>.</figcaption>
</figure>


The first interesting observation is that circles map to planes in the lifted space.

**Lemma 2.** Let $p_1, p_2$ and $p_3$ be non-collinear points from $P$. They form a unique circle, which uniquely maps to a plane in the lifted space containing $p'_1$, $p'_2$ and $p'_3$.

<proof>

A circle of radius $r$ and origin $(x_0, y_0)$ in the $xy$-plane is defined by the equation

$$
(x - x_0)^2 + (y - y_0)^2 = r^2
$$

We can rearrange terms and replace constants to obtain:

$$
x^2 + y^2 - ax - by - c = 0
$$

Since our mapping is such that $z = x^2 + y^2$ we have:

$$
z - ax - by - c = 0
$$

which is the equation of a plane in 3D. Since each $p_i$ satisfies the equation of the circle, their corresponding lifted points satisfy the equation of the plane.

</proof>

Now suppose a point lies inside the circle induced by $p_1, p_2$ and $p_3$. What's the relation with the corresponding plane in the lifted space?

**Lemma 3.** Let $C$ be the circle induced by $p_1, p_2$ and $p_3$. If a point $q$ lies inside $C$, it lies "below" the corresponding plane $P$ in the lifted space. If it is in $C$, it belongs to $P$ and if outside $C$ it lies "above" $P$.

<proof>

This is a consequence of the equations. Suppose $q = (x_q, y_q)$. Then

$$
d = x_q^2 + y_q^2 - ax_q - by_q - c
$$

If $q$ is inside the circle, then $d \lt 0$.

We have $z_q = x_q^2 + y_q^2$. Compare that with $z$ when evaluated at the plane at $(x_q, y_q)$:

$$
z = ax_q + by_q + c
$$

Take $z_q - z$:

$$
z_q - z = x_q^2 + y_q^2 - ax_q - by_q - c = d
$$

so $z_q - z = d \lt 0$, or that $z_q \lt z$ so $q'$ lies below the plane. The other results follow by changing the sign to $=$ and $\gt$.

</proof>

Now take the 3D convex hull of the lifted points, which will be a polyhedron with faces. Consider a face that is visible from below. It contains a set of points $p'_1, p'_2, \cdots p'_k$ (mostly $k=3$ but it's possible to have more in degenerate cases). By definition these points are coplanar, and since the circle to plane mapping is unique, the corresponding points $p_1, p_2, \cdots p_k$ must be cocircular in the $xy$-plane.

<figure class="center_children">
  <img src="{{resources_path}}/convex_hull.png" alt="See caption." />
  <figcaption>Figure 2: The 3D convex hull of the lifted points. It's hard to see its details in this resolution. Check out the source website if still available. Screenshot from <a href="https://compgeo.github.io/voronoi-3d/src/index.html">compgeo.github.io</a>.</figcaption>
</figure>

We claim that such a circle contains no other point of $P$. If it did, by *Lemma 3* it would map to a lifted point below the plane, which would then lie outside the convex hull, a contradiction. So this satisfies the constraint of the Delaunay triangulation. However, it's possible that $k \gt 3$ but in that case we can triangulate the corresponding polygon in any way we want since for any 3 points we'll obtain the same circle which will still not contain the other *inside*.

By construction we can guarantee every lifted point belongs to at least one face of the convex hull that is visible from below. So if we project each face that is visible from below onto the $xy$-plane and perform triangulation of the degenerate cases, we'll obtain a Delaunay triangulation!

<figure class="center_children">
  <img src="{{resources_path}}/projection.png" alt="See caption." />
  <figcaption>Figure 3: The projected faces from the convex hull on the $xy$-plane define the Delaunay triangulation. Screenshot from <a href="https://compgeo.github.io/voronoi-3d/src/index.html">compgeo.github.io</a>.</figcaption>
</figure>

This proves not only that a Delaunay triangulation always exists but also provides an algorithm to find one.

**Corollary 4.** The Delaunay triangulation always exists for a set of non-collinear points.

It's also possible to show that if there are no cocircular points in $P$, then every Delaunay triangulation maps to the convex hull of the lifted space and hence that it is unique.

## The Bowyer–Watson Algorithm

The algorithm is conceptually simple. We start with an empty triangle that is big enough to contain all points in $P$ and such that the circumcircle of any triangle in $P$ would contain any its vertices. Suppose its vertices are $a, b, c$, then this triangle is the Delaunay triangulation for $a, b, c$.

Then we insert one point $p$ of $P$ at a time. Once we add $p$, remove all triangles that violate the Delaunay conditions (i.e. their circumcicle contains $p$). This will leave out a "hole" whose border is a polygon. Now connect each vertex of this polygon to $p$, forming a new triangulation. This triangulation is a Delaunay triangulation for $a, b, c$ and all the points of $P$ added so far.

A Python implementation is the following:

{% highlight python %}
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
{% endhighlight %}

The complexity of this algorithm is $O(n^2)$ because of the 2 nested loops and the fact that the number of triangles in a triangulation is $O(n)$. But it's possible to optimize the bad triangle search to $O(\log n)$, to obtain a sub-quadratic algorithm.

*Lemma 5* handles the more difficult task of proving the algorithm is correct. The gist of the proof is to show that what the algorithm is doing is essentially constructing the 3D convex hull of the lifted space incrementally.

**Lemma 5.** The Bowyer–Watson algorithm finds the Delaunay triangulation of a set of non-collinear points $P$.

<proof>
The first thing we prove is that at the end of each iteration the triangulation is the Delaunay triangulation of `a, b, c` and all the points added so far. We do it by induction. The base case is trivial since a triangle is the Delaunay triangulation of 3 non-collinear vertices. Now assume `tris` is a Delaunay triangulation at the start of an iteration.
<br /><br />
The first thing we observe is that `bad_tris` is never empty. That's because since `p` lies inside the bounding triangle and `tris` is a valid triangulation for it, it must lie inside one triangle `x, y, z`, so such a triangle is bad. Note that it can't lie on the edge because we're assuming no three points are collinear.
<br /><br />
The point `p` we're adding is contained in the circumcircles of `bad_tris`. Using the lifting argument these triangles correspond to faces in the convex hull visible from below. Since `p` is inside their circles by <i>Lemma 3</i> it lies "below" the supporting plane of those faces. This also means `p` is outside of the existing convex hull and we wish to extend it to include `p`.
<br /><br />
If we were to shine light from `p`, it would illuminate all the faces from the bad triangles but not the good ones. A convex hull containing `p` then would not contain the illuminated faces. Due to convexity, the union of the bad faces forms a contiguous region. We can remove these faces and then connect the vertices of the hole with `p`. Since `p` is now coplanar with the vertices that were previously violating the Delaunay condition, this new triangulation is a Delaunay one. This process is essentially computing the 3D convex hull of the lifted points iteratively (i.e. an online algorithm).
<br /><br />
Now we show the correctness of the last step. Removing a point from a convex hull only removes the faces adjacent to it. Since `a, b, c` are the outermost vertices, their removal won't affect faces involving only vertices of `P`, so the remaining faces visible from below are from the convex hull of `P` and is thus a Delaunay triangulation of `P`.
</proof>


We can complete the implementation by defining the functions we used above, starting with `bounding_triangle()`.

### Bounding Triangle

Generating a triangle based on the coordinates of $P$ is not enough because we need to guarantee that no circumcircle of any triangle in $P$ will contain $a, b$ or $c$. A simple but less robust approach is to just use a very large triangle:

{% highlight python %}
def bounding_triangle2(P):
    "rectangle triangle enough to contain points of P"
    a = (-MAXV, -MAXV)
    b = (-MAXV, 2*MAXV)
    c = (2*MAXV, -MAXV)
    return (a, b, c)
{% endhighlight %}

<br />
### Point inside circumcircle

We can use the lifting technique and instead of determining whether a point $p$ is inside the circumcircle of a triangle $a, b, c$ (which would require finding the circle equation and taking square roots), we check if it's "below" the supporting plane of their corresponding lifted points.

**Lemma 6.** Let $a = (a_x, a_y), b = (b_x, b_y), c = (c_x, c_y)$ be vertices of a triangle oriented counter-clockwise. Then a point $p = (p_x, p_y)$ lies within the circumcircle of this triangle if:

$$
\det

\begin{pmatrix}
a_x-p_x & a_y-p_y & a_z-p_z \\
b_x-p_x & b_y-p_y & b_z-p_z \\
c_x-p_x & c_y-p_y & c_z-p_z
\end{pmatrix}
\gt 0
$$

<proof>
Let $A, B, C$ and $P$ be the 3D points corresponding to $a, b, c, p$. That is, $A = (a_x, a_y, a_x^2 + a_y^2)$ and so on. Let's translate the system so $A$ is at the origin. We have $u = B - A$ and $v = C - A$ and $w = P - A$. Then the cross product:

$$
n = (B - A) \times (C - A)
$$

is the normal vector to the plane $ABC$. We claim that for faces on the lower convex hull, this vector points towards the interior of the convex hull. We compute the $z$-coordinate explicitly as:

$$
n_z = (b_x - a_x)(c_y - a_y) - (b_y - a_y)(c_x - a_x)
$$

which is positive because $a, b, c$ are counter-clockwise. Since this is from a lower face the normal points towards the interior. Then if we take the dot product of this with $(P - A)$ we can tell the orientation of $P$ with respect to the plane:

$$
n \cdot (P - A)
$$

A negative value means it is on opposite sides, hence outside of the convex hull, below the plane and outside the circumcircle. The expression:

$$
((B - A) \times (C - A)) \cdot (P - A)
$$

computed through the determinant:

$$
- \det
\begin{pmatrix}
a_x & a_y & a_x^2 + a_y^2 & 1 \\
b_x & b_y & b_x^2 + b_y^2 & 1 \\
c_x & c_y & c_x^2 + c_y^2 & 1 \\
p_x & p_y & p_x^2 + p_y^2 & 1 \\
\end{pmatrix}
$$

which we can simplify to:

$$
-\det

\begin{pmatrix}
a_x-p_x & a_y-p_y & a_z-p_z \\
b_x-p_x & b_y-p_y & b_z-p_z \\
c_x-p_x & c_y-p_y & c_z-p_z
\end{pmatrix}
$$
</proof>

The code then consists of evaluating this determinant explicitly:

{% highlight python %}
def orient(a, b, c) -> int:
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])

def in_circumcircle(t, p) -> bool:
    a, b, c = t
    # normalize to counterclockwise
    if orient(a, b, c) < 0:
        b, c = c, b
    ax, ay = a[0] - p[0], a[1] - p[1]
    bx, by = b[0] - p[0], b[1] - p[1]
    cx, cy = c[0] - p[0], c[1] - p[1]
    det = (
        (ax * ax + ay * ay) * (bx * cy - cx * by)
        - (bx * bx + by * by) * (ax * cy - cx * ay)
        + (cx * cx + cy * cy) * (ax * by - bx * ay)
    )
    return det > 0
{% endhighlight %}

<br />

### Boundary

Finding the boundary is relatively easy. We just need to count the frequency of each edge and only keep those that only appear once:

{% highlight python %}
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
{% endhighlight %}

<br />
### Has Vertices

The last function is the easiest, there's not much to explain:

{% highlight python %}
def has_any_vertex(t, vs):
    for v in vs:
        if v in t:
            return True
    return False
{% endhighlight %}

<br />
### Validation

One way to test the correctness of our implementation is to check if the resulting triangulation is Delaunay. Which is applying the definition that no points in $P$ are in the circumcircle of a triangle in the triangulation.

{% highlight python %}
def is_delaunay(P, tris):
    for p in P:
        for t in tris:
           if in_circumcircle(t, p):
            return False
    return True

tris = compute_delaunay(P)
assert is_delaunay(P, tris)
{% endhighlight %}

We can also check if some properties of the triangulation holds:

{% highlight python %}
from scipy.spatial import ConvexHull

hull = ConvexHull(P)
h = len(hull.vertices)

e = len(edge_freq(tris))
n = len(P)

assert e == 3 * n - 3 - h
{% endhighlight %}

The full code is on [Github]().


## Conclusion

I had a lot of fun studying the Delaunay triangulation and the implementation of the Bowyer-Watson algorithm. Computational geometry was my favorite theoretical computer science subject and I did my master's in this area.

I took a graduate class in Computational geometry and I do recall hearing about Voronoi and Delaunay triangulation but don't recall how deep the classes went on these topics. I'd be very sad if I actually studied the proofs and the algorithm before because it felt very novel to me. I think about this [often](https://www.kuniga.me/blog/2022/12/27/on-memory.html).

We've seen the relationship between Delaunay and 3D convex hulls here. In the future I hope to connect them to Voronoi diagrams as well.

## Related Posts

In [On Lifetime]({{blog}}/2023/07/10/on-lifetime.html) I mention that I wrote Java applets for interactive computational geometry algorithms such as for polygon triangulation and finding the convex hull in 2D! It did cross my mind to write a JavaScript-based on for the Bowyer-Watson algorithm, especially if Claude can do most of the implementation, but it would still take some work to test, polish and read the code, so I decided to pass.

In the [The Gauss-Lucas Theorem]({{blog}}/2023/11/03/gauss-lucas-theorem.html) we also deal with convex hulls and I used the same Python library `scipy.spatial.ConvexHull` for validating the number of edges property.
