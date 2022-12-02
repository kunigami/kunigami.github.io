from sys import stdin
from queue import Queue

# Sufficently large number
INF = 12341234
S, T = 0, 1

class Hungarian:
  def __init__(self, edges, n, m):
    self.n = max(n, m)
    self.build_adjacency_mat(edges)

  def build_adjacency_mat(self, edges):
    adj = [ [None]*self.n, [None]*self.n ]
    for i in range(self.n):
      adj[S][i] = [0]*self.n
      adj[T][i] = [0]*self.n

    for [s, t, w] in edges:
      adj[S][s][t] = adj[T][t][s] = w

    self.adj = adj

  def init_parent(self):
    self.parent = [ [None]*self.n, [None]*self.n ]

  def init_candidates(self):
    self.candidates = Queue()
    # every exposed node in S is a candidate
    for i in range(self.n):
        if self.match[S][i] is None:
          self.candidates.put((0, i))

  def init_visited(self):
    self.visited = [[False]*self.n, [False]*self.n]

  def init_match(self):
    self.match = [[None]*self.n, [None]*self.n]

  def init_u(self):
    u = [0]*self.n
    for i in range(self.n):
      for j in range(self.n):
        u[i] = max(u[i], self.adj[S][i][j])

    self.u = u

  def init_v(self):
    self.v = [0]*self.n

  def init_slack(self):
    self.slack = [INF]*self.n

  def expand_matching(self):
    self.init_candidates()
    self.init_parent()
    self.init_visited()
    self.init_slack()

    while True:
      augmented = self.visit_forest()
      if augmented:
        break

      self.change_duals()

  def visit_s(self, i):
    # update pi from edges not in match
    for j in range(self.n):
      if self.match[S][i] == j:
        continue

      slack = self.u[i] + self.v[j] - self.adj[S][i][j]
      if slack < self.slack[j]:
        self.slack[j] = slack
        self.parent[T][j] = i

        # edge (i,j) is now in the forest. keep visiting
        if slack == 0:
          self.candidates.put((1, j))

  def visit_t(self, j):
    i = self.match[T][j]
    # found an augmenting path
    if i is None:
      self.augment(j)
      return True

    self.parent[S][i] = j
    self.candidates.put((0, i))
    return False

  def visit_forest(self):
    while not self.candidates.empty():
      [side, c] = self.candidates.get()

      if self.visited[side][c]:
        continue

      self.visited[side][c] = True

      if side == S: # c in S
        self.visit_s(c)
      else: # c in T
        augmented = self.visit_t(c)
        if augmented:
          return True
    return False

  def augment(self, j):
    while j is not None:
      i = self.parent[T][j]
      self.match[T][j] = i
      self.match[S][i] = j
      j = self.parent[S][i]

  def change_duals(self):
    delta = min(v for v in self.slack if v > 0)

    for i in range(self.n):
      if self.visited[S][i]:
        self.u[i] -= delta

    for j in range(self.n):
      if self.visited[T][j]:
        self.v[j] += delta
      # frontier edge
      elif self.slack[j] > 0:
        self.slack[j] -= delta
        # note: self.parent[j] has been set
        # during the forest visit
        if self.slack[j] == 0:
          self.candidates.put((1, j))

  def solve(self):
    self.init_match()
    self.init_u()
    self.init_v()
    self.init_slack()

    # for each iteration we should increase the matching side
    for t in range(self.n):
      self.expand_matching()

    return sum(self.u) + sum(self.v)


Tc = int(stdin.readline())
for t in range(Tc):
    [n, m] = [int(x) for x in stdin.readline().split(' ')]

    edges = []
    while True:
      [u, v, w] = [int(x) for x in stdin.readline().split(' ')]
      if u == 0:
        break

      edges.append([u-1, v-1, w])

    h = Hungarian(edges, n, m)
    v = h.solve()

    print(v)

    # break
