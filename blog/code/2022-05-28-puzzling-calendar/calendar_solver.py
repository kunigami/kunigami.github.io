from functools import cached_property
import json
import numpy as np

data = [
"""
###
###
""",
"""
##.
###
""",
"""
#..
#..
###
""",
"""
####
..#.
""",
"""
##
#.
##
""",
"""
###.
..##
""",
"""
..#
###
#..
""",
"""
####
...#
""",
]

def pair(x):
  return (x[0], x[1])

class Board:
  """
    month: 1-12
    day:   1-31
  """
  def __init__(self, month, day):
    self.mat = np.zeros(shape=(7, 7))
    # cut-off corners
    self.mat[0][6] = -1
    self.mat[1][6] = -1
    for j in range(3, 7):
      self.mat[6][j] = -1

    month_row = (month - 1)//6
    month_col = (month - 1) % 6
    self.mat[month_row][month_col] = -1

    day_row = (day - 1)//7 + 2
    day_col = (day - 1) % 7
    self.mat[day_row][day_col] = -1

    self.pieces_map = {}

  def is_inside(self, p):
    [i, j] = p
    [n, m] = self.mat.shape
    return i >= 0 and i < n and j >= 0 and j < m

  def at(self, at):
    return self.mat[pair(at)]

  def is_set(self, p):
    return self.at(p) != 0

  def is_valid(self, p):
    return self.is_inside(p) and not self.is_set(p)

  def fits_at(self, piece, at):
    coords = piece.relative_coords
    return all(self.is_valid(coord + at) for coord in coords)

  def set_at(self, at, tag):
    if not self.is_inside(at):
      raise Exception("out of bounds")
    if self.is_set(at):
      raise Exception("already occupied")
    self.mat[pair(at)] = tag

  def clear_at(self, at):
    if not self.is_inside(at):
      raise Exception("out of bounds")
    if not self.is_set(at):
      raise Exception("nothing to clear")
    tag = self.at(at)
    self.mat[pair(at)] = 0

  def has_piece(self, piece_id):
    return piece_id in self.pieces_map

  def place_pice(self, piece, at):
    coords = piece.relative_coords
    for coord in coords:
      self.set_at(coord + at, piece.id)
    self.pieces_map[piece.id] = [piece, at]

  def remove_pice(self, piece, at):
    coords = piece.relative_coords
    for coord in coords:
      self.clear_at(coord + at)
    del self.pieces_map[piece.id]

  def get_open_coords(self):
    [n, m] = self.mat.shape
    coords = []
    for i in range(n):
      for j in range(m):
        if not self.is_set([i, j]):
          coords.append(np.array([i, j]))
    return coords



class Piece:
  def __init__(self, id_, rows, is_refl=False, rot_cnt=0):
    self.id = id_
    self.mat = rows
    self.is_refl = is_refl
    self.rot_cnt = rot_cnt

  @staticmethod
  def from_raw(id_, raw):
    rows = []
    for l in raw.split("\n"):
      if l == '':
        continue
      rows.append([(1 if c == '#' else 0) for c in l])

    return Piece(id_, np.array(rows))

  def get_primary_coord(self):
    [n, m] = self.mat.shape
    for i in range(n):
      for j in range(m):
        if self.mat[i][j] == 1:
          return np.array([i, j])

  # relative to primary
  @cached_property
  def relative_coords(self):
    origin = self.get_primary_coord()
    coords = []
    [n, m] = self.mat.shape
    for i in range(n):
      for j in range(m):
        if self.mat[i][j] == 1:
          coords.append(np.array([i, j]) - origin)
    return coords

  def __str__(self):
    [n, m] = self.mat.shape
    ls = [0]*n
    for i in range(n):
      ls[i] = ''
      for j in range(m):
        ls[i] += '#' if self.mat[i][j] else '.'
    return "\n".join(ls)

  def rotate(self):
    return Piece(self.id, np.rot90(self.mat), rot_cnt=(self.rot_cnt + 1)%4)

  def reflect(self):
    return Piece(self.id, np.fliplr(self.mat), is_refl=not self.is_refl)

class Solver:

  def __init__(self, b, pieces):
    self.b = b
    self.coords = b.get_open_coords()

    self.pieces_forms = {}
    for p in pieces:
      self.pieces_forms[p.id] = self.compute_piece_forms(p)

    self.sol = None

  def compute_piece_forms(self, p):
    forms = {}

    for is_refl in [False, True]:
      for r in range(4):
        forms[str(p)] = p
        p = p.rotate()

      p = p.reflect()

    return forms.values()

  def backtrack(self, idx = 0):

    if idx == len(self.coords):
      print(self.b.mat)
      self.sol = self.b.pieces_map.copy()
      return True

    coord = self.coords[idx]
    b = self.b
    if b.is_set(coord):
      return self.backtrack(idx + 1)

    for piece_id, forms in self.pieces_forms.items():
      if b.has_piece(piece_id):
        continue

      # try different rotations
      for form in forms:

        if not b.fits_at(form, coord):
          continue

        b.place_pice(form, coord)
        r = self.backtrack(idx + 1)
        b.remove_pice(form, coord)

        if r:
          return True

    return False

  def get_compacted_sol(self):
    # sort by piece id
    sol = sorted(self.sol.items(), key=lambda x: x[0])
    pieces_info = [m[1] for m in sol]
    int_sols = []
    for piece_info in pieces_info:
      [piece, at] = piece_info
      pos = [int(x) for x in at]
      int_sols.append([
        piece.rot_cnt,
        1 if piece.is_refl else 0,
        pos
      ])
    return int_sols


pieces = [Piece.from_raw(i + 1, data[i]) for i in range(len(data))]

sols = []
for month in range(12):
  for day in range(31):

    print(f'solving for {month + 1}/{day + 1}')
    b = Board(month + 1, day + 1)
    s = Solver(b, pieces)
    s.backtrack()
    sols.append(s.get_compacted_sol())

with open("sol.json", "w") as file:
  file.write("solutions = [\n")
  for sol in sols:
    print(sol)
    file.write(json.dumps(sol) + ",\n")
  file.write("]\n")
