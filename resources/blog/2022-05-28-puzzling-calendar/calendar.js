
const shapes = [
`
###
###
`,
`
##.
###
`,
`
#..
#..
###
`,
`
####
..#.
`,
`
##
#.
##
`,
`
###.
..##
`,
`
..#
###
#..
`,
`
####
...#
`,
];

const colors = [
  '#ffcdb2',
  '#a3b18a',
  '#83c5be',
  '#d5bdaf',
  '#90e0ef',
  '#f2cc8f',
  '#f28482',
  '#be95c4',
];

const Pair = {
  add: (p1, p2) => {
    return [p1[0] + p2[0], p1[1] + p2[1]];
  },
  eq: (p1, p2) => {
    return p1[0] == p2[0] && p1[1] == p2[1];
  },
  sub: (p1, p2) => {
    return [p1[0] - p2[0], p1[1] - p2[1]];
  },
  mul: (p, k) => {
    return [p[0]*k, p[1]*k];
  }
};

class Piece {
  constructor(rows) {
    this._rows = rows;
  }

  static from_raw(raw) {
    const rows = raw.split("\n")
      .filter(l => l != '')
      .map(l => Array.from(l).map(c => c == '#' ? 1 : 0));
    return new Piece(rows);
  }

  reflect() {
    const {nrows, ncols} = this.shape();
    const new_rows = new Array(nrows);
    for (let i = 0; i < nrows; i++) {
      new_rows[i] = new Array(ncols);
       for (let j = 0; j < ncols; j++) {
         new_rows[i][ncols - 1 - j] = this._rows[i][j];
       }
    }
    return new Piece(new_rows);
  }

  _rotate() {
    const {nrows, ncols} = this.shape();
    const new_rows = new Array(ncols);
    for (let j = 0; j < ncols; j++) {
      new_rows[j] = new Array(nrows);
       for (let i = 0; i < nrows; i++) {
         new_rows[j][i] = this._rows[i][ncols - 1 - j];
       }
    }
    return new Piece(new_rows);
  }

  rotate(n) {
    let new_shape = this;
    for (let i = 0; i < n%4; i++) {
      new_shape = new_shape._rotate();
    }
    return new_shape;
  }

  shape() {
    return {nrows: this._rows.length, ncols: this._rows[0].length};
  }

  get_primary_coord() {
    const {nrows, ncols} = this.shape();
    for (let i = 0; i < nrows; i++) {
       for (let j = 0; j < ncols; j++) {
         if (this._rows[i][j] == 1) {
           return [i, j];
         }
       }
    }
  }

  is_valid(p) {
    const {nrows, ncols} = this.shape();
    const [i, j] = p;
    return !(i < 0 || i > nrows || j < 0 || j > ncols);
  }

  hash(p) {
    const {ncols} = this.shape();
    return p[0]*(ncols + 1) + p[1];
  }

  from_hash(h) {
    const {ncols} = this.shape();
    return [Math.floor(h / (ncols + 1)), h % (ncols + 1)];
  }

  get_coords() {
    const {nrows, ncols} = this.shape();
    const coords = [];
    for (let i = 0; i < nrows; i++) {
      for (let j = 0; j < ncols; j++) {
        if (this._rows[i][j] == 1) {
          coords.push([i, j]);
        }
      }
    }
    return coords;
  }

  build_edge_map() {
    const edges_map = {};
    // vertices of square clock-wise
    const deltas = [ [0, 0], [0, 1], [1, 1], [1, 0] ];
    const coords = this.get_coords();
    coords.forEach(coord => {
      for (let k = 0; k < deltas.length; k++) {
        const src = this.hash(Pair.add(coord, deltas[k]));
        const next_delta = deltas[(k + 1) % deltas.length]
        const dst = this.hash(Pair.add(coord, next_delta));

        // rm duplicate edges
        if (dst in edges_map && edges_map[dst].has(src)) {
          edges_map[dst].delete(src);
        } else {
          if (!(src in edges_map)) {
            edges_map[src] = new Set();
          }
          edges_map[src].add(dst);
        }
      }
    });
    return edges_map;
  }

  get_polygon_coords() {
    // assumption: edges_map forms a circuit
    const edges_map = this.build_edge_map();
    const initial_coord = this.get_primary_coord();
    let coord = initial_coord;
    const pts = [initial_coord];
    while (true) {
      const adjs = Array.from(edges_map[this.hash(coord)] || new Set());
      if (adjs.length == 0) {
        throw new Error(`Could not find edges for ${coord}`);
      }
      let next_coord = this.from_hash(adjs[0]);
      if (Pair.eq(next_coord, initial_coord)) {
        break;
      }
      coord = next_coord;
      pts.push(coord);
    }

    // Normalize coordinates so primary coord is 0,0
    return pts.map(pt => Pair.sub(pt, initial_coord));
  }
}


function is_cell(i, j) {
  if (i <= 1) {
    return j < 6;
  }
  if (i == 6) {
    return j < 3;
  }
  return true;
}

const MONTH_NAMES_SHORT = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
function get_label(i, j) {
  if (i <= 1) {
    return MONTH_NAMES_SHORT[i*6 + j];
  }
  return (i - 2)*7 + j + 1;
}

function Calendar() {

  const offset = 22;
  const sq_sz = 65;

  const get_pos = ([i, j]) => {
      const x = offset + j * sq_sz;
      const y = offset + i * sq_sz;
      return [y, x];
    };

  const svgSquares = [];
  for (let i = 0; i < 7; i++) {
    for (let j = 0; j < 7; j++) {
      if (!is_cell(i, j)) {
        continue;
      }
      const [y, x] = get_pos([i, j]);

      svgSquares.push(
        <g key={i*7 + j}>
          <rect width={sq_sz} height={sq_sz} x={x} y={y} fill="none" stroke="none" />
          <text
            x={x + sq_sz / 2}
            y={y + sq_sz / 2}
            dominantBaseline="middle"
            textAnchor="middle" >
          {get_label(i, j)}
          </text>
        </g>
      );
    }
  }

  const draw_polygon = (shape, [i, j], color, idx) => {
    const pts = shape.get_polygon_coords();
    const points_str = pts.map(pt => {
      const p = get_pos([i, j]);
      const [y, x] = Pair.add(Pair.mul(pt, sq_sz), p);
      return `${x},${y}`;
    }).join(" ");
    return <polygon key={idx} points={points_str} fill={color} stroke="none" />;
  }

  const date = new Date();
  const month = date.getMonth();
  const day = date.getDate() - 1;
  const date_id = month * 31 + day;
  const today_solution = solutions[date_id];

  const svgPolygons = today_solution.map((piece_config, idx) => {
    const [rot, is_refl, pos] = piece_config;
    let s = Piece.from_raw(shapes[idx]);
    if (is_refl) {
      s = s.reflect();
    }
    s = s.rotate(rot);
    return draw_polygon(s, pos, colors[idx]);
  });

  return <svg width="500" height="500" >
    <rect width="500" height="500" rx="10" fill="#f5ebe0" stroke="none" shapeRendering="geometricPrecision" />
    {svgPolygons}
    {svgSquares}
   </svg>;
}

ReactDOM.render(
  <Calendar />,
  document.querySelector("#calendar")
);
