(() => {
  "use strict";

  const SIZE = 512;
  const SVG_NS = "http://www.w3.org/2000/svg";

  function svgElement(name, attributes = {}, text = "") {
    const element = document.createElementNS(SVG_NS, name);
    for (const [key, value] of Object.entries(attributes)) {
      element.setAttribute(key, value);
    }
    if (text) element.textContent = text;
    return element;
  }

  function randomInteger(lo, hi) {
    return lo + Math.floor(Math.random() * (hi - lo + 1));
  }

  function partition(points, lo, hi, pivotIndex, axis) {
    const pivot = points[pivotIndex];
    [points[pivotIndex], points[hi]] = [points[hi], points[pivotIndex]];
    let store = lo;

    for (let i = lo; i < hi; i += 1) {
      if (points[i][axis] <= pivot[axis]) {
        [points[store], points[i]] = [points[i], points[store]];
        store += 1;
      }
    }

    [points[store], points[hi]] = [points[hi], points[store]];
    return store;
  }

  function medianIndex(points, lo, hi, axis) {
    const target = lo + Math.floor((hi - lo) / 2);
    let left = lo;
    let right = hi;

    while (true) {
      if (left === right) return left;
      const pivot = partition(points, left, right, randomInteger(left, right), axis);
      if (pivot === target) return pivot;
      if (pivot > target) right = pivot - 1;
      else left = pivot + 1;
    }
  }

  function buildKdTree(inputPoints) {
    const points = [...inputPoints];

    function build(lo, hi, depth, bounds, parent = null) {
      if (lo > hi) return null;
      const axis = depth % 2 === 0 ? "x" : "y";
      const median = medianIndex(points, lo, hi, axis);
      const pivot = points[median];
      const node = { pivot, axis, depth, bounds, parent, left: null, right: null };
      const leftBounds = { ...bounds };
      const rightBounds = { ...bounds };

      if (axis === "x") {
        leftBounds.xMax = pivot.x;
        rightBounds.xMin = pivot.x;
      } else {
        leftBounds.yMax = pivot.y;
        rightBounds.yMin = pivot.y;
      }

      node.left = build(lo, median - 1, depth + 1, leftBounds, node);
      node.right = build(median + 1, hi, depth + 1, rightBounds, node);
      return node;
    }

    return build(0, points.length - 1, 0, {
      xMin: 0,
      yMin: 0,
      xMax: SIZE,
      yMax: SIZE,
    });
  }

  function allNodes(root) {
    const nodes = [];
    function visit(node) {
      if (!node) return;
      nodes.push(node);
      visit(node.left);
      visit(node.right);
    }
    visit(root);
    return nodes;
  }

  function assignTreePositions(root, options = {}) {
    const nodes = allNodes(root);
    const width = options.width ?? 560;
    const margin = options.margin ?? 38;
    const top = options.top ?? 46;
    const levelGap = options.levelGap ?? 105;
    const spacing = nodes.length > 1 ? (width - 2 * margin) / (nodes.length - 1) : 0;
    let rank = 0;

    function visit(node) {
      if (!node) return;
      visit(node.left);
      node.treeX = margin + rank * spacing;
      node.treeY = top + node.depth * levelGap;
      rank += 1;
      visit(node.right);
    }
    visit(root);
  }

  function distance(a, b) {
    return Math.hypot(a.x - b.x, a.y - b.y);
  }

  window.KDTreeDemo = {
    SIZE,
    allNodes,
    assignTreePositions,
    buildKdTree,
    distance,
    svgElement,
  };
})();
