#include <string.h>
#include <algorithm>
#include <iostream>
#include <vector>
#include <cassert>
#include <climits>

constexpr int S = 0;
constexpr int T = 1;
constexpr int MAXN = 200;

using namespace std;

struct Edge {
  int i;
  int j;
  int w;
};

struct Node {
  int i;
  int side;
};

struct Queue {
  Queue() {
  }

  void clear() {
    begin_ = end_ = 0;
  }

  bool empty() {
    return begin_ == end_;
  }

  void put(Node node) {
    data_[end_] = std::move(node);
    inc(end_);
  }

  Node get() {
    assert (!empty());
    auto node = data_[begin_];
    inc(begin_);
    return node;
  }

  void inc(int &idx) {
    idx = (idx + 1) % MAXN;
  }

  int size() {
    return ((end_ + MAXN) - begin_) % MAXN;
  }

  int begin_ = 0;
  int end_ = 0;
  Node data_[MAXN];
};

class Hungarian {

  public:
    Hungarian() {
      for (int i = 0; i < MAXN; i++) {
        for (int j = 0; j < MAXN; j++) {
          adj_[S][i][j] = adj_[T][i][j] = 0;
        }
      }
    }

  void set_edges(std::vector<Edge> &edges, int n) {
    n_ = n;

    // Clear previous edges
    for (auto edge : edges_) {
      adj_[S][edge.i][edge.j] = adj_[T][edge.j][edge.i] = 0;
    }

    for (auto edge : edges) {
      adj_[S][edge.i][edge.j] = adj_[T][edge.j][edge.i] = edge.w;
    }
    edges_ = edges;
  }

  int solve() {
    for (int i = 0; i < n_; i++) {
      match_[S][i] = match_[T][i] = -1;
      v_[i] = 0;

      u_[i] = 0;
      for (int j = 0; j < n_; j++) {
        u_[i] = std::max(u_[i], adj_[S][i][j]);
      }
    }

    for (int t = 0; t < n_; t++) {
      expand_matching();
    }

    int s = 0;
    for (int i = 0; i < n_; i++) {
      s += u_[i] + v_[i];
    }
    return s;
  }

  void expand_matching() {
    candidates_.clear();
    for (int i = 0; i < n_; i++) {
      if (match_[S][i] == -1) {
        candidates_.put({.side = S, .i = i});
      }

      slack_[i] = INT_MAX;

      parent_[S][i] = parent_[T][i] = -1;

      visited_[S][i] = visited_[T][i] = false;

    }

    while (true) {
      bool augmented = visit_forest();
      if (augmented) {
        return;
      }
      change_duals();
    }
  }

  bool visit_forest() {
    while (!candidates_.empty()) {
      auto node = candidates_.get();
      auto side = node.side;
      auto c = node.i;

      if (visited_[side][c]) {
        continue;
      }

      visited_[side][c] = true;

      if (side == S) {
        visit_s(c);
      } else {
        bool augmented = visit_t(c);
        if (augmented) {
          return true;
        }
      }
    }

    return false;
  }

  void visit_s(int i) {
    for (int j = 0; j <n_; j++) {
      if (match_[S][i] == j) {
        continue;
      }

      int slack = u_[i] + v_[j] - adj_[S][i][j];
      if (slack < slack_[j]) {
        slack_[j] = slack;
        parent_[T][j] = i;

        // edge (i,j) is now in the forest. keep visiting
        if (slack == 0) {
          candidates_.put({.side = T, .i = j});
        }
      }
    }
  }

  bool visit_t(int j) {
    int i = match_[T][j];
    if (i == -1) {
      augment(j);
      return true;
    }

    parent_[S][i] = j;
    candidates_.put({.side = S, .i = i});
    return false;
  }

  void augment(int j) {
    while (j != -1) {
      int i = parent_[T][j];
      assert (i != -1);
      match_[T][j] = i;
      match_[S][i] = j;
      j = parent_[S][i];
    }
  }

  void change_duals() {
    bool added = false;
    int delta = INT_MAX;
    for (int i = 0; i < n_; i++) {
      if (slack_[i] > 0) {
        delta = std::min(delta, slack_[i]);
      }
    }

    for (int i = 0; i < n_; i++) {
      if (visited_[S][i]) {
        u_[i] -= delta;
      }

      if (visited_[T][i]) {
        v_[i] += delta;
      } else if (slack_[i] > 0) { // frontier edge
        slack_[i] -= delta;
        if (slack_[i] == 0) {
          added = true;
          candidates_.put({.side = T, .i = i});
        }
      }
    }
    assert (added);
  }

  private:
  int n_;
  std::vector<Edge> edges_;
  int adj_[2][MAXN][MAXN];
  int match_[2][MAXN];
  int parent_[2][MAXN];
  int u_[MAXN];
  int v_[MAXN];
  int slack_[MAXN];
  bool visited_[2][MAXN];
  Queue candidates_;
};

int main() {

  int to, from, t, cost;
  int n1, n2;

  scanf("%d",&t);

  Hungarian hungarian;

  while(t--){
    scanf("%d %d",&n1, &n2);

    int n = std::max(n1, n2);

    std::vector<Edge> edges;
    while(1){
      scanf("%d %d %d",&from,&to,&cost);
      if(from == 0 && to == 0) break;
      edges.push_back({
        .i = from - 1,
        .j = to - 1,
        .w = cost,
      });
    }
    hungarian.set_edges(edges, n);

    int r = hungarian.solve();
    printf("%d\n",r);
  }
  return 0;
}
