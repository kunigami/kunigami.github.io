#include "gtest/gtest.h"

#include "shared_ptr.h"

namespace {

struct C {
  static int destroyed_cnt;
  C() {}
  C(int x): x(x) {}
  ~C() {
    destroyed_cnt++;
  }

  int x = 0;
};

int C::destroyed_cnt{0};

struct ListNode {
  static int destroyed_cnt;
  shared_ptr<ListNode> next;

  ~ListNode() {
    destroyed_cnt++;
  }
};

int ListNode::destroyed_cnt{0};


class SharedPtrTest : public ::testing::Test {
  protected:
    virtual void SetUp() {
      C::destroyed_cnt = 0;
      ListNode::destroyed_cnt = 0;
    }

    virtual void TearDown() {
    }
};

TEST_F(SharedPtrTest, NullPtr) {
  {
    shared_ptr<C> cp;
  }
  EXPECT_EQ(0, C::destroyed_cnt);
}

TEST_F(SharedPtrTest, CreateAndDestroy) {
  {
    auto cp = shared_ptr<C>::make(2);
  }
  EXPECT_EQ(1, C::destroyed_cnt);
}

TEST_F(SharedPtrTest, CreateMultipleAndDestroy) {
  {
    auto cp = shared_ptr<C>::make(2);
    {
      auto cp2 = cp;
    }
    EXPECT_EQ(0, C::destroyed_cnt);
  }
  EXPECT_EQ(1, C::destroyed_cnt);
}

TEST_F(SharedPtrTest, Move) {
  auto cp = shared_ptr<C>::make(2);
  shared_ptr<C> cp2(std::move(cp));

  EXPECT_EQ(nullptr, cp.get());
  EXPECT_EQ(2, cp2->x);
}

TEST_F(SharedPtrTest, Access) {
  auto cp = shared_ptr<C>::make(2);
  EXPECT_EQ(2, cp->x);
}

TEST_F(SharedPtrTest, Dereference) {
  auto cp = shared_ptr<C>::make(2);
  C c = *cp;
  EXPECT_EQ(2, c.x);
}

TEST_F(SharedPtrTest, Cycle) {
  {
    auto n1 = shared_ptr<ListNode>::make();
    auto n2 = shared_ptr<ListNode>::make();
    auto n3 = shared_ptr<ListNode>::make();
    n1->next = n2;
    n2->next = n3;
    n3->next = n1;
  }
  EXPECT_EQ(0, ListNode::destroyed_cnt);
}

TEST_F(SharedPtrTest, NoCycle) {
  {
    auto n1 = shared_ptr<ListNode>::make();
    auto n2 = shared_ptr<ListNode>::make();
    auto n3 = shared_ptr<ListNode>::make();
    n1->next = n2;
    n2->next = n3;
  }
  EXPECT_EQ(3, ListNode::destroyed_cnt);
}

}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
