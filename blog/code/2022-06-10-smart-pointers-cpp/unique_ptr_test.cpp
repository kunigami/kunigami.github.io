#include "gtest/gtest.h"

#include "unique_ptr.h"

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

class UniquePtrTest : public ::testing::Test {
  protected:
    virtual void SetUp() {
      C::destroyed_cnt = 0;
    }

    virtual void TearDown() {
    }
};

TEST_F(UniquePtrTest, NullPtr) {
  {
    unique_ptr<C> cp;
  }
  EXPECT_EQ(0, C::destroyed_cnt);
}

TEST_F(UniquePtrTest, CreateAndDestroy) {
  {
    auto cp = unique_ptr<C>::make(2);
  }
  EXPECT_EQ(1, C::destroyed_cnt);
}

TEST_F(UniquePtrTest, Move) {
  auto cp = unique_ptr<C>::make(2);
  unique_ptr<C> cp2(std::move(cp));
  std::cout << "assigned" << std::endl;
  EXPECT_EQ(nullptr, cp.get());
  EXPECT_EQ(2, cp2->x);
}

TEST_F(UniquePtrTest, Access) {
  auto cp = unique_ptr<C>::make(2);
  EXPECT_EQ(2, cp->x);
}

TEST_F(UniquePtrTest, Assignment) {
  auto cp = unique_ptr<C>::make(2);
  cp = unique_ptr<C>::make(3);
  EXPECT_EQ(1, C::destroyed_cnt);
}

TEST_F(UniquePtrTest, ManualMove) {
  auto cp = unique_ptr<C>::make(2);
  auto cp2 = unique_ptr<C>::make(3);
  cp = std::move(cp2);
  EXPECT_EQ(nullptr, cp2.get());
  EXPECT_EQ(1, C::destroyed_cnt);
}

TEST_F(UniquePtrTest, Dereference) {
  auto cp = unique_ptr<C>::make(2);
  C c = *cp;
  EXPECT_EQ(2, c.x);
}

TEST_F(UniquePtrTest, Release) {
  auto cp = unique_ptr<C>::make(2);
  C* c = cp.release();
  EXPECT_EQ(2, c->x);
  EXPECT_EQ(nullptr, cp.get());
}

}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
