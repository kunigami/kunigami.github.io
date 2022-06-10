#include <algorithm>

template <typename T>
class shared_ptr {
  public:

  shared_ptr() : shared_ptr(nullptr) {
  }

  template<typename... Args>
  shared_ptr<T> static make(Args&&... args) {
    return shared_ptr<T>(new T(std::forward<Args>(args)...));
  }

  // Allows transfering ownership
  shared_ptr(shared_ptr<T> &&ptr) {
    ref_count_ = ptr.ref_count_;
    raw_ptr_ = ptr.raw_ptr_;
    ptr.raw_ptr_ = nullptr;
    ptr.ref_count_ = nullptr;
  }

  // Share reference count
  shared_ptr(const shared_ptr<T> &ptr) {
    ref_count_ = ptr.ref_count_;
    ++(*ref_count_);
  }

  ~shared_ptr() {
    clear();
  }

  const T* get() { return raw_ptr_; }

  shared_ptr<T>& operator= (const shared_ptr<T> &other) {
    clear();
    raw_ptr_ = other.raw_ptr_;
    ref_count_ = other.ref_count_;
    ++(*ref_count_);
    return *this;
  }

  shared_ptr<T>& operator= (const shared_ptr<T> &&other) {
    clear();
    raw_ptr_ = other.raw_ptr_;
    ref_count_ = other.ref_count_;
    other.raw_ptr_ = null;
    return *this;
  }

  // Pointer syntax
  T* operator->() { return raw_ptr_; }
  T& operator*() { return *raw_ptr_; }

  private:

    explicit shared_ptr(T* raw_ptr) : raw_ptr_(raw_ptr) {
      ref_count_ = new int(0);
      ++(*ref_count_);
    }

    void clear() {
      // Cleaned up already
      if (ref_count_ == nullptr && raw_ptr_ == nullptr) {
        return;
      }

      --(*ref_count_);
      if (*ref_count_ == 0) {
        delete raw_ptr_;
        delete ref_count_;
      }
    }

    T* raw_ptr_;
    int* ref_count_;
};
