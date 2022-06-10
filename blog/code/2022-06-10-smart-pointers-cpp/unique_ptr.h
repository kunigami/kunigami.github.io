#include <iostream>

template <typename T>
class unique_ptr {
  public:

  unique_ptr() {
  }

  // Allows transfering ownership
  unique_ptr(unique_ptr<T> &&ptr) {
    std::swap(ptr.raw_ptr_, raw_ptr_);
  }

  template<typename... Args>
  unique_ptr<T> static make(Args&&... args) {
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
  }

  // Disable copy constructor since we only allow
  // one reference.
  unique_ptr(const unique_ptr<T> &ptr) = delete;

  ~unique_ptr() {
    reset();
  }

  unique_ptr<T>& operator= (const unique_ptr<T> &other) = delete;

  unique_ptr<T>& operator= (unique_ptr<T> &&other) {
    reset(other.release());
    return *this;
  }

  const T* get() { return raw_ptr_; }

  void reset(T* raw_ptr = nullptr) {
    if (raw_ptr_) {
      delete raw_ptr_;
    }
    raw_ptr_ = raw_ptr;
  }

  T* release() {
    auto raw_ptr = raw_ptr_;
    raw_ptr_ = nullptr;
    return raw_ptr;
  }

  // Pointer syntax
  T* operator->() { return raw_ptr_; }
  T& operator*() { return *raw_ptr_; }

  private:

  explicit unique_ptr(T* raw_ptr) : raw_ptr_(raw_ptr) {
    std::cout << "raw_ptr" << std::endl;
  }

  T* raw_ptr_ = nullptr;
};
