// Compile with
// g++ -std=c++20 coroutine.cpp

#include <coroutine>
#include <iostream>
#include <stdexcept>
#include <thread>
 
using namespace std;

// forward declaration
struct Promise;

struct Task {
    using promise_type = Promise;
    using Handle = std::coroutine_handle<Promise>;

    Task(Handle handle) : _handle(handle) {}

    static Task from_promise(Promise& promise) {
        return Task(Handle::from_promise(promise));
    }

    std::string operator ()();

    Promise& promise() const {
        return _handle.promise();
    }

    bool await_ready() { return false; }
    
    void await_suspend(Task handler) { }

    std::string await_resume();

    Handle _handle;
};

struct Promise {
    Task get_return_object() {
        return {Task::from_promise(*this)};
    }

    std::suspend_never initial_suspend() { return {}; }

    std::suspend_always final_suspend() noexcept { 
        return {}; 
    }

    void unhandled_exception() {}
    
    void return_value(std::string value_) {
        value = value_;
    }
    
    std::string get_value() {
        return value;
    }

    std::suspend_always yield_value(std::string value_) {
        value = value_;
        return {};
    }

    private:
        std::string value;
};

// These methods from Task have to be implemented 
// after "Promise" is defined

std::string Task::operator ()() { 
    _handle(); 
    return promise().get_value();
}

std::string Task::await_resume() {
    return promise().get_value();
}

Task g() {
    co_return "hello";
}

Task f() {
    auto v = co_await g();
    co_return v + " world";
}

int main() {
    Task h = f(); // initializing   
    std::cout << h() << std::endl;
}
