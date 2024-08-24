/**

   Basic coroutine library with lots of debugging.

   Compile with:

   g++ -std=c++20 coro_lib.cpp
 */
// 
// 

#include <coroutine>
#include <iostream>
#include <stdexcept>
#include <thread>
 
using namespace std;

// forward declaration
struct Promise;

struct FinalAwaiter {
    using Handle = std::coroutine_handle<Promise>;

    bool await_ready() noexcept {
        return false;
    }

    void await_suspend(Handle h) noexcept;

    void await_resume() noexcept {}
};

struct Task {
    using promise_type = Promise;
    using Handle = std::coroutine_handle<Promise>;

    Task(Handle handle) : handle_(handle) {}

    static Task from_promise(Promise& promise) {
        return Task(Handle::from_promise(promise));
    }

    std::string operator ()();

    Promise& promise() const {
        return handle_.promise();
    }

    bool await_ready() { return false; }

    void await_suspend(Task handler);

    std::string await_resume();

    void resume() {
        handle_.resume();
    }

    Handle handle_;
};

int promiseCount = 0;

struct Promise {
    Promise() {
        id_ = promiseCount++;
        print("construct");
    }

    void print(std::string msg) {
        std::cout << "[" << id_ << "] " << msg << std::endl;
    }

    Task get_return_object() {
        return {Task::from_promise(*this)};
    }

    std::suspend_always initial_suspend() { return {}; }

    FinalAwaiter final_suspend() noexcept { 
        print("final_suspend, FinalAwaiter");        
        return FinalAwaiter{}; 
    }

    void unhandled_exception() {}
    
    void return_value(std::string value_) {
        print("return_value: " + value_);
        is_done = true;
        value = value_;
    }
    
    std::string get_value() {
        return value;
    }

    std::suspend_always yield_value(std::string value_) {
        value = value_;
        return {};
    }

    int get_id() {
        return id_;
    }

    std::string value;
    std::optional<Task> caller;
    int id_ = -1;
    bool is_done = false;
};

// These methods from Task have to be implemented 
// after "Promise" is defined

void FinalAwaiter::await_suspend(Handle h) noexcept {
    h.promise().print("[FinalAwaiter] await_suspend");
    if (h.promise().caller) {     
        h.promise().print(
            "[FinalAwaiter] resuming task " + std::to_string(h.promise().caller->handle_.promise().get_id())
        );   
        h.promise().caller->handle_.resume();
    }
}

std::string Task::operator ()() { 
    handle_(); 
    return promise().get_value();
}

std::string Task::await_resume() {
    handle_.promise().print("[Task] await_resume");
    return promise().get_value();
}

void Task::await_suspend(Task handler) {    
    handle_.promise().print(
        "[Task] await_suspend. next task is " + 
        std::to_string(handler.handle_.promise().get_id())
    );

    handle_.promise().caller = handler;
    handle_.resume();
    handle_.promise().print("finished resuming...");
}

Task h() {
    std::cout << "starting h()" << std::endl;
    co_return "hello";
}

Task g() {
    std::cout << "starting g()" << std::endl;
    auto v1 = co_await h();
    std::cout << "v1: " << v1 << std::endl;
    auto v2 = co_await h();
    std::cout << "v2: " << v2 << std::endl;
    co_return v1 + v2;
}

Task f() {
    auto r = co_await g();
    co_return r;
}

int main() {
    Task h = f(); // initializing   
    std::cout << h() << std::endl;
}
