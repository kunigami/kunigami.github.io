/**

   Basic coroutine library with lots of debugging, using symmetric transfer.

   Compile with:

   g++ -std=c++20 coro_lib.cpp

*/

#include <coroutine>
#include <iostream>
#include <stdexcept>
#include <thread>
#include <future>
#include <condition_variable>
#include <cassert>

using namespace std;

// forward declarations
struct Promise;
struct RootTask;

struct FinalAwaiter {
    using Handle = std::coroutine_handle<Promise>;

    bool await_ready() noexcept {
        return false;
    }

    std::coroutine_handle<> await_suspend(Handle h) noexcept;

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

    std::coroutine_handle<> await_suspend(std::coroutine_handle<> handler);

    std::string await_resume();

    std::string result();

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
    std::coroutine_handle<> caller;
    int id_ = -1;
    bool is_done = false;
};

// These methods from Task have to be implemented 
// after "Promise" is defined

std::coroutine_handle<> FinalAwaiter::await_suspend(Handle h) noexcept {
    h.promise().print("[FinalAwaiter] await_suspend");
    return h.promise().caller;
}

std::string Task::operator ()() { 
    handle_(); 
    return promise().get_value();
}

std::string Task::await_resume() {
    handle_.promise().print("[Task] await_resume");
    return promise().get_value();
}

std::coroutine_handle<> Task::await_suspend(std::coroutine_handle<> handler) {    
    handle_.promise().caller = handler;
    return handle_;
}


std::string Task::result() {
    return handle_.promise().get_value();
}


Task h() {
    std::cout << "starting h()" << std::endl;
    co_return "hello";
}


Task g() {
    std::cout << "starting g()" << std::endl;
    auto v1 = co_await h();
    std::cout << "v1: " << std::endl;
    auto v2 = co_await h();
    std::cout << "v2: " << std::endl;
    co_return v1 + v2;
}

Task f() {
    std::cout << "starting f()" << std::endl;
    auto r = co_await g();
    co_return r;
}

struct RootTaskPromise {
    
    std::suspend_never initial_suspend() {
		return{};
	}

    std::coroutine_handle<RootTaskPromise> get_return_object() {
        return {std::coroutine_handle<RootTaskPromise>::from_promise(*this)};
    }

    void unhandled_exception() noexcept {
	}


    void start(std::promise<void> &&p);

    void return_value(std::string result) {
        m_result = result;
    }
    
    std::suspend_always final_suspend() noexcept {
        return {};
	}

    std::string m_result;
    std::promise<void> m_p;
};

struct RootTask {
    using coroutine_handle_t = std::coroutine_handle<RootTaskPromise>;
    using promise_type = RootTaskPromise;

	RootTask(coroutine_handle_t coroutine)
		: m_coroutine(coroutine) {}

    void resume() {
        m_coroutine.resume();
    }

    std::string& result() {
		return m_coroutine.promise().m_result;
	}

    coroutine_handle_t m_coroutine;
};


void RootTaskPromise::start(std::promise<void> &&p) {
    m_p = std::move(p);
    get_return_object().resume();
}

RootTask yield(Task&& t) {
    std::string value = co_await t;
	co_return value;
}

std::string sync_wait(Task&& t) {
    RootTask t2 = yield(std::move(t));
    return t2.result();
}

int main() {
    Task t = g(); // initializing  
    auto x = sync_wait(std::move(t));
    std::cout << x << std::endl;
}
