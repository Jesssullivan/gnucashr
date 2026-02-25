#pragma once
// Result<T, E> monadic error handling
// Mirrors gnucashr R package's ok()/err() pattern
//
// Uses std::optional internally (not std::variant) to handle:
// - T == E case (e.g. Result<string, string>)
// - Non-default-constructible T (e.g. Result<Book>)

#include <optional>
#include <string>
#include <stdexcept>
#include <utility>

namespace gnucash {

template<typename T, typename E = std::string>
class Result {
    std::optional<T> ok_value_;
    std::optional<E> err_value_;
    bool ok_;

public:
    // Constructors
    static Result ok(T val) {
        Result r;
        r.ok_value_.emplace(std::move(val));
        r.ok_ = true;
        return r;
    }

    static Result err(E error) {
        Result r;
        r.err_value_.emplace(std::move(error));
        r.ok_ = false;
        return r;
    }

    // Queries
    bool is_ok() const { return ok_; }
    bool is_err() const { return !ok_; }

    // Unwrap
    const T& unwrap() const {
        if (!ok_) throw std::runtime_error("unwrap() called on err Result");
        return *ok_value_;
    }

    T& unwrap() {
        if (!ok_) throw std::runtime_error("unwrap() called on err Result");
        return *ok_value_;
    }

    const E& unwrap_err() const {
        if (ok_) throw std::runtime_error("unwrap_err() called on ok Result");
        return *err_value_;
    }

    T unwrap_or(T default_val) const {
        if (ok_) return *ok_value_;
        return default_val;
    }

    // Functor: map over the ok value
    template<typename F>
    auto map(F&& f) const -> Result<decltype(f(std::declval<T>())), E> {
        using U = decltype(f(std::declval<T>()));
        if (ok_) return Result<U, E>::ok(f(*ok_value_));
        return Result<U, E>::err(*err_value_);
    }

    // Monad: bind (flatMap)
    template<typename F>
    auto bind(F&& f) const -> decltype(f(std::declval<T>())) {
        using R = decltype(f(std::declval<T>()));
        if (ok_) return f(*ok_value_);
        return R::err(*err_value_);
    }

    // Pattern matching
    template<typename OkFn, typename ErrFn>
    auto match(OkFn&& ok_fn, ErrFn&& err_fn) const
        -> decltype(ok_fn(std::declval<T>())) {
        if (ok_) return ok_fn(*ok_value_);
        return err_fn(*err_value_);
    }

private:
    Result() : ok_(false) {}
};

// Specialization for void ok type
template<typename E>
class Result<void, E> {
    std::optional<E> err_value_;
    bool ok_;

public:
    static Result ok() {
        Result r;
        r.ok_ = true;
        return r;
    }

    static Result err(E error) {
        Result r;
        r.err_value_.emplace(std::move(error));
        r.ok_ = false;
        return r;
    }

    bool is_ok() const { return ok_; }
    bool is_err() const { return !ok_; }

    void unwrap() const {
        if (!ok_) throw std::runtime_error("unwrap() called on err Result<void>");
    }

    const E& unwrap_err() const {
        if (ok_) throw std::runtime_error("unwrap_err() called on ok Result<void>");
        return *err_value_;
    }

private:
    Result() : ok_(false) {}
};

} // namespace gnucash
