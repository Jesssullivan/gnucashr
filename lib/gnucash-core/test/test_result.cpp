#include <catch2/catch_test_macros.hpp>
#include "gnucash/result.h"

using namespace gnucash;

TEST_CASE("Result<T> ok construction", "[result]") {
    auto r = Result<int>::ok(42);
    REQUIRE(r.is_ok());
    REQUIRE_FALSE(r.is_err());
    REQUIRE(r.unwrap() == 42);
}

TEST_CASE("Result<T> err construction", "[result]") {
    auto r = Result<int>::err("bad input");
    REQUIRE(r.is_err());
    REQUIRE_FALSE(r.is_ok());
    REQUIRE(r.unwrap_err() == "bad input");
}

TEST_CASE("Result<T> unwrap throws on err", "[result]") {
    auto r = Result<int>::err("oops");
    REQUIRE_THROWS_AS(r.unwrap(), std::runtime_error);
}

TEST_CASE("Result<T> unwrap_err throws on ok", "[result]") {
    auto r = Result<int>::ok(1);
    REQUIRE_THROWS_AS(r.unwrap_err(), std::runtime_error);
}

TEST_CASE("Result<T> unwrap_or", "[result]") {
    SECTION("returns value on ok") {
        auto r = Result<int>::ok(42);
        REQUIRE(r.unwrap_or(0) == 42);
    }
    SECTION("returns default on err") {
        auto r = Result<int>::err("fail");
        REQUIRE(r.unwrap_or(-1) == -1);
    }
}

TEST_CASE("Result<T> map", "[result]") {
    SECTION("transforms ok value") {
        auto r = Result<int>::ok(21);
        auto doubled = r.map([](int x) { return x * 2; });
        REQUIRE(doubled.is_ok());
        REQUIRE(doubled.unwrap() == 42);
    }
    SECTION("passes through err") {
        auto r = Result<int>::err("fail");
        auto doubled = r.map([](int x) { return x * 2; });
        REQUIRE(doubled.is_err());
        REQUIRE(doubled.unwrap_err() == "fail");
    }
}

TEST_CASE("Result<T> bind (flatMap)", "[result]") {
    auto safe_div = [](int x) -> Result<double> {
        if (x == 0) return Result<double>::err("division by zero");
        return Result<double>::ok(100.0 / x);
    };

    SECTION("chains ok values") {
        auto r = Result<int>::ok(4);
        auto result = r.bind(safe_div);
        REQUIRE(result.is_ok());
        REQUIRE(result.unwrap() == 25.0);
    }
    SECTION("short-circuits on err") {
        auto r = Result<int>::err("no value");
        auto result = r.bind(safe_div);
        REQUIRE(result.is_err());
        REQUIRE(result.unwrap_err() == "no value");
    }
    SECTION("propagates function err") {
        auto r = Result<int>::ok(0);
        auto result = r.bind(safe_div);
        REQUIRE(result.is_err());
        REQUIRE(result.unwrap_err() == "division by zero");
    }
}

TEST_CASE("Result<T> match", "[result]") {
    auto r_ok = Result<int>::ok(42);
    auto r_err = Result<int>::err("bad");

    auto ok_result = r_ok.match(
        [](int x) { return std::to_string(x); },
        [](const std::string& e) { return std::string("error: ") + e; }
    );
    REQUIRE(ok_result == "42");

    auto err_result = r_err.match(
        [](int x) { return std::to_string(x); },
        [](const std::string& e) { return std::string("error: ") + e; }
    );
    REQUIRE(err_result == "error: bad");
}

TEST_CASE("Result<void> ok", "[result]") {
    auto r = Result<void>::ok();
    REQUIRE(r.is_ok());
    REQUIRE_FALSE(r.is_err());
    REQUIRE_NOTHROW(r.unwrap());
}

TEST_CASE("Result<void> err", "[result]") {
    auto r = Result<void>::err("failed");
    REQUIRE(r.is_err());
    REQUIRE(r.unwrap_err() == "failed");
    REQUIRE_THROWS_AS(r.unwrap(), std::runtime_error);
}

TEST_CASE("Result<T> with string value", "[result]") {
    auto r = Result<std::string>::ok("hello");
    REQUIRE(r.is_ok());
    REQUIRE(r.unwrap() == "hello");
}

TEST_CASE("Result<T> mutable unwrap", "[result]") {
    auto r = Result<std::vector<int>>::ok({1, 2, 3});
    REQUIRE(r.is_ok());
    r.unwrap().push_back(4);
    REQUIRE(r.unwrap().size() == 4);
}
