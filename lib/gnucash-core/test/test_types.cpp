#include <catch2/catch_test_macros.hpp>
#include "gnucash/types.h"

using namespace gnucash;

TEST_CASE("parse_account_type basic", "[types]") {
    REQUIRE(parse_account_type("BANK") == AccountType::BANK);
    REQUIRE(parse_account_type("CASH") == AccountType::CASH);
    REQUIRE(parse_account_type("CREDIT") == AccountType::CREDIT);
    REQUIRE(parse_account_type("ASSET") == AccountType::ASSET);
    REQUIRE(parse_account_type("LIABILITY") == AccountType::LIABILITY);
    REQUIRE(parse_account_type("STOCK") == AccountType::STOCK);
    REQUIRE(parse_account_type("MUTUAL") == AccountType::MUTUAL);
    REQUIRE(parse_account_type("CURRENCY") == AccountType::CURRENCY);
    REQUIRE(parse_account_type("INCOME") == AccountType::INCOME);
    REQUIRE(parse_account_type("EXPENSE") == AccountType::EXPENSE);
    REQUIRE(parse_account_type("EQUITY") == AccountType::EQUITY);
    REQUIRE(parse_account_type("RECEIVABLE") == AccountType::RECEIVABLE);
    REQUIRE(parse_account_type("PAYABLE") == AccountType::PAYABLE);
    REQUIRE(parse_account_type("ROOT") == AccountType::ROOT);
    REQUIRE(parse_account_type("TRADING") == AccountType::TRADING);
    REQUIRE(parse_account_type("NONE") == AccountType::NONE);
}

TEST_CASE("parse_account_type case insensitive", "[types]") {
    REQUIRE(parse_account_type("bank") == AccountType::BANK);
    REQUIRE(parse_account_type("Bank") == AccountType::BANK);
    REQUIRE(parse_account_type("bAnK") == AccountType::BANK);
}

TEST_CASE("parse_account_type unknown returns NONE", "[types]") {
    REQUIRE(parse_account_type("INVALID") == AccountType::NONE);
    REQUIRE(parse_account_type("") == AccountType::NONE);
}

TEST_CASE("account_type_to_string", "[types]") {
    REQUIRE(account_type_to_string(AccountType::BANK) == "BANK");
    REQUIRE(account_type_to_string(AccountType::ROOT) == "ROOT");
    REQUIRE(account_type_to_string(AccountType::EXPENSE) == "EXPENSE");
    REQUIRE(account_type_to_string(AccountType::NONE) == "NONE");
}

TEST_CASE("account_type roundtrip", "[types]") {
    for (auto type : {AccountType::BANK, AccountType::CASH, AccountType::CREDIT,
                       AccountType::ASSET, AccountType::LIABILITY, AccountType::INCOME,
                       AccountType::EXPENSE, AccountType::EQUITY, AccountType::ROOT}) {
        auto str = account_type_to_string(type);
        auto parsed = parse_account_type(str);
        REQUIRE(parsed == type);
    }
}
