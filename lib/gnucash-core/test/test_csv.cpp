#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/csv.h"
#include "gnucash/fraction.h"

using namespace gnucash;
using Catch::Matchers::WithinAbs;

// --- Fraction::from_string ---

TEST_CASE("Fraction::from_string basic amounts", "[fraction][csv]") {
    SECTION("positive integer") {
        auto f = Fraction::from_string("45");
        REQUIRE(f.num == 4500);
        REQUIRE(f.denom == 100);
    }
    SECTION("positive decimal") {
        auto f = Fraction::from_string("45.23");
        REQUIRE(f.num == 4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("negative decimal") {
        auto f = Fraction::from_string("-45.23");
        REQUIRE(f.num == -4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("dollar sign") {
        auto f = Fraction::from_string("$45.23");
        REQUIRE(f.num == 4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("negative dollar sign") {
        auto f = Fraction::from_string("$-45.23");
        REQUIRE(f.num == -4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("dollar sign before negative") {
        auto f = Fraction::from_string("-$45.23");
        REQUIRE(f.num == -4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("thousands separator") {
        auto f = Fraction::from_string("1,234.56");
        REQUIRE(f.num == 123456);
        REQUIRE(f.denom == 100);
    }
    SECTION("accounting negative (parens)") {
        auto f = Fraction::from_string("(45.23)");
        REQUIRE(f.num == -4523);
        REQUIRE(f.denom == 100);
    }
    SECTION("empty string") {
        auto f = Fraction::from_string("");
        REQUIRE(f.num == 0);
        REQUIRE(f.denom == 100);
    }
    SECTION("three decimal places") {
        auto f = Fraction::from_string("1.234");
        REQUIRE(f.num == 1234);
        REQUIRE(f.denom == 1000);
    }
    SECTION("zero") {
        auto f = Fraction::from_string("0.00");
        REQUIRE(f.num == 0);
        REQUIRE(f.denom == 100);
    }
}

// --- parse_csv_line ---

TEST_CASE("parse_csv_line basic", "[csv]") {
    auto fields = parse_csv_line("a,b,c");
    REQUIRE(fields.size() == 3);
    REQUIRE(fields[0] == "a");
    REQUIRE(fields[1] == "b");
    REQUIRE(fields[2] == "c");
}

TEST_CASE("parse_csv_line quoted fields", "[csv]") {
    auto fields = parse_csv_line("\"hello, world\",b,c");
    REQUIRE(fields.size() == 3);
    REQUIRE(fields[0] == "hello, world");
}

TEST_CASE("parse_csv_line escaped quotes", "[csv]") {
    auto fields = parse_csv_line("\"he said \"\"hello\"\"\",b");
    REQUIRE(fields.size() == 2);
    REQUIRE(fields[0] == "he said \"hello\"");
}

TEST_CASE("parse_csv_line empty fields", "[csv]") {
    auto fields = parse_csv_line(",a,,b,");
    REQUIRE(fields.size() == 5);
    REQUIRE(fields[0] == "");
    REQUIRE(fields[1] == "a");
    REQUIRE(fields[2] == "");
    REQUIRE(fields[3] == "b");
    REQUIRE(fields[4] == "");
}

// --- normalize_date ---

TEST_CASE("normalize_date ISO format", "[csv]") {
    REQUIRE(normalize_date("2024-01-15", "YYYY-MM-DD") == "2024-01-15");
    REQUIRE(normalize_date("2024-01-15T12:00:00", "YYYY-MM-DD") == "2024-01-15");
}

TEST_CASE("normalize_date MM/DD/YYYY", "[csv]") {
    REQUIRE(normalize_date("01/15/2024", "MM/DD/YYYY") == "2024-01-15");
    REQUIRE(normalize_date("12/31/2023", "MM/DD/YYYY") == "2023-12-31");
}

TEST_CASE("normalize_date MM/DD/YY", "[csv]") {
    REQUIRE(normalize_date("01/15/24", "MM/DD/YY") == "2024-01-15");
}

// --- detect_csv_format ---

TEST_CASE("detect_csv_format PayPal", "[csv]") {
    auto f = detect_csv_format("\"Date\",\"Time\",\"TimeZone\",\"Name\",\"Type\",\"Status\",\"Currency\",\"Gross\",\"Fee\",\"Net\",\"From Email Address\",\"To Email Address\",\"Transaction ID\"");
    REQUIRE(f.has_value());
    REQUIRE(f->name == "paypal");
}

TEST_CASE("detect_csv_format Stripe", "[csv]") {
    auto f = detect_csv_format("id,Created (UTC),Amount,Currency,Description");
    REQUIRE(f.has_value());
    REQUIRE(f->name == "stripe");
}

TEST_CASE("detect_csv_format Venmo", "[csv]") {
    auto f = detect_csv_format(",ID,Datetime,Type,Status,Note,From,To,Amount (total)");
    REQUIRE(f.has_value());
    REQUIRE(f->name == "venmo");
}

TEST_CASE("detect_csv_format Apple Card", "[csv]") {
    auto f = detect_csv_format("Transaction Date,Clearing Date,Description,Merchant,Category,Type,Amount (USD)");
    REQUIRE(f.has_value());
    REQUIRE(f->name == "apple_card");
}

TEST_CASE("detect_csv_format unknown", "[csv]") {
    auto f = detect_csv_format("column1,column2,column3");
    REQUIRE_FALSE(f.has_value());
}

// --- parse_csv ---

TEST_CASE("parse_csv generic format", "[csv]") {
    std::string csv =
        "date,amount,description\n"
        "2024-01-15,-45.23,Grocery Store\n"
        "2024-01-16,1000.00,Paycheck\n";

    auto result = parse_csv(csv, generic_format());
    REQUIRE(result.is_ok());
    auto& txns = result.unwrap();
    REQUIRE(txns.size() == 2);

    REQUIRE(txns[0].date == "2024-01-15");
    REQUIRE(txns[0].amount.num == -4523);
    REQUIRE(txns[0].amount.denom == 100);
    REQUIRE(txns[0].description == "Grocery Store");

    REQUIRE(txns[1].date == "2024-01-16");
    REQUIRE(txns[1].amount.num == 100000);
    REQUIRE(txns[1].amount.denom == 100);
}

TEST_CASE("parse_csv handles Windows line endings", "[csv]") {
    std::string csv = "date,amount,description\r\n2024-01-15,-10.00,Test\r\n";

    auto result = parse_csv(csv, generic_format());
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().size() == 1);
}

TEST_CASE("parse_csv skips empty lines", "[csv]") {
    std::string csv =
        "date,amount,description\n"
        "\n"
        "2024-01-15,-10.00,Test\n"
        "\n";

    auto result = parse_csv(csv, generic_format());
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().size() == 1);
}

TEST_CASE("parse_csv with quoted commas in description", "[csv]") {
    std::string csv =
        "date,amount,description\n"
        "2024-01-15,-25.00,\"Coffee, Tea & More\"\n";

    auto result = parse_csv(csv, generic_format());
    REQUIRE(result.is_ok());
    auto& txns = result.unwrap();
    REQUIRE(txns.size() == 1);
    REQUIRE(txns[0].description == "Coffee, Tea & More");
}

TEST_CASE("parse_csv error on all malformed lines", "[csv]") {
    std::string csv = "date,amount,description\nmalformed\nbad\n";

    auto result = parse_csv(csv, generic_format());
    REQUIRE(result.is_err());
}

TEST_CASE("parse_csv negate_amount flag", "[csv]") {
    CsvFormat f = generic_format();
    f.negate_amount = true;

    std::string csv = "date,amount,description\n2024-01-15,45.23,Test\n";

    auto result = parse_csv(csv, f);
    REQUIRE(result.is_ok());
    auto& txns = result.unwrap();
    REQUIRE(txns[0].amount.num == -4523);
}
