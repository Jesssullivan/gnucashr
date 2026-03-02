#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include <gnucash/ofx.h>
#include <string>

using Catch::Approx;

// Sample OFX v1.x (SGML) content
static const std::string OFX_V1 = R"(
OFXHEADER:100
DATA:OFXSGML
VERSION:102
SECURITY:NONE
ENCODING:USASCII
CHARSET:1252
COMPRESSION:NONE
OLDFILEUID:NONE
NEWFILEUID:NONE

<OFX>
<SIGNONMSGSRSV1>
<SONRS>
<STATUS><CODE>0<SEVERITY>INFO</STATUS>
<DTSERVER>20240315120000
<LANGUAGE>ENG
<FI><ORG>Test Bank<FID>12345</FI>
</SONRS>
</SIGNONMSGSRSV1>
<BANKMSGSRSV1>
<STMTTRNRS>
<TRNUID>0
<STATUS><CODE>0<SEVERITY>INFO</STATUS>
<STMTRS>
<CURDEF>USD
<BANKACCTFROM>
<BANKID>123456789
<ACCTID>9876543210
<ACCTTYPE>CHECKING
</BANKACCTFROM>
<BANKTRANLIST>
<DTSTART>20240101
<DTEND>20240315
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240115
<TRNAMT>-42.50
<FITID>2024011501
<NAME>GROCERY STORE
<MEMO>Weekly groceries
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT
<DTPOSTED>20240201
<TRNAMT>3500.00
<FITID>2024020101
<NAME>EMPLOYER INC
<MEMO>Salary deposit
</STMTTRN>
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240210
<TRNAMT>-150.00
<FITID>2024021001
<NAME>ELECTRIC CO
</STMTTRN>
</BANKTRANLIST>
<LEDGERBAL>
<BALAMT>5307.50
<DTASOF>20240315
</LEDGERBAL>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
)";

// Sample OFX v2.x (XML) content
static const std::string OFX_V2 = R"(<?xml version="1.0" encoding="UTF-8"?>
<?OFX OFXHEADER="200" VERSION="220"?>
<OFX>
<SIGNONMSGSRSV1>
<SONRS>
<STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS>
<DTSERVER>20240315120000</DTSERVER>
<LANGUAGE>ENG</LANGUAGE>
</SONRS>
</SIGNONMSGSRSV1>
<BANKMSGSRSV1>
<STMTTRNRS>
<STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS>
<STMTRS>
<CURDEF>EUR</CURDEF>
<BANKACCTFROM>
<BANKID>TESTBANK</BANKID>
<ACCTID>DE12345678</ACCTID>
<ACCTTYPE>SAVINGS</ACCTTYPE>
</BANKACCTFROM>
<BANKTRANLIST>
<DTSTART>20240101</DTSTART>
<DTEND>20240301</DTEND>
<STMTTRN>
<TRNTYPE>DEBIT</TRNTYPE>
<DTPOSTED>20240115</DTPOSTED>
<TRNAMT>-25.99</TRNAMT>
<FITID>XML001</FITID>
<NAME>STREAMING SERVICE</NAME>
<MEMO>Monthly sub</MEMO>
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT</TRNTYPE>
<DTPOSTED>20240201</DTPOSTED>
<TRNAMT>1000.00</TRNAMT>
<FITID>XML002</FITID>
<NAME>TRANSFER IN</NAME>
</STMTTRN>
</BANKTRANLIST>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
)";

TEST_CASE("detect_ofx_version v1 SGML", "[ofx]") {
    REQUIRE(gnucash::detect_ofx_version(OFX_V1) == "1");
}

TEST_CASE("detect_ofx_version v2 XML", "[ofx]") {
    REQUIRE(gnucash::detect_ofx_version(OFX_V2) == "2");
}

TEST_CASE("detect_ofx_version unknown", "[ofx]") {
    REQUIRE(gnucash::detect_ofx_version("just some text") == "unknown");
}

TEST_CASE("parse_ofx v1 account info", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V1);
    REQUIRE(result.account.account_id == "9876543210");
    REQUIRE(result.account.bank_id == "123456789");
    REQUIRE(result.account.account_type == "CHECKING");
    REQUIRE(result.account.org_name == "Test Bank");
    REQUIRE(result.account.fid == "12345");
    REQUIRE(result.account.currency == "USD");
    REQUIRE(result.account.date_start == "2024-01-01");
    REQUIRE(result.account.date_end == "2024-03-15");
}

TEST_CASE("parse_ofx v1 transactions", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V1);
    REQUIRE(result.transactions.size() == 3);

    // First transaction
    REQUIRE(result.transactions[0].date == "2024-01-15");
    REQUIRE(result.transactions[0].amount == Approx(-42.50));
    REQUIRE(result.transactions[0].name == "GROCERY STORE");
    REQUIRE(result.transactions[0].fitid == "2024011501");
    REQUIRE(result.transactions[0].memo == "Weekly groceries");
    REQUIRE(result.transactions[0].trntype == "DEBIT");

    // Second transaction (salary)
    REQUIRE(result.transactions[1].amount == Approx(3500.00));
    REQUIRE(result.transactions[1].trntype == "CREDIT");

    // Third transaction (no memo)
    REQUIRE(result.transactions[2].name == "ELECTRIC CO");
    REQUIRE(result.transactions[2].amount == Approx(-150.00));
}

TEST_CASE("parse_ofx v2 XML", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V2);
    REQUIRE(result.account.account_id == "DE12345678");
    REQUIRE(result.account.bank_id == "TESTBANK");
    REQUIRE(result.account.account_type == "SAVINGS");
    REQUIRE(result.account.currency == "EUR");

    REQUIRE(result.transactions.size() == 2);
    REQUIRE(result.transactions[0].amount == Approx(-25.99));
    REQUIRE(result.transactions[0].name == "STREAMING SERVICE");
    REQUIRE(result.transactions[1].amount == Approx(1000.00));
}

TEST_CASE("parse_ofx empty content", "[ofx]") {
    auto result = gnucash::parse_ofx("");
    REQUIRE(result.transactions.empty());
    REQUIRE(result.account.currency == "USD"); // default
}

TEST_CASE("parse_ofx ledger balance", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V1);
    REQUIRE(result.account.ledger_balance == "5307.50");
}

TEST_CASE("parse_ofx amount_fraction v1", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V1);
    REQUIRE(result.transactions.size() == 3);

    // -42.50 -> Fraction(-4250, 100)
    REQUIRE(result.transactions[0].amount_fraction.num == -4250);
    REQUIRE(result.transactions[0].amount_fraction.denom == 100);

    // 3500.00 -> Fraction(350000, 100)
    REQUIRE(result.transactions[1].amount_fraction.num == 350000);
    REQUIRE(result.transactions[1].amount_fraction.denom == 100);

    // -150.00 -> Fraction(-15000, 100)
    REQUIRE(result.transactions[2].amount_fraction.num == -15000);
    REQUIRE(result.transactions[2].amount_fraction.denom == 100);
}

TEST_CASE("parse_ofx amount_fraction v2", "[ofx]") {
    auto result = gnucash::parse_ofx(OFX_V2);
    REQUIRE(result.transactions.size() == 2);

    // -25.99
    REQUIRE(result.transactions[0].amount_fraction.num == -2599);
    REQUIRE(result.transactions[0].amount_fraction.denom == 100);

    // 1000.00
    REQUIRE(result.transactions[1].amount_fraction.num == 100000);
    REQUIRE(result.transactions[1].amount_fraction.denom == 100);
}
