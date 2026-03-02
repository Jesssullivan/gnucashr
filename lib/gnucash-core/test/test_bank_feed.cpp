#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/bank_feed.h"
#include "gnucash/book.h"
#include "gnucash/ofx.h"
#include "gnucash/csv.h"
#include "gnucash/slots.h"
#include "gnucash/guid.h"
#include <filesystem>

using namespace gnucash;
using Catch::Matchers::WithinAbs;
namespace fs = std::filesystem;

#ifndef FIXTURE_DIR
#error "FIXTURE_DIR must be defined"
#endif

static const std::string ACCOUNTS_DB = std::string(FIXTURE_DIR) + "/with-accounts.gnucash";

static std::string make_writable_copy(const std::string& src) {
    auto tmp = fs::temp_directory_path() / ("bankfeed_test_" + generate_guid() + ".gnucash");
    fs::copy_file(src, tmp, fs::copy_options::overwrite_existing);
    return tmp.string();
}

static std::string get_usd_guid(Book& book) {
    auto commodities = book.get_commodities();
    return commodities.empty() ? "" : commodities[0].guid;
}

// Well-known account GUIDs from with-accounts.gnucash
static const std::string CHECKING = "a1000000000000000000000000000008";
static const std::string GROCERIES = "a1000000000000000000000000000017";
static const std::string IMBALANCE = "a1000000000000000000000000000019"; // Imbalance-USD

// Create Imbalance-USD account if it doesn't exist
static void ensure_imbalance(Book& book, const std::string& usd_guid) {
    auto acct = book.get_account(IMBALANCE);
    if (!acct) {
        // Create it under root
        book.create_account("Imbalance-USD", AccountType::BANK,
                           book.root_account_guid(), "", "", false, false);
    }
}

// OFX sample content for testing
static const std::string SAMPLE_OFX = R"(
OFXHEADER:100
DATA:OFXSGML
VERSION:102
<OFX>
<BANKMSGSRSV1>
<STMTTRNRS>
<STMTRS>
<CURDEF>USD
<BANKACCTFROM>
<BANKID>123456789
<ACCTID>987654321
<ACCTTYPE>CHECKING
</BANKACCTFROM>
<BANKTRANLIST>
<DTSTART>20240101
<DTEND>20240131
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240115
<TRNAMT>-42.50
<FITID>FIT001
<NAME>GROCERY STORE
<MEMO>Weekly groceries
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT
<DTPOSTED>20240120
<TRNAMT>3500.00
<FITID>FIT002
<NAME>PAYROLL DEPOSIT
</STMTTRN>
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240125
<TRNAMT>-150.00
<FITID>FIT003
<NAME>ELECTRIC CO
</STMTTRN>
</BANKTRANLIST>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
)";

// --- OFX Import ---

TEST_CASE("import_ofx basic", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto usd = get_usd_guid(book);

    // Create imbalance account
    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    REQUIRE(imb_r.is_ok());
    std::string imbalance_guid = imb_r.unwrap();

    auto result = import_ofx(book, SAMPLE_OFX, CHECKING, imbalance_guid);
    REQUIRE(result.is_ok());

    auto& r = result.unwrap();
    REQUIRE(r.total_parsed == 3);
    REQUIRE(r.imported == 3);
    REQUIRE(r.duplicates == 0);
    REQUIRE(r.errors == 0);
    REQUIRE(r.imported_guids.size() == 3);

    // Verify transactions exist
    for (const auto& guid : r.imported_guids) {
        auto tx = book.get_transaction(guid);
        REQUIRE(tx.has_value());
        REQUIRE(tx->splits.size() == 2);
    }

    // Verify Fraction amounts (no double precision loss)
    auto splits = book.get_splits_for_account(CHECKING);
    REQUIRE(splits.size() == 3);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_ofx dedup by FITID", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    REQUIRE(imb_r.is_ok());

    // First import
    auto r1 = import_ofx(book, SAMPLE_OFX, CHECKING, imb_r.unwrap());
    REQUIRE(r1.is_ok());
    REQUIRE(r1.unwrap().imported == 3);

    // Second import of same data
    auto r2 = import_ofx(book, SAMPLE_OFX, CHECKING, imb_r.unwrap());
    REQUIRE(r2.is_ok());
    REQUIRE(r2.unwrap().imported == 0);
    REQUIRE(r2.unwrap().duplicates == 3);

    // Total splits should still be 3
    auto splits = book.get_splits_for_account(CHECKING);
    REQUIRE(splits.size() == 3);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_ofx Fraction amounts", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    auto r = import_ofx(book, SAMPLE_OFX, CHECKING, imb_r.unwrap());
    REQUIRE(r.is_ok());

    auto splits = book.get_splits_for_account(CHECKING);
    // Find the -42.50 split
    bool found_grocery = false;
    for (const auto& s : splits) {
        if (s.value.num == -4250 && s.value.denom == 100) {
            found_grocery = true;
            break;
        }
    }
    REQUIRE(found_grocery);

    book.close();
    fs::remove(tmp);
}

// --- CSV Import ---

static const std::string SAMPLE_CSV =
    "date,amount,description\n"
    "2024-02-01,-25.00,Coffee Shop\n"
    "2024-02-03,500.00,Transfer In\n"
    "2024-02-05,-99.99,Online Store\n";

TEST_CASE("import_csv basic", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());

    auto result = import_csv(book, SAMPLE_CSV, generic_format(), CHECKING, imb_r.unwrap());
    REQUIRE(result.is_ok());

    auto& r = result.unwrap();
    REQUIRE(r.total_parsed == 3);
    REQUIRE(r.imported == 3);
    REQUIRE(r.duplicates == 0);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_csv dedup with synthetic FITID", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    auto imb_guid = imb_r.unwrap();

    // First import
    import_csv(book, SAMPLE_CSV, generic_format(), CHECKING, imb_guid);

    // Second import of same data
    auto r2 = import_csv(book, SAMPLE_CSV, generic_format(), CHECKING, imb_guid);
    REQUIRE(r2.is_ok());
    REQUIRE(r2.unwrap().imported == 0);
    REQUIRE(r2.unwrap().duplicates == 3);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_csv with explicit IDs", "[bank_feed]") {
    std::string csv =
        "date,amount,description,id\n"
        "2024-03-01,-10.00,Test,TXN001\n"
        "2024-03-02,-20.00,Test2,TXN002\n";

    CsvFormat f = generic_format();
    f.id_col = 3;

    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    auto imb_guid = imb_r.unwrap();

    auto result = import_csv(book, csv, f, CHECKING, imb_guid);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().imported == 2);

    // Verify FITIDs stored
    ensure_slots_table(book.raw_db());
    auto found = find_split_by_fitid(book.raw_db(), CHECKING, "TXN001");
    REQUIRE(found.has_value());

    book.close();
    fs::remove(tmp);
}

// --- check_duplicates ---

TEST_CASE("check_duplicates batch", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    import_ofx(book, SAMPLE_OFX, CHECKING, imb_r.unwrap());

    auto dupes = check_duplicates(book, CHECKING,
        {"FIT001", "FIT002", "FIT999", "NEW001"});

    REQUIRE(dupes.size() == 2);
    REQUIRE(dupes.count("FIT001") == 1);
    REQUIRE(dupes.count("FIT002") == 1);
    REQUIRE(dupes.count("FIT999") == 0);

    book.close();
    fs::remove(tmp);
}

// --- Import amounts precision ---

TEST_CASE("import_ofx preserves exact amounts", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());
    import_ofx(book, SAMPLE_OFX, CHECKING, imb_r.unwrap());

    // Checking balance should be: -42.50 + 3500.00 + (-150.00) = 3307.50
    double bal = book.get_account_balance(CHECKING);
    REQUIRE_THAT(bal, WithinAbs(3307.50, 1e-9));

    book.close();
    fs::remove(tmp);
}

// --- Edge cases ---

TEST_CASE("import_ofx empty content", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());

    auto result = import_ofx(book, "", CHECKING, imb_r.unwrap());
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().total_parsed == 0);
    REQUIRE(result.unwrap().imported == 0);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_csv empty content", "[bank_feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());

    auto result = import_csv(book, "date,amount,description\n", generic_format(),
                              CHECKING, imb_r.unwrap());
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().total_parsed == 0);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("import_csv with MM/DD/YYYY dates", "[bank_feed]") {
    std::string csv =
        "date,amount,description\n"
        "01/15/2024,-30.00,Test\n";

    CsvFormat f = generic_format();
    f.date_format = "MM/DD/YYYY";

    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto imb_r = book.create_account("Imbalance-USD", AccountType::BANK,
        book.root_account_guid());

    auto result = import_csv(book, csv, f, CHECKING, imb_r.unwrap());
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().imported == 1);

    // Verify date was normalized
    auto splits = book.get_splits_for_account(CHECKING);
    REQUIRE(splits.size() == 1);
    auto tx = book.get_transaction(splits[0].tx_guid);
    REQUIRE(tx.has_value());
    REQUIRE(tx->post_date.substr(0, 10) == "2024-01-15");

    book.close();
    fs::remove(tmp);
}
