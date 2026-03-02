#include <catch2/catch_test_macros.hpp>
#include "gnucash/slots.h"
#include "gnucash/book.h"
#include "gnucash/guid.h"
#include <sqlite3.h>
#include <cstdio>
#include <filesystem>

using namespace gnucash;
namespace fs = std::filesystem;

#ifndef FIXTURE_DIR
#error "FIXTURE_DIR must be defined"
#endif

static const std::string ACCOUNTS_DB = std::string(FIXTURE_DIR) + "/with-accounts.gnucash";

static std::string make_writable_copy(const std::string& src) {
    auto tmp = fs::temp_directory_path() / ("slots_test_" + generate_guid() + ".gnucash");
    fs::copy_file(src, tmp, fs::copy_options::overwrite_existing);
    return tmp.string();
}

// --- ensure_slots_table ---

TEST_CASE("ensure_slots_table creates table on fresh db", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    auto r = ensure_slots_table(book.raw_db());
    REQUIRE(r.is_ok());

    // Calling again should succeed (IF NOT EXISTS)
    auto r2 = ensure_slots_table(book.raw_db());
    REQUIRE(r2.is_ok());

    book.close();
    fs::remove(tmp);
}

// --- set_slot / get_slot ---

TEST_CASE("set and get string slot", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    std::string guid = "a1000000000000000000000000000008"; // Checking
    auto set_r = set_slot(book.raw_db(), guid, "online_id", "FITID12345");
    REQUIRE(set_r.is_ok());

    auto slot = get_slot(book.raw_db(), guid, "online_id");
    REQUIRE(slot.has_value());
    REQUIRE(slot->string_val == "FITID12345");
    REQUIRE(slot->slot_type == SlotType::STRING);
    REQUIRE(slot->obj_guid == guid);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("set_slot overwrites existing value", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    std::string guid = "a1000000000000000000000000000008";
    set_slot(book.raw_db(), guid, "notes", "first value");
    set_slot(book.raw_db(), guid, "notes", "second value");

    auto slot = get_slot(book.raw_db(), guid, "notes");
    REQUIRE(slot.has_value());
    REQUIRE(slot->string_val == "second value");

    book.close();
    fs::remove(tmp);
}

TEST_CASE("set and get integer slot", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    std::string guid = "a1000000000000000000000000000008";
    auto set_r = set_slot_int(book.raw_db(), guid, "import_count", 42);
    REQUIRE(set_r.is_ok());

    auto slot = get_slot(book.raw_db(), guid, "import_count");
    REQUIRE(slot.has_value());
    REQUIRE(slot->int64_val == 42);
    REQUIRE(slot->slot_type == SlotType::INTEGER);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("get_slot returns nullopt for nonexistent", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    auto slot = get_slot(book.raw_db(), "deadbeef", "nonexistent");
    REQUIRE_FALSE(slot.has_value());

    book.close();
    fs::remove(tmp);
}

// --- delete_slot ---

TEST_CASE("delete_slot removes slot", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    std::string guid = "a1000000000000000000000000000008";
    set_slot(book.raw_db(), guid, "temp_key", "temp_val");

    auto del_r = delete_slot(book.raw_db(), guid, "temp_key");
    REQUIRE(del_r.is_ok());

    auto slot = get_slot(book.raw_db(), guid, "temp_key");
    REQUIRE_FALSE(slot.has_value());

    book.close();
    fs::remove(tmp);
}

// --- get_all_slots ---

TEST_CASE("get_all_slots returns all slots for entity", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    std::string guid = "a1000000000000000000000000000008";
    set_slot(book.raw_db(), guid, "key_a", "val_a");
    set_slot(book.raw_db(), guid, "key_b", "val_b");
    set_slot(book.raw_db(), guid, "key_c", "val_c");

    auto slots = get_all_slots(book.raw_db(), guid);
    REQUIRE(slots.size() == 3);

    // Sorted by name
    REQUIRE(slots[0].name == "key_a");
    REQUIRE(slots[1].name == "key_b");
    REQUIRE(slots[2].name == "key_c");

    book.close();
    fs::remove(tmp);
}

TEST_CASE("get_all_slots empty for unknown guid", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    auto slots = get_all_slots(book.raw_db(), "nonexistent_guid");
    REQUIRE(slots.empty());

    book.close();
    fs::remove(tmp);
}

// --- find_split_by_fitid ---

TEST_CASE("find_split_by_fitid with posted transaction", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    // Post a transaction first
    auto commodities = book.get_commodities();
    REQUIRE_FALSE(commodities.empty());

    Transaction txn;
    txn.currency_guid = commodities[0].guid;
    txn.post_date = "2026-03-01 12:00:00";
    txn.description = "Bank import test";

    Split s1;
    s1.account_guid = "a1000000000000000000000000000008"; // Checking
    s1.value = {-2500, 100};
    s1.quantity = {-2500, 100};
    s1.guid = generate_guid();

    Split s2;
    s2.account_guid = "a1000000000000000000000000000017"; // Groceries
    s2.value = {2500, 100};
    s2.quantity = {2500, 100};

    txn.splits = {s1, s2};

    auto post_r = book.post_transaction(txn);
    REQUIRE(post_r.is_ok());

    // Set online_id slot on the checking split
    set_slot(book.raw_db(), s1.guid, "online_id", "FITID_TEST_001");

    // Find by FITID
    auto found = find_split_by_fitid(book.raw_db(),
        "a1000000000000000000000000000008", "FITID_TEST_001");
    REQUIRE(found.has_value());
    REQUIRE(*found == s1.guid);

    // Wrong FITID returns nullopt
    auto not_found = find_split_by_fitid(book.raw_db(),
        "a1000000000000000000000000000008", "NONEXISTENT_FITID");
    REQUIRE_FALSE(not_found.has_value());

    book.close();
    fs::remove(tmp);
}

TEST_CASE("find_split_by_fitid scoped to account", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    auto commodities = book.get_commodities();
    Transaction txn;
    txn.currency_guid = commodities[0].guid;
    txn.post_date = "2026-03-01 12:00:00";
    txn.description = "Scoped FITID test";

    Split s1;
    s1.account_guid = "a1000000000000000000000000000008"; // Checking
    s1.value = {-1000, 100};
    s1.quantity = {-1000, 100};
    s1.guid = generate_guid();

    Split s2;
    s2.account_guid = "a1000000000000000000000000000017"; // Groceries
    s2.value = {1000, 100};
    s2.quantity = {1000, 100};

    txn.splits = {s1, s2};
    book.post_transaction(txn);

    set_slot(book.raw_db(), s1.guid, "online_id", "SCOPED_FITID");

    // Same FITID but different account -> not found
    auto not_found = find_split_by_fitid(book.raw_db(),
        "a1000000000000000000000000000009", "SCOPED_FITID"); // Savings
    REQUIRE_FALSE(not_found.has_value());

    book.close();
    fs::remove(tmp);
}

// --- check_fitids ---

TEST_CASE("check_fitids batch lookup", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    auto commodities = book.get_commodities();
    Transaction txn;
    txn.currency_guid = commodities[0].guid;
    txn.post_date = "2026-03-01 12:00:00";
    txn.description = "Batch FITID test";

    Split s1;
    s1.account_guid = "a1000000000000000000000000000008";
    s1.value = {-500, 100};
    s1.quantity = {-500, 100};
    s1.guid = generate_guid();

    Split s2;
    s2.account_guid = "a1000000000000000000000000000017";
    s2.value = {500, 100};
    s2.quantity = {500, 100};

    txn.splits = {s1, s2};
    book.post_transaction(txn);

    set_slot(book.raw_db(), s1.guid, "online_id", "BATCH_001");

    auto result = check_fitids(book.raw_db(),
        "a1000000000000000000000000000008",
        {"BATCH_001", "BATCH_002", "BATCH_003"});

    REQUIRE(result.size() == 1);
    REQUIRE(result.count("BATCH_001") == 1);
    REQUIRE(result["BATCH_001"] == s1.guid);
    REQUIRE(result.count("BATCH_002") == 0);

    book.close();
    fs::remove(tmp);
}

// --- Multiple slots on different entities ---

TEST_CASE("slots isolated between entities", "[slots]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    ensure_slots_table(book.raw_db());

    set_slot(book.raw_db(), "entity_a", "key", "value_a");
    set_slot(book.raw_db(), "entity_b", "key", "value_b");

    auto sa = get_slot(book.raw_db(), "entity_a", "key");
    auto sb = get_slot(book.raw_db(), "entity_b", "key");

    REQUIRE(sa.has_value());
    REQUIRE(sb.has_value());
    REQUIRE(sa->string_val == "value_a");
    REQUIRE(sb->string_val == "value_b");

    auto all_a = get_all_slots(book.raw_db(), "entity_a");
    REQUIRE(all_a.size() == 1);

    book.close();
    fs::remove(tmp);
}
