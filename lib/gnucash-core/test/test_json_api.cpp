#include <catch2/catch_test_macros.hpp>
#include "../src/json_api.h"
#include <nlohmann/json.hpp>
#include <string>
#include <fstream>
#include <cstdlib>

using json = nlohmann::json;

static std::string fixture(const std::string& name) {
    return std::string(FIXTURE_DIR) + "/" + name;
}

// Helper to make a writable copy of a fixture
static std::string make_writable_copy(const std::string& fixture_name) {
    std::string src = fixture(fixture_name);
    std::string dst = std::string(FIXTURE_DIR) + "/tmp_" + fixture_name;
    std::ifstream in(src, std::ios::binary);
    std::ofstream out(dst, std::ios::binary);
    out << in.rdbuf();
    return dst;
}

static void remove_file(const std::string& path) {
    std::remove(path.c_str());
}

TEST_CASE("dispatch: missing method", "[json_api]") {
    auto resp = gnucash::dispatch({{"id", 1}});
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["error"]["message"] == "missing 'method' field");
    REQUIRE(resp["id"] == 1);
}

TEST_CASE("dispatch: unknown method", "[json_api]") {
    auto resp = gnucash::dispatch({{"method", "foo"}, {"id", 2}});
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["error"]["message"] == "unknown method: foo");
}

TEST_CASE("dispatch: open nonexistent file", "[json_api]") {
    auto resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", "/nonexistent.gnucash"}}},
        {"id", 3}
    });
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["id"] == 3);
}

TEST_CASE("dispatch: open missing path param", "[json_api]") {
    auto resp = gnucash::dispatch({
        {"method", "open"},
        {"params", json::object()},
        {"id", 4}
    });
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["error"]["message"] == "missing required param: path");
}

TEST_CASE("dispatch: info without open", "[json_api]") {
    // Force close any prior state
    gnucash::dispatch({{"method", "close"}, {"id", 0}});

    auto resp = gnucash::dispatch({{"method", "info"}, {"id", 5}});
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["error"]["message"] == "no book open");
}

TEST_CASE("dispatch: open and info", "[json_api]") {
    auto resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 10}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"]["read_only"] == true);
    REQUIRE(resp["result"].contains("book_guid"));

    auto info = gnucash::dispatch({{"method", "info"}, {"id", 11}});
    REQUIRE(info["result"]["valid"] == true);
    REQUIRE(info["result"]["default_currency"].get<std::string>().size() > 0);
}

TEST_CASE("dispatch: get_accounts", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 20}
    });

    auto resp = gnucash::dispatch({{"method", "get_accounts"}, {"id", 21}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].is_array());
    REQUIRE(resp["result"].size() == 20);

    // Check one account has expected fields
    auto first = resp["result"][0];
    REQUIRE(first.contains("guid"));
    REQUIRE(first.contains("name"));
    REQUIRE(first.contains("type"));
}

TEST_CASE("dispatch: account_tree", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 30}
    });

    auto resp = gnucash::dispatch({{"method", "account_tree"}, {"id", 31}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].is_array());

    // Should have accounts with full_path populated
    bool found_path = false;
    for (const auto& a : resp["result"]) {
        if (!a["full_path"].get<std::string>().empty()) {
            found_path = true;
            break;
        }
    }
    REQUIRE(found_path);
}

TEST_CASE("dispatch: get_account by guid", "[json_api]") {
    auto open_resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 40}
    });
    std::string root_guid = open_resp["result"]["root_account_guid"].get<std::string>();

    auto resp = gnucash::dispatch({
        {"method", "get_account"},
        {"params", {{"guid", root_guid}}},
        {"id", 41}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"]["type"] == "ROOT");
}

TEST_CASE("dispatch: get_account_by_path", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 50}
    });

    auto resp = gnucash::dispatch({
        {"method", "get_account_by_path"},
        {"params", {{"path", "Root Account:Assets:Current Assets:Checking Account"}}},
        {"id", 51}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"]["name"] == "Checking Account");
}

TEST_CASE("dispatch: get_commodities", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 60}
    });

    auto resp = gnucash::dispatch({{"method", "get_commodities"}, {"id", 61}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].is_array());
    REQUIRE(resp["result"].size() >= 1);
    REQUIRE(resp["result"][0].contains("mnemonic"));
}

TEST_CASE("dispatch: get_transactions empty", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 70}
    });

    auto resp = gnucash::dispatch({{"method", "get_transactions"}, {"id", 71}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].is_array());
    REQUIRE(resp["result"].size() == 0);
}

TEST_CASE("dispatch: get_balance", "[json_api]") {
    auto open_resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 80}
    });
    std::string root_guid = open_resp["result"]["root_account_guid"].get<std::string>();

    auto resp = gnucash::dispatch({
        {"method", "get_balance"},
        {"params", {{"account_guid", root_guid}}},
        {"id", 81}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"]["balance"] == 0.0);
}

TEST_CASE("dispatch: trial_balance", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 90}
    });

    auto resp = gnucash::dispatch({{"method", "trial_balance"}, {"id", 91}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].is_array());
}

TEST_CASE("dispatch: create_account", "[json_api]") {
    auto tmp = make_writable_copy("with-accounts.gnucash");

    auto open_resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", tmp}, {"read_only", false}}},
        {"id", 100}
    });
    REQUIRE(open_resp.contains("result"));
    std::string root_guid = open_resp["result"]["root_account_guid"].get<std::string>();

    auto resp = gnucash::dispatch({
        {"method", "create_account"},
        {"params", {
            {"name", "Test JSON Account"},
            {"type", "EXPENSE"},
            {"parent_guid", root_guid}
        }},
        {"id", 101}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].contains("guid"));
    REQUIRE(resp["result"]["guid"].get<std::string>().size() == 32);

    gnucash::dispatch({{"method", "close"}, {"id", 102}});
    remove_file(tmp);
}

TEST_CASE("dispatch: post_transaction", "[json_api]") {
    auto tmp = make_writable_copy("with-accounts.gnucash");

    auto open_resp = gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", tmp}, {"read_only", false}}},
        {"id", 110}
    });
    REQUIRE(open_resp.contains("result"));

    // Get two account GUIDs
    auto tree = gnucash::dispatch({{"method", "account_tree"}, {"id", 111}});
    std::string checking_guid, expenses_guid;
    for (const auto& a : tree["result"]) {
        if (a["full_path"] == "Root Account:Assets:Current Assets:Checking Account")
            checking_guid = a["guid"].get<std::string>();
        if (a["full_path"] == "Root Account:Expenses:Groceries")
            expenses_guid = a["guid"].get<std::string>();
    }
    REQUIRE(!checking_guid.empty());
    REQUIRE(!expenses_guid.empty());

    auto resp = gnucash::dispatch({
        {"method", "post_transaction"},
        {"params", {
            {"description", "Test grocery purchase"},
            {"post_date", "2024-03-15 00:00:00"},
            {"splits", {
                {{"account_guid", expenses_guid}, {"value_num", 5000}, {"value_denom", 100}},
                {{"account_guid", checking_guid}, {"value_num", -5000}, {"value_denom", 100}}
            }}
        }},
        {"id", 112}
    });
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"].contains("guid"));

    // Verify balance changed
    auto bal = gnucash::dispatch({
        {"method", "get_balance"},
        {"params", {{"account_guid", expenses_guid}}},
        {"id", 113}
    });
    REQUIRE(bal["result"]["balance"] == 50.0);

    gnucash::dispatch({{"method", "close"}, {"id", 114}});
    remove_file(tmp);
}

TEST_CASE("dispatch: close", "[json_api]") {
    gnucash::dispatch({
        {"method", "open"},
        {"params", {{"path", fixture("with-accounts.gnucash")}}},
        {"id", 120}
    });

    auto resp = gnucash::dispatch({{"method", "close"}, {"id", 121}});
    REQUIRE(resp.contains("result"));
    REQUIRE(resp["result"] == "closed");

    // Subsequent calls should fail
    auto info = gnucash::dispatch({{"method", "info"}, {"id", 122}});
    REQUIRE(info.contains("error"));
}

TEST_CASE("dispatch: null id handled", "[json_api]") {
    auto resp = gnucash::dispatch({{"method", "info"}});
    REQUIRE(resp.contains("error"));
    REQUIRE(resp["id"].is_null());
}
