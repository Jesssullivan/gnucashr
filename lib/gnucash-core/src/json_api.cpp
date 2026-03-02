#include "json_api.h"
#include <gnucash/book.h>
#include <gnucash/types.h>
#include <gnucash/fraction.h>
#include <gnucash/guid.h>
#include <gnucash/ofx.h>
#include <gnucash/csv.h>
#include <gnucash/slots.h>
#include <gnucash/bank_feed.h>
#include <gnucash/reconcile.h>
#include <iostream>
#include <memory>
#include <optional>

namespace gnucash {

// --- Singleton book handle ---
// The JSON API manages a single open book at a time.
static std::unique_ptr<Book> g_book;

// --- JSON serialization helpers ---

static json fraction_to_json(const Fraction& f) {
    return {{"num", f.num}, {"denom", f.denom}, {"value", f.to_double()}};
}

static json commodity_to_json(const Commodity& c) {
    return {
        {"guid", c.guid},
        {"namespace", c.ns},
        {"mnemonic", c.mnemonic},
        {"fullname", c.fullname},
        {"cusip", c.cusip},
        {"fraction", c.fraction},
        {"quote_flag", c.quote_flag},
        {"quote_source", c.quote_source}
    };
}

static json account_to_json(const Account& a) {
    return {
        {"guid", a.guid},
        {"name", a.name},
        {"type", account_type_to_string(a.type)},
        {"commodity_guid", a.commodity_guid},
        {"commodity_scu", a.commodity_scu},
        {"non_std_scu", a.non_std_scu},
        {"parent_guid", a.parent_guid},
        {"code", a.code},
        {"description", a.description},
        {"hidden", a.hidden},
        {"placeholder", a.placeholder},
        {"full_path", a.full_path}
    };
}

static json split_to_json(const Split& s) {
    json j = {
        {"guid", s.guid},
        {"tx_guid", s.tx_guid},
        {"account_guid", s.account_guid},
        {"memo", s.memo},
        {"action", s.action},
        {"reconcile_state", s.reconcile_state == ReconcileState::RECONCILED ? "y" :
                            s.reconcile_state == ReconcileState::CLEARED ? "c" : "n"},
        {"value", fraction_to_json(s.value)},
        {"quantity", fraction_to_json(s.quantity)}
    };
    if (s.reconcile_date) j["reconcile_date"] = *s.reconcile_date;
    if (s.lot_guid) j["lot_guid"] = *s.lot_guid;
    return j;
}

static json transaction_to_json(const Transaction& t) {
    json splits = json::array();
    for (const auto& s : t.splits)
        splits.push_back(split_to_json(s));
    return {
        {"guid", t.guid},
        {"currency_guid", t.currency_guid},
        {"num", t.num},
        {"post_date", t.post_date},
        {"enter_date", t.enter_date},
        {"description", t.description},
        {"splits", splits}
    };
}

static json price_to_json(const Price& p) {
    return {
        {"guid", p.guid},
        {"commodity_guid", p.commodity_guid},
        {"currency_guid", p.currency_guid},
        {"date", p.date},
        {"source", p.source},
        {"type", p.type},
        {"value", fraction_to_json(p.value)}
    };
}

static json trial_entry_to_json(const TrialBalanceEntry& e) {
    return {
        {"account_guid", e.account_guid},
        {"account_name", e.account_name},
        {"full_path", e.full_path},
        {"account_type", account_type_to_string(e.account_type)},
        {"balance", e.balance}
    };
}

// --- Parse helpers ---

static std::optional<std::string> opt_string(const json& params, const std::string& key) {
    if (params.contains(key) && !params[key].is_null())
        return params[key].get<std::string>();
    return std::nullopt;
}

// --- Error/result builders ---

static json make_result(const json& result, const json& id) {
    return {{"result", result}, {"id", id}};
}

static json make_error(const std::string& message, const json& id) {
    return {{"error", {{"message", message}}}, {"id", id}};
}

// --- Method handlers ---

static json handle_open(const json& params, const json& id) {
    if (!params.contains("path"))
        return make_error("missing required param: path", id);

    std::string path = params["path"].get<std::string>();
    bool read_only = params.value("read_only", true);

    // Close existing book if open
    g_book.reset();

    auto result = Book::open(path, read_only);
    if (result.is_err())
        return make_error(result.unwrap_err(), id);

    g_book = std::make_unique<Book>(std::move(result.unwrap()));
    return make_result({
        {"path", g_book->path()},
        {"book_guid", g_book->book_guid()},
        {"root_account_guid", g_book->root_account_guid()},
        {"default_currency", g_book->default_currency()},
        {"read_only", g_book->is_read_only()}
    }, id);
}

static json handle_close(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    g_book.reset();
    return make_result("closed", id);
}

static json handle_info(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    return make_result({
        {"path", g_book->path()},
        {"book_guid", g_book->book_guid()},
        {"root_account_guid", g_book->root_account_guid()},
        {"default_currency", g_book->default_currency()},
        {"read_only", g_book->is_read_only()},
        {"valid", g_book->is_valid()}
    }, id);
}

static json handle_get_accounts(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto accounts = g_book->get_accounts();
    json arr = json::array();
    for (const auto& a : accounts)
        arr.push_back(account_to_json(a));
    return make_result(arr, id);
}

static json handle_account_tree(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto accounts = g_book->account_tree();
    json arr = json::array();
    for (const auto& a : accounts)
        arr.push_back(account_to_json(a));
    return make_result(arr, id);
}

static json handle_get_account(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("guid"))
        return make_error("missing required param: guid", id);
    auto account = g_book->get_account(params["guid"].get<std::string>());
    if (!account)
        return make_error("account not found", id);
    return make_result(account_to_json(*account), id);
}

static json handle_get_account_by_path(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("path"))
        return make_error("missing required param: path", id);
    auto account = g_book->get_account_by_path(params["path"].get<std::string>());
    if (!account)
        return make_error("account not found", id);
    return make_result(account_to_json(*account), id);
}

static json handle_get_transactions(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto from_date = opt_string(params, "from_date");
    auto to_date = opt_string(params, "to_date");
    auto txns = g_book->get_transactions(from_date, to_date);
    json arr = json::array();
    for (const auto& t : txns)
        arr.push_back(transaction_to_json(t));
    return make_result(arr, id);
}

static json handle_get_transaction(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("guid"))
        return make_error("missing required param: guid", id);
    auto txn = g_book->get_transaction(params["guid"].get<std::string>());
    if (!txn)
        return make_error("transaction not found", id);
    return make_result(transaction_to_json(*txn), id);
}

static json handle_get_splits(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("account_guid"))
        return make_error("missing required param: account_guid", id);
    auto splits = g_book->get_splits_for_account(params["account_guid"].get<std::string>());
    json arr = json::array();
    for (const auto& s : splits)
        arr.push_back(split_to_json(s));
    return make_result(arr, id);
}

static json handle_get_commodities(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto commodities = g_book->get_commodities();
    json arr = json::array();
    for (const auto& c : commodities)
        arr.push_back(commodity_to_json(c));
    return make_result(arr, id);
}

static json handle_get_prices(const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto prices = g_book->get_prices();
    json arr = json::array();
    for (const auto& p : prices)
        arr.push_back(price_to_json(p));
    return make_result(arr, id);
}

static json handle_get_balance(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("account_guid"))
        return make_error("missing required param: account_guid", id);
    auto as_of = opt_string(params, "as_of");
    double balance = g_book->get_account_balance(
        params["account_guid"].get<std::string>(), as_of);
    return make_result({{"balance", balance}}, id);
}

static json handle_trial_balance(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    auto as_of = opt_string(params, "as_of");
    auto entries = g_book->trial_balance(as_of);
    json arr = json::array();
    for (const auto& e : entries)
        arr.push_back(trial_entry_to_json(e));
    return make_result(arr, id);
}

static json handle_create_account(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("name") || !params.contains("type") || !params.contains("parent_guid"))
        return make_error("missing required params: name, type, parent_guid", id);

    auto type = parse_account_type(params["type"].get<std::string>());
    auto result = g_book->create_account(
        params["name"].get<std::string>(),
        type,
        params["parent_guid"].get<std::string>(),
        params.value("description", ""),
        params.value("code", ""),
        params.value("hidden", false),
        params.value("placeholder", false));

    if (result.is_err())
        return make_error(result.unwrap_err(), id);
    return make_result({{"guid", result.unwrap()}}, id);
}

static json handle_post_transaction(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("description") || !params.contains("post_date") || !params.contains("splits"))
        return make_error("missing required params: description, post_date, splits", id);

    Transaction txn;
    txn.description = params["description"].get<std::string>();
    txn.post_date = params["post_date"].get<std::string>();
    txn.enter_date = params.value("enter_date", txn.post_date);
    txn.num = params.value("num", "");
    txn.currency_guid = params.value("currency_guid", g_book->default_currency());

    for (const auto& sj : params["splits"]) {
        Split s;
        s.account_guid = sj.at("account_guid").get<std::string>();
        s.value.num = sj.at("value_num").get<int64_t>();
        s.value.denom = sj.value("value_denom", int64_t(100));
        s.quantity = s.value;  // same for single-currency
        if (sj.contains("quantity_num")) {
            s.quantity.num = sj["quantity_num"].get<int64_t>();
            s.quantity.denom = sj.value("quantity_denom", s.value.denom);
        }
        s.memo = sj.value("memo", "");
        s.action = sj.value("action", "");
        s.reconcile_state = ReconcileState::NOT_RECONCILED;
        txn.splits.push_back(std::move(s));
    }

    auto result = g_book->post_transaction(txn);
    if (result.is_err())
        return make_error(result.unwrap_err(), id);
    return make_result({{"guid", result.unwrap()}}, id);
}

static json handle_delete_transaction(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("guid"))
        return make_error("missing required param: guid", id);
    auto result = g_book->delete_transaction(params["guid"].get<std::string>());
    if (result.is_err())
        return make_error(result.unwrap_err(), id);
    return make_result("deleted", id);
}

static json handle_void_transaction(const json& params, const json& id) {
    if (!g_book)
        return make_error("no book open", id);
    if (!params.contains("guid") || !params.contains("reason"))
        return make_error("missing required params: guid, reason", id);
    auto result = g_book->void_transaction(
        params["guid"].get<std::string>(),
        params["reason"].get<std::string>());
    if (result.is_err())
        return make_error(result.unwrap_err(), id);
    return make_result("voided", id);
}

static json handle_parse_ofx(const json& params, const json& id) {
    if (!params.contains("content"))
        return make_error("missing required param: content", id);

    std::string content = params["content"].get<std::string>();
    auto result = parse_ofx(content);

    json txns = json::array();
    for (const auto& t : result.transactions) {
        txns.push_back({
            {"date", t.date},
            {"amount", t.amount},
            {"amount_num", t.amount_fraction.num},
            {"amount_denom", t.amount_fraction.denom},
            {"name", t.name},
            {"fitid", t.fitid},
            {"memo", t.memo},
            {"trntype", t.trntype}
        });
    }

    return make_result({
        {"version", detect_ofx_version(content)},
        {"account", {
            {"account_id", result.account.account_id},
            {"bank_id", result.account.bank_id},
            {"account_type", result.account.account_type},
            {"org_name", result.account.org_name},
            {"fid", result.account.fid},
            {"date_start", result.account.date_start},
            {"date_end", result.account.date_end},
            {"ledger_balance", result.account.ledger_balance},
            {"currency", result.account.currency}
        }},
        {"transactions", txns}
    }, id);
}

// --- Import / Reconcile handlers ---

static json handle_import_ofx(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("content"))
        return make_error("missing required param: content", id);
    if (!params.contains("account_path"))
        return make_error("missing required param: account_path", id);

    std::string content = params["content"].get<std::string>();
    std::string account_path = params["account_path"].get<std::string>();
    std::string imbalance_path = params.value("imbalance_account", "Imbalance-USD");

    // Resolve account paths to GUIDs
    auto acct = g_book->get_account_by_path(account_path);
    if (!acct) return make_error("Account not found: " + account_path, id);

    auto imbalance = g_book->get_account_by_path(imbalance_path);
    if (!imbalance) return make_error("Imbalance account not found: " + imbalance_path, id);

    auto result = import_ofx(*g_book, content, acct->guid, imbalance->guid);
    if (result.is_err()) return make_error(result.unwrap_err(), id);

    auto& r = result.unwrap();
    json errors_json = json::array();
    for (const auto& e : r.error_messages) errors_json.push_back(e);

    return make_result({
        {"total_parsed", r.total_parsed},
        {"imported", r.imported},
        {"duplicates", r.duplicates},
        {"errors", r.errors},
        {"imported_guids", r.imported_guids},
        {"error_messages", errors_json}
    }, id);
}

static json handle_import_csv(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("content"))
        return make_error("missing required param: content", id);
    if (!params.contains("account_path"))
        return make_error("missing required param: account_path", id);

    std::string content = params["content"].get<std::string>();
    std::string account_path = params["account_path"].get<std::string>();
    std::string imbalance_path = params.value("imbalance_account", "Imbalance-USD");
    std::string format_name = params.value("format", "generic");

    // Resolve format
    CsvFormat format;
    if (format_name == "paypal") format = paypal_format();
    else if (format_name == "stripe") format = stripe_format();
    else if (format_name == "venmo") format = venmo_format();
    else if (format_name == "apple_card") format = apple_card_format();
    else format = generic_format();

    // Allow generic format override
    if (format_name == "generic" && params.contains("delimiter"))
        format.delimiter = params["delimiter"].get<std::string>()[0];
    if (params.contains("date_col")) format.date_col = params["date_col"].get<int>();
    if (params.contains("amount_col")) format.amount_col = params["amount_col"].get<int>();
    if (params.contains("desc_col")) format.desc_col = params["desc_col"].get<int>();
    if (params.contains("date_format")) format.date_format = params["date_format"].get<std::string>();
    if (params.contains("has_header")) format.has_header = params["has_header"].get<bool>();

    // Resolve account paths
    auto acct = g_book->get_account_by_path(account_path);
    if (!acct) return make_error("Account not found: " + account_path, id);

    auto imbalance = g_book->get_account_by_path(imbalance_path);
    if (!imbalance) return make_error("Imbalance account not found: " + imbalance_path, id);

    auto result = import_csv(*g_book, content, format, acct->guid, imbalance->guid);
    if (result.is_err()) return make_error(result.unwrap_err(), id);

    auto& r = result.unwrap();
    json errors_json = json::array();
    for (const auto& e : r.error_messages) errors_json.push_back(e);

    return make_result({
        {"total_parsed", r.total_parsed},
        {"imported", r.imported},
        {"duplicates", r.duplicates},
        {"errors", r.errors},
        {"imported_guids", r.imported_guids},
        {"error_messages", errors_json}
    }, id);
}

static json handle_check_duplicates(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("account_path"))
        return make_error("missing required param: account_path", id);
    if (!params.contains("fitids"))
        return make_error("missing required param: fitids", id);

    std::string account_path = params["account_path"].get<std::string>();
    auto acct = g_book->get_account_by_path(account_path);
    if (!acct) return make_error("Account not found: " + account_path, id);

    std::vector<std::string> fitids;
    for (const auto& f : params["fitids"]) fitids.push_back(f.get<std::string>());

    auto result = check_duplicates(*g_book, acct->guid, fitids);

    json duplicates = json::object();
    for (const auto& [fitid, split_guid] : result) {
        duplicates[fitid] = split_guid;
    }

    return make_result({
        {"total_checked", static_cast<int>(fitids.size())},
        {"duplicates_found", static_cast<int>(result.size())},
        {"duplicates", duplicates}
    }, id);
}

static json handle_get_slots(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("obj_guid"))
        return make_error("missing required param: obj_guid", id);

    std::string obj_guid = params["obj_guid"].get<std::string>();
    ensure_slots_table(g_book->raw_db());

    if (params.contains("name")) {
        // Get single slot
        std::string name = params["name"].get<std::string>();
        auto slot = get_slot(g_book->raw_db(), obj_guid, name);
        if (!slot) return make_result(json(nullptr), id);

        return make_result({
            {"name", slot->name},
            {"slot_type", static_cast<int>(slot->slot_type)},
            {"string_val", slot->string_val},
            {"int64_val", slot->int64_val},
            {"double_val", slot->double_val}
        }, id);
    } else {
        // Get all slots
        auto slots = get_all_slots(g_book->raw_db(), obj_guid);
        json slots_json = json::array();
        for (const auto& s : slots) {
            slots_json.push_back({
                {"name", s.name},
                {"slot_type", static_cast<int>(s.slot_type)},
                {"string_val", s.string_val},
                {"int64_val", s.int64_val},
                {"double_val", s.double_val}
            });
        }
        return make_result({{"slots", slots_json}}, id);
    }
}

static json handle_set_slot(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("obj_guid") || !params.contains("name") || !params.contains("value"))
        return make_error("missing required params: obj_guid, name, value", id);

    std::string obj_guid = params["obj_guid"].get<std::string>();
    std::string name = params["name"].get<std::string>();
    std::string value = params["value"].get<std::string>();

    ensure_slots_table(g_book->raw_db());
    auto result = set_slot(g_book->raw_db(), obj_guid, name, value);
    if (result.is_err()) return make_error(result.unwrap_err(), id);

    return make_result({{"status", "ok"}}, id);
}

static json handle_update_split(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("split_guid") || !params.contains("new_account_path"))
        return make_error("missing required params: split_guid, new_account_path", id);

    std::string split_guid = params["split_guid"].get<std::string>();
    std::string account_path = params["new_account_path"].get<std::string>();

    auto acct = g_book->get_account_by_path(account_path);
    if (!acct) return make_error("Account not found: " + account_path, id);

    auto result = g_book->update_split(split_guid, acct->guid);
    if (result.is_err()) return make_error(result.unwrap_err(), id);

    return make_result({
        {"status", "ok"},
        {"split_guid", split_guid},
        {"new_account_guid", acct->guid},
        {"new_account_path", acct->full_path}
    }, id);
}

static json handle_reconcile_account(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("account_path") || !params.contains("statement_date") ||
        !params.contains("statement_balance"))
        return make_error("missing required params: account_path, statement_date, statement_balance", id);

    std::string account_path = params["account_path"].get<std::string>();
    std::string statement_date = params["statement_date"].get<std::string>();
    auto statement_balance = Fraction::from_string(
        params["statement_balance"].get<std::string>());

    auto acct = g_book->get_account_by_path(account_path);
    if (!acct) return make_error("Account not found: " + account_path, id);

    auto result = reconcile_account(*g_book, acct->guid, statement_date, statement_balance);
    if (result.is_err()) return make_error(result.unwrap_err(), id);

    auto& r = result.unwrap();
    return make_result({
        {"splits_reconciled", r.splits_reconciled},
        {"statement_balance", fraction_to_json(r.statement_balance)},
        {"book_balance", fraction_to_json(r.book_balance)},
        {"difference", fraction_to_json(r.difference)},
        {"balanced", r.balanced}
    }, id);
}

static json handle_match_imported(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);
    if (!params.contains("account_a_path") || !params.contains("account_b_path"))
        return make_error("missing required params: account_a_path, account_b_path", id);

    std::string a_path = params["account_a_path"].get<std::string>();
    std::string b_path = params["account_b_path"].get<std::string>();
    std::string from_date = params.value("from_date", "2000-01-01");
    std::string to_date = params.value("to_date", "2099-12-31");
    int date_window = params.value("date_window", 3);
    double min_similarity = params.value("min_similarity", 0.5);

    auto acct_a = g_book->get_account_by_path(a_path);
    if (!acct_a) return make_error("Account not found: " + a_path, id);

    auto acct_b = g_book->get_account_by_path(b_path);
    if (!acct_b) return make_error("Account not found: " + b_path, id);

    auto matches = find_cross_institution_matches(
        *g_book, acct_a->guid, acct_b->guid, from_date, to_date,
        date_window, min_similarity);

    json matches_json = json::array();
    for (const auto& m : matches) {
        matches_json.push_back({
            {"split_a_guid", m.split_a_guid},
            {"split_b_guid", m.split_b_guid},
            {"tx_a_guid", m.tx_a_guid},
            {"tx_b_guid", m.tx_b_guid},
            {"amount", fraction_to_json(m.amount)},
            {"date_a", m.date_a},
            {"date_b", m.date_b},
            {"desc_a", m.desc_a},
            {"desc_b", m.desc_b},
            {"similarity", m.similarity}
        });
    }

    return make_result({
        {"matches", matches_json},
        {"total_matches", static_cast<int>(matches.size())}
    }, id);
}

static json handle_bank_feed_status(const json& params, const json& id) {
    if (!g_book) return make_error("no book open", id);

    ensure_slots_table(g_book->raw_db());

    // If account_path provided, show import status for that account
    if (params.contains("account_path")) {
        std::string account_path = params["account_path"].get<std::string>();
        auto acct = g_book->get_account_by_path(account_path);
        if (!acct) return make_error("Account not found: " + account_path, id);

        // Count splits with online_id slots (imported transactions)
        auto splits = g_book->get_splits_for_account(acct->guid);
        int imported_count = 0;
        int unreconciled_count = 0;

        for (const auto& s : splits) {
            auto slot = get_slot(g_book->raw_db(), s.guid, "online_id");
            if (slot) imported_count++;
            if (s.reconcile_state == ReconcileState::NOT_RECONCILED)
                unreconciled_count++;
        }

        return make_result({
            {"account_path", account_path},
            {"account_guid", acct->guid},
            {"total_splits", static_cast<int>(splits.size())},
            {"imported_count", imported_count},
            {"unreconciled_count", unreconciled_count}
        }, id);
    }

    // No account specified -- summary of all accounts with imported transactions
    auto accounts = g_book->account_tree();
    json summary = json::array();
    for (const auto& acct : accounts) {
        auto splits = g_book->get_splits_for_account(acct.guid);
        if (splits.empty()) continue;

        int imported = 0;
        for (const auto& s : splits) {
            auto slot = get_slot(g_book->raw_db(), s.guid, "online_id");
            if (slot) imported++;
        }
        if (imported == 0) continue;

        summary.push_back({
            {"account_path", acct.full_path},
            {"account_guid", acct.guid},
            {"total_splits", static_cast<int>(splits.size())},
            {"imported_count", imported}
        });
    }

    return make_result({{"accounts", summary}}, id);
}

// --- Dispatch ---

json dispatch(const json& request) {
    json id = request.value("id", json(nullptr));
    std::string method;

    try {
        if (!request.contains("method"))
            return make_error("missing 'method' field", id);
        method = request["method"].get<std::string>();
    } catch (const std::exception& e) {
        return make_error(std::string("malformed request: ") + e.what(), id);
    }

    json params = request.value("params", json::object());

    try {
        if (method == "open")              return handle_open(params, id);
        if (method == "close")             return handle_close(id);
        if (method == "info")              return handle_info(id);
        if (method == "get_accounts")      return handle_get_accounts(id);
        if (method == "account_tree")      return handle_account_tree(id);
        if (method == "get_account")       return handle_get_account(params, id);
        if (method == "get_account_by_path") return handle_get_account_by_path(params, id);
        if (method == "get_transactions")  return handle_get_transactions(params, id);
        if (method == "get_transaction")   return handle_get_transaction(params, id);
        if (method == "get_splits")        return handle_get_splits(params, id);
        if (method == "get_commodities")   return handle_get_commodities(id);
        if (method == "get_prices")        return handle_get_prices(id);
        if (method == "get_balance")       return handle_get_balance(params, id);
        if (method == "trial_balance")     return handle_trial_balance(params, id);
        if (method == "create_account")    return handle_create_account(params, id);
        if (method == "post_transaction")  return handle_post_transaction(params, id);
        if (method == "delete_transaction") return handle_delete_transaction(params, id);
        if (method == "void_transaction")  return handle_void_transaction(params, id);
        if (method == "parse_ofx")         return handle_parse_ofx(params, id);
        if (method == "import_ofx")        return handle_import_ofx(params, id);
        if (method == "import_csv")        return handle_import_csv(params, id);
        if (method == "check_duplicates")  return handle_check_duplicates(params, id);
        if (method == "get_slots")         return handle_get_slots(params, id);
        if (method == "set_slot")          return handle_set_slot(params, id);
        if (method == "update_split")      return handle_update_split(params, id);
        if (method == "reconcile_account") return handle_reconcile_account(params, id);
        if (method == "match_imported")    return handle_match_imported(params, id);
        if (method == "bank_feed_status")  return handle_bank_feed_status(params, id);

        return make_error("unknown method: " + method, id);
    } catch (const std::exception& e) {
        return make_error(std::string("internal error: ") + e.what(), id);
    }
}

// --- Main loop ---

void run_json_loop() {
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;

        json response;
        try {
            json request = json::parse(line);
            response = dispatch(request);
        } catch (const json::parse_error& e) {
            response = make_error(std::string("JSON parse error: ") + e.what(), nullptr);
        }

        std::cout << response.dump() << "\n" << std::flush;
    }
}

} // namespace gnucash
