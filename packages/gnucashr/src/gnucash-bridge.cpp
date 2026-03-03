// gnucash-bridge.cpp - Rcpp wrapper for gnucash-core Book operations
//
// Exposes the standalone C++ library to R via Rcpp external pointers.
// This allows R code to use gnucash-core's direct SQLite access
// instead of the R-level RSQLite/DBI path.

#include <Rcpp.h>
#include <gnucash/book.h>
#include <gnucash/types.h>
#include <gnucash/fraction.h>
#include <gnucash/guid.h>
#include <gnucash/csv.h>
#include <gnucash/slots.h>
#include <gnucash/bank_feed.h>
#include <gnucash/reconcile.h>
#include <gnucash/identity.h>
#include <gnucash/audit.h>
#include <gnucash/agent_state.h>
#include <gnucash/security.h>
#include <gnucash/approval.h>

using namespace Rcpp;

// R external pointer type for Book
typedef Rcpp::XPtr<gnucash::Book> BookPtr;

// Helper: check book pointer is valid
static gnucash::Book& check_book(SEXP ptr) {
    BookPtr book(ptr);
    if (!book || !book->is_valid())
        Rcpp::stop("Book handle is invalid or closed");
    return *book;
}

//' Open a GnuCash SQLite Database (C++ Backend)
//'
//' Opens a GnuCash SQLite database file using the native C++ library,
//' returning an external pointer handle for subsequent operations.
//'
//' @param path Path to .gnucash SQLite database file
//' @param read_only Open in read-only mode (default TRUE)
//' @return External pointer to Book object
//' @export
// [[Rcpp::export]]
SEXP gc_open(std::string path, bool read_only = true) {
    auto result = gnucash::Book::open(path, read_only);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    gnucash::Book* book = new gnucash::Book(std::move(result.unwrap()));
    BookPtr ptr(book, true);  // prevent R GC from finalizing immediately
    return ptr;
}

//' Close a GnuCash Database Handle
//'
//' @param book_ptr External pointer from gc_open()
//' @export
// [[Rcpp::export]]
void gc_close(SEXP book_ptr) {
    check_book(book_ptr).close();
}

//' Get Book Metadata
//'
//' @param book_ptr External pointer from gc_open()
//' @return Named list with book_guid, root_account_guid, default_currency, read_only
//' @export
// [[Rcpp::export]]
List gc_info(SEXP book_ptr) {
    auto& book = check_book(book_ptr);
    return List::create(
        Named("path") = book.path(),
        Named("book_guid") = book.book_guid(),
        Named("root_account_guid") = book.root_account_guid(),
        Named("default_currency") = book.default_currency(),
        Named("read_only") = book.is_read_only()
    );
}

// --- Conversion helpers: gnucash types -> R data frames ---

static DataFrame accounts_to_df(const std::vector<gnucash::Account>& accounts) {
    int n = accounts.size();
    CharacterVector guid(n), name(n), type(n), commodity_guid(n);
    IntegerVector commodity_scu(n);
    LogicalVector non_std_scu(n), hidden(n), placeholder(n);
    CharacterVector parent_guid(n), code(n), description(n), full_path(n);

    for (int i = 0; i < n; i++) {
        const auto& a = accounts[i];
        guid[i] = a.guid;
        name[i] = a.name;
        type[i] = gnucash::account_type_to_string(a.type);
        commodity_guid[i] = a.commodity_guid;
        commodity_scu[i] = a.commodity_scu;
        non_std_scu[i] = a.non_std_scu;
        parent_guid[i] = a.parent_guid;
        code[i] = a.code;
        description[i] = a.description;
        hidden[i] = a.hidden;
        placeholder[i] = a.placeholder;
        full_path[i] = a.full_path;
    }

    return DataFrame::create(
        Named("guid") = guid,
        Named("name") = name,
        Named("account_type") = type,
        Named("commodity_guid") = commodity_guid,
        Named("commodity_scu") = commodity_scu,
        Named("non_std_scu") = non_std_scu,
        Named("parent_guid") = parent_guid,
        Named("code") = code,
        Named("description") = description,
        Named("hidden") = hidden,
        Named("placeholder") = placeholder,
        Named("full_path") = full_path,
        Named("stringsAsFactors") = false
    );
}

static DataFrame transactions_to_df(const std::vector<gnucash::Transaction>& txns) {
    // Flatten transactions + splits into a single data frame
    int total = 0;
    for (const auto& t : txns)
        total += std::max((int)t.splits.size(), 1);

    CharacterVector tx_guid(total), currency_guid(total), num(total);
    CharacterVector post_date(total), enter_date(total), description(total);
    CharacterVector split_guid(total), account_guid(total), memo(total), action(total);
    CharacterVector reconcile_state(total);
    NumericVector value_num(total), value_denom(total), quantity_num(total), quantity_denom(total);

    int row = 0;
    for (const auto& t : txns) {
        for (const auto& s : t.splits) {
            tx_guid[row] = t.guid;
            currency_guid[row] = t.currency_guid;
            num[row] = t.num;
            post_date[row] = t.post_date;
            enter_date[row] = t.enter_date;
            description[row] = t.description;
            split_guid[row] = s.guid;
            account_guid[row] = s.account_guid;
            memo[row] = s.memo;
            action[row] = s.action;
            reconcile_state[row] = s.reconcile_state == gnucash::ReconcileState::RECONCILED ? "y" :
                                   s.reconcile_state == gnucash::ReconcileState::CLEARED ? "c" : "n";
            value_num[row] = s.value.num;
            value_denom[row] = s.value.denom;
            quantity_num[row] = s.quantity.num;
            quantity_denom[row] = s.quantity.denom;
            row++;
        }
        if (t.splits.empty()) {
            tx_guid[row] = t.guid;
            currency_guid[row] = t.currency_guid;
            num[row] = t.num;
            post_date[row] = t.post_date;
            enter_date[row] = t.enter_date;
            description[row] = t.description;
            row++;
        }
    }

    return DataFrame::create(
        Named("tx_guid") = tx_guid,
        Named("currency_guid") = currency_guid,
        Named("num") = num,
        Named("post_date") = post_date,
        Named("enter_date") = enter_date,
        Named("description") = description,
        Named("split_guid") = split_guid,
        Named("account_guid") = account_guid,
        Named("memo") = memo,
        Named("action") = action,
        Named("reconcile_state") = reconcile_state,
        Named("value_num") = value_num,
        Named("value_denom") = value_denom,
        Named("quantity_num") = quantity_num,
        Named("quantity_denom") = quantity_denom,
        Named("stringsAsFactors") = false
    );
}

// --- Read operations ---

//' Get All Accounts (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @return Data frame of accounts
//' @export
// [[Rcpp::export]]
DataFrame gc_get_accounts(SEXP book_ptr) {
    auto& book = check_book(book_ptr);
    return accounts_to_df(book.get_accounts());
}

//' Get Account Tree with Full Paths (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @return Data frame of accounts with full_path populated
//' @export
// [[Rcpp::export]]
DataFrame gc_account_tree(SEXP book_ptr) {
    auto& book = check_book(book_ptr);
    return accounts_to_df(book.account_tree());
}

//' Get Account by GUID (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param guid Account GUID
//' @return Data frame with single account row, or empty data frame if not found
//' @export
// [[Rcpp::export]]
DataFrame gc_get_account(SEXP book_ptr, std::string guid) {
    auto& book = check_book(book_ptr);
    auto account = book.get_account(guid);
    if (!account)
        return accounts_to_df({});
    return accounts_to_df({*account});
}

//' Get Account by Path (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param path Colon-separated account path
//' @return Data frame with single account row, or empty data frame if not found
//' @export
// [[Rcpp::export]]
DataFrame gc_get_account_by_path(SEXP book_ptr, std::string path) {
    auto& book = check_book(book_ptr);
    auto account = book.get_account_by_path(path);
    if (!account)
        return accounts_to_df({});
    return accounts_to_df({*account});
}

//' Get Transactions (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param from_date Optional start date filter (YYYY-MM-DD)
//' @param to_date Optional end date filter (YYYY-MM-DD)
//' @return Data frame of transactions with splits
//' @export
// [[Rcpp::export]]
DataFrame gc_get_transactions(SEXP book_ptr,
                              Nullable<std::string> from_date = R_NilValue,
                              Nullable<std::string> to_date = R_NilValue) {
    auto& book = check_book(book_ptr);
    std::optional<std::string> from, to;
    if (from_date.isNotNull()) from = Rcpp::as<std::string>(from_date);
    if (to_date.isNotNull()) to = Rcpp::as<std::string>(to_date);
    return transactions_to_df(book.get_transactions(from, to));
}

//' Get Account Balance (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_guid Account GUID
//' @param as_of Optional date for point-in-time balance
//' @return Numeric balance value
//' @export
// [[Rcpp::export]]
double gc_get_balance(SEXP book_ptr, std::string account_guid,
                      Nullable<std::string> as_of = R_NilValue) {
    auto& book = check_book(book_ptr);
    std::optional<std::string> date;
    if (as_of.isNotNull()) date = Rcpp::as<std::string>(as_of);
    return book.get_account_balance(account_guid, date);
}

//' Get Trial Balance (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param as_of Optional date for point-in-time trial balance
//' @return Data frame with account_guid, account_name, full_path, account_type, balance
//' @export
// [[Rcpp::export]]
DataFrame gc_trial_balance(SEXP book_ptr,
                           Nullable<std::string> as_of = R_NilValue) {
    auto& book = check_book(book_ptr);
    std::optional<std::string> date;
    if (as_of.isNotNull()) date = Rcpp::as<std::string>(as_of);
    auto entries = book.trial_balance(date);

    int n = entries.size();
    CharacterVector guid(n), name(n), path(n), type(n);
    NumericVector balance(n);

    for (int i = 0; i < n; i++) {
        guid[i] = entries[i].account_guid;
        name[i] = entries[i].account_name;
        path[i] = entries[i].full_path;
        type[i] = gnucash::account_type_to_string(entries[i].account_type);
        balance[i] = entries[i].balance;
    }

    return DataFrame::create(
        Named("account_guid") = guid,
        Named("account_name") = name,
        Named("full_path") = path,
        Named("account_type") = type,
        Named("balance") = balance,
        Named("stringsAsFactors") = false
    );
}

// --- Write operations ---

//' Create Account (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param name Account name
//' @param type Account type string (e.g., "EXPENSE", "BANK")
//' @param parent_guid Parent account GUID
//' @param description Optional description
//' @param code Optional account code
//' @param hidden Whether account is hidden
//' @param placeholder Whether account is a placeholder
//' @return GUID of created account
//' @export
// [[Rcpp::export]]
std::string gc_create_account(SEXP book_ptr,
                              std::string name,
                              std::string type,
                              std::string parent_guid,
                              std::string description = "",
                              std::string code = "",
                              bool hidden = false,
                              bool placeholder = false) {
    auto& book = check_book(book_ptr);
    auto result = book.create_account(
        name, gnucash::parse_account_type(type), parent_guid,
        description, code, hidden, placeholder);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
    return result.unwrap();
}

//' Post Transaction (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param description Transaction description
//' @param post_date Post date (YYYY-MM-DD HH:MM:SS)
//' @param splits_df Data frame with columns: account_guid, value_num, value_denom
//' @param num Optional transaction number
//' @return GUID of created transaction
//' @export
// [[Rcpp::export]]
std::string gc_post_transaction(SEXP book_ptr,
                                std::string description,
                                std::string post_date,
                                DataFrame splits_df,
                                std::string num = "") {
    auto& book = check_book(book_ptr);

    gnucash::Transaction txn;
    txn.description = description;
    txn.post_date = post_date;
    txn.enter_date = post_date;
    txn.num = num;
    txn.currency_guid = book.default_currency();

    CharacterVector acct_guids = splits_df["account_guid"];
    NumericVector val_nums = splits_df["value_num"];
    NumericVector val_denoms = splits_df["value_denom"];

    for (int i = 0; i < acct_guids.size(); i++) {
        gnucash::Split s;
        s.account_guid = Rcpp::as<std::string>(acct_guids[i]);
        s.value.num = static_cast<int64_t>(val_nums[i]);
        s.value.denom = static_cast<int64_t>(val_denoms[i]);
        s.quantity = s.value;
        s.reconcile_state = gnucash::ReconcileState::NOT_RECONCILED;
        txn.splits.push_back(std::move(s));
    }

    auto result = book.post_transaction(txn);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
    return result.unwrap();
}

//' Delete Transaction (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param guid Transaction GUID
//' @export
// [[Rcpp::export]]
void gc_delete_transaction(SEXP book_ptr, std::string guid) {
    auto& book = check_book(book_ptr);
    auto result = book.delete_transaction(guid);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

//' Void Transaction (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param guid Transaction GUID
//' @param reason Void reason
//' @export
// [[Rcpp::export]]
void gc_void_transaction(SEXP book_ptr, std::string guid, std::string reason) {
    auto& book = check_book(book_ptr);
    auto result = book.void_transaction(guid, reason);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

// --- Additional Book operations ---

//' Update Split Account (C++ Backend)
//'
//' Reassign a split to a different account (for recategorization).
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param split_guid Split GUID to update
//' @param new_account_guid Target account GUID
//' @export
// [[Rcpp::export]]
void gc_update_split(SEXP book_ptr, std::string split_guid, std::string new_account_guid) {
    auto& book = check_book(book_ptr);
    auto result = book.update_split(split_guid, new_account_guid);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

// Helper: convert splits vector to data frame
static DataFrame splits_to_df(const std::vector<gnucash::Split>& splits) {
    int n = splits.size();
    CharacterVector guid(n), tx_guid(n), account_guid(n), memo(n), action(n);
    CharacterVector reconcile_state(n);
    NumericVector value_num(n), value_denom(n), quantity_num(n), quantity_denom(n);

    for (int i = 0; i < n; i++) {
        const auto& s = splits[i];
        guid[i] = s.guid;
        tx_guid[i] = s.tx_guid;
        account_guid[i] = s.account_guid;
        memo[i] = s.memo;
        action[i] = s.action;
        reconcile_state[i] = s.reconcile_state == gnucash::ReconcileState::RECONCILED ? "y" :
                             s.reconcile_state == gnucash::ReconcileState::CLEARED ? "c" : "n";
        value_num[i] = s.value.num;
        value_denom[i] = s.value.denom;
        quantity_num[i] = s.quantity.num;
        quantity_denom[i] = s.quantity.denom;
    }

    return DataFrame::create(
        Named("guid") = guid,
        Named("tx_guid") = tx_guid,
        Named("account_guid") = account_guid,
        Named("memo") = memo,
        Named("action") = action,
        Named("reconcile_state") = reconcile_state,
        Named("value_num") = value_num,
        Named("value_denom") = value_denom,
        Named("quantity_num") = quantity_num,
        Named("quantity_denom") = quantity_denom,
        Named("stringsAsFactors") = false
    );
}

//' Get Splits for Account (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_guid Account GUID
//' @return Data frame of splits ordered by post_date
//' @export
// [[Rcpp::export]]
DataFrame gc_get_splits(SEXP book_ptr, std::string account_guid) {
    auto& book = check_book(book_ptr);
    return splits_to_df(book.get_splits_for_account(account_guid));
}

// --- CSV operations ---

//' Parse CSV Bank Statement (C++ Backend)
//'
//' Parse a CSV string using a named format preset.
//'
//' @param content CSV content as string
//' @param format_name Format preset: "paypal", "stripe", "venmo", "apple_card", "generic"
//' @return Data frame with date, amount_num, amount_denom, description, memo, id, category
//' @export
// [[Rcpp::export]]
DataFrame gc_parse_csv(std::string content, std::string format_name = "generic") {
    gnucash::CsvFormat format;
    if (format_name == "paypal") format = gnucash::paypal_format();
    else if (format_name == "stripe") format = gnucash::stripe_format();
    else if (format_name == "venmo") format = gnucash::venmo_format();
    else if (format_name == "apple_card") format = gnucash::apple_card_format();
    else format = gnucash::generic_format();

    auto result = gnucash::parse_csv(content, format);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& txns = result.unwrap();
    int n = txns.size();
    CharacterVector date(n), description(n), memo(n), id(n), category(n);
    NumericVector amount_num(n), amount_denom(n);

    for (int i = 0; i < n; i++) {
        date[i] = txns[i].date;
        amount_num[i] = txns[i].amount.num;
        amount_denom[i] = txns[i].amount.denom;
        description[i] = txns[i].description;
        memo[i] = txns[i].memo;
        id[i] = txns[i].id;
        category[i] = txns[i].category;
    }

    return DataFrame::create(
        Named("date") = date,
        Named("amount_num") = amount_num,
        Named("amount_denom") = amount_denom,
        Named("description") = description,
        Named("memo") = memo,
        Named("id") = id,
        Named("category") = category,
        Named("stringsAsFactors") = false
    );
}

//' Detect CSV Format (C++ Backend)
//'
//' Auto-detect CSV format from the header line.
//'
//' @param header_line First line of the CSV file
//' @return Format name string, or empty string if unknown
//' @export
// [[Rcpp::export]]
std::string gc_detect_csv_format(std::string header_line) {
    auto format = gnucash::detect_csv_format(header_line);
    if (format) return format->name;
    return "";
}

//' Get CSV Format Info (C++ Backend)
//'
//' Return column mapping for a named format.
//'
//' @param format_name Format name: "paypal", "stripe", "venmo", "apple_card", "generic"
//' @return Named list with format details
//' @export
// [[Rcpp::export]]
List gc_csv_format_info(std::string format_name) {
    gnucash::CsvFormat f;
    if (format_name == "paypal") f = gnucash::paypal_format();
    else if (format_name == "stripe") f = gnucash::stripe_format();
    else if (format_name == "venmo") f = gnucash::venmo_format();
    else if (format_name == "apple_card") f = gnucash::apple_card_format();
    else f = gnucash::generic_format();

    return List::create(
        Named("name") = f.name,
        Named("delimiter") = std::string(1, f.delimiter),
        Named("has_header") = f.has_header,
        Named("date_col") = f.date_col,
        Named("amount_col") = f.amount_col,
        Named("desc_col") = f.desc_col,
        Named("memo_col") = f.memo_col,
        Named("id_col") = f.id_col,
        Named("category_col") = f.category_col,
        Named("date_format") = f.date_format
    );
}

// --- Slots operations ---

//' Get Slot Value (C++ Backend)
//'
//' Get a single slot value from the GnuCash slots table.
//'
//' @param book_ptr External pointer from gc_open()
//' @param obj_guid Entity GUID (split, account, or transaction)
//' @param name Slot name (e.g., "online_id", "notes")
//' @return Named list with slot details, or NULL if not found
//' @export
// [[Rcpp::export]]
SEXP gc_get_slot(SEXP book_ptr, std::string obj_guid, std::string name) {
    auto& book = check_book(book_ptr);
    auto slot = gnucash::get_slot(book.raw_db(), obj_guid, name);
    if (!slot) return R_NilValue;

    return List::create(
        Named("id") = slot->id,
        Named("obj_guid") = slot->obj_guid,
        Named("name") = slot->name,
        Named("slot_type") = static_cast<int>(slot->slot_type),
        Named("string_val") = slot->string_val,
        Named("int64_val") = static_cast<double>(slot->int64_val),
        Named("double_val") = slot->double_val,
        Named("guid_val") = slot->guid_val
    );
}

//' Set String Slot Value (C++ Backend)
//'
//' Set a string-typed slot on an entity. Creates or updates.
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param obj_guid Entity GUID
//' @param name Slot name
//' @param value Slot value
//' @export
// [[Rcpp::export]]
void gc_set_slot(SEXP book_ptr, std::string obj_guid, std::string name, std::string value) {
    auto& book = check_book(book_ptr);
    auto db = book.raw_db();
    auto r = gnucash::ensure_slots_table(db);
    if (r.is_err()) Rcpp::stop(r.unwrap_err());
    auto result = gnucash::set_slot(db, obj_guid, name, value);
    if (result.is_err()) Rcpp::stop(result.unwrap_err());
}

//' Delete Slot (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param obj_guid Entity GUID
//' @param name Slot name
//' @export
// [[Rcpp::export]]
void gc_delete_slot(SEXP book_ptr, std::string obj_guid, std::string name) {
    auto& book = check_book(book_ptr);
    auto result = gnucash::delete_slot(book.raw_db(), obj_guid, name);
    if (result.is_err()) Rcpp::stop(result.unwrap_err());
}

//' Get All Slots for Entity (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param obj_guid Entity GUID
//' @return Data frame of all slots
//' @export
// [[Rcpp::export]]
DataFrame gc_get_all_slots(SEXP book_ptr, std::string obj_guid) {
    auto& book = check_book(book_ptr);
    auto slots = gnucash::get_all_slots(book.raw_db(), obj_guid);

    int n = slots.size();
    IntegerVector id(n);
    CharacterVector name(n), string_val(n), guid_val(n);
    IntegerVector slot_type(n);
    NumericVector int64_val(n), double_val(n);

    for (int i = 0; i < n; i++) {
        id[i] = slots[i].id;
        name[i] = slots[i].name;
        slot_type[i] = static_cast<int>(slots[i].slot_type);
        string_val[i] = slots[i].string_val;
        int64_val[i] = static_cast<double>(slots[i].int64_val);
        double_val[i] = slots[i].double_val;
        guid_val[i] = slots[i].guid_val;
    }

    return DataFrame::create(
        Named("id") = id,
        Named("name") = name,
        Named("slot_type") = slot_type,
        Named("string_val") = string_val,
        Named("int64_val") = int64_val,
        Named("double_val") = double_val,
        Named("guid_val") = guid_val,
        Named("stringsAsFactors") = false
    );
}

//' Find Split by FITID (C++ Backend)
//'
//' Dedup check: find a split with matching online_id in the given account.
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_guid Account GUID to search within
//' @param fitid FITID (Financial Institution Transaction ID)
//' @return Split GUID string, or empty string if not found
//' @export
// [[Rcpp::export]]
std::string gc_find_split_by_fitid(SEXP book_ptr, std::string account_guid, std::string fitid) {
    auto& book = check_book(book_ptr);
    auto result = gnucash::find_split_by_fitid(book.raw_db(), account_guid, fitid);
    return result.value_or("");
}

//' Batch Check FITIDs (C++ Backend)
//'
//' Check which FITIDs already exist for an account.
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_guid Account GUID
//' @param fitids Character vector of FITIDs to check
//' @return Data frame with columns fitid and split_guid (only matches)
//' @export
// [[Rcpp::export]]
DataFrame gc_check_fitids(SEXP book_ptr, std::string account_guid,
                          CharacterVector fitids) {
    auto& book = check_book(book_ptr);
    std::vector<std::string> fitid_vec;
    for (int i = 0; i < fitids.size(); i++)
        fitid_vec.push_back(Rcpp::as<std::string>(fitids[i]));

    auto result = gnucash::check_fitids(book.raw_db(), account_guid, fitid_vec);

    int n = result.size();
    CharacterVector out_fitid(n), out_split(n);
    int idx = 0;
    for (const auto& [f, s] : result) {
        out_fitid[idx] = f;
        out_split[idx] = s;
        idx++;
    }

    return DataFrame::create(
        Named("fitid") = out_fitid,
        Named("split_guid") = out_split,
        Named("stringsAsFactors") = false
    );
}

// --- Bank feed import operations ---

//' Import OFX Bank Feed (C++ Backend)
//'
//' Parse OFX content, dedup by FITID, and import transactions.
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param content OFX file content as string
//' @param target_account_guid Bank account GUID
//' @param imbalance_account_guid Imbalance/uncategorized account GUID
//' @return Named list with total_parsed, imported, duplicates, errors, imported_guids
//' @export
// [[Rcpp::export]]
List gc_import_ofx_feed(SEXP book_ptr, std::string content,
                        std::string target_account_guid,
                        std::string imbalance_account_guid) {
    auto& book = check_book(book_ptr);
    auto result = gnucash::import_ofx(book, content, target_account_guid, imbalance_account_guid);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& r = result.unwrap();
    return List::create(
        Named("total_parsed") = r.total_parsed,
        Named("imported") = r.imported,
        Named("duplicates") = r.duplicates,
        Named("errors") = r.errors,
        Named("imported_guids") = wrap(r.imported_guids),
        Named("error_messages") = wrap(r.error_messages)
    );
}

//' Import CSV Bank Feed (C++ Backend)
//'
//' Parse CSV content, dedup by FITID, and import transactions.
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param content CSV file content as string
//' @param format_name CSV format preset name
//' @param target_account_guid Bank account GUID
//' @param imbalance_account_guid Imbalance/uncategorized account GUID
//' @return Named list with total_parsed, imported, duplicates, errors, imported_guids
//' @export
// [[Rcpp::export]]
List gc_import_csv_feed(SEXP book_ptr, std::string content,
                        std::string format_name,
                        std::string target_account_guid,
                        std::string imbalance_account_guid) {
    auto& book = check_book(book_ptr);

    gnucash::CsvFormat format;
    if (format_name == "paypal") format = gnucash::paypal_format();
    else if (format_name == "stripe") format = gnucash::stripe_format();
    else if (format_name == "venmo") format = gnucash::venmo_format();
    else if (format_name == "apple_card") format = gnucash::apple_card_format();
    else format = gnucash::generic_format();

    auto result = gnucash::import_csv(book, content, format,
                                       target_account_guid, imbalance_account_guid);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& r = result.unwrap();
    return List::create(
        Named("total_parsed") = r.total_parsed,
        Named("imported") = r.imported,
        Named("duplicates") = r.duplicates,
        Named("errors") = r.errors,
        Named("imported_guids") = wrap(r.imported_guids),
        Named("error_messages") = wrap(r.error_messages)
    );
}

//' Check Duplicate FITIDs (C++ Backend)
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_guid Account GUID
//' @param fitids Character vector of FITIDs
//' @return Data frame with fitid and split_guid columns (matches only)
//' @export
// [[Rcpp::export]]
DataFrame gc_check_duplicates(SEXP book_ptr, std::string account_guid,
                              CharacterVector fitids) {
    auto& book = check_book(book_ptr);
    std::vector<std::string> fitid_vec;
    for (int i = 0; i < fitids.size(); i++)
        fitid_vec.push_back(Rcpp::as<std::string>(fitids[i]));

    auto result = gnucash::check_duplicates(book, account_guid, fitid_vec);

    int n = result.size();
    CharacterVector out_fitid(n), out_split(n);
    int idx = 0;
    for (const auto& [f, s] : result) {
        out_fitid[idx] = f;
        out_split[idx] = s;
        idx++;
    }

    return DataFrame::create(
        Named("fitid") = out_fitid,
        Named("split_guid") = out_split,
        Named("stringsAsFactors") = false
    );
}

// --- Reconciliation operations ---

//' Reconcile Account (C++ Backend)
//'
//' Mark unreconciled splits as cleared up to statement_date and compute balance.
//'
//' @param book_ptr External pointer from gc_open() (must be read-write)
//' @param account_guid Account GUID
//' @param statement_date Statement date (YYYY-MM-DD)
//' @param statement_balance Expected balance as double
//' @return Named list with splits_reconciled, statement_balance, book_balance,
//'   difference, balanced
//' @export
// [[Rcpp::export]]
List gc_reconcile_account(SEXP book_ptr, std::string account_guid,
                          std::string statement_date,
                          double statement_balance) {
    auto& book = check_book(book_ptr);
    auto stmt_bal = gnucash::Fraction::from_double(statement_balance);
    auto result = gnucash::reconcile_account(book, account_guid, statement_date, stmt_bal);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& r = result.unwrap();
    return List::create(
        Named("splits_reconciled") = r.splits_reconciled,
        Named("statement_balance") = r.statement_balance.to_double(),
        Named("book_balance") = r.book_balance.to_double(),
        Named("difference") = r.difference.to_double(),
        Named("balanced") = r.balanced
    );
}

//' Find Cross-Institution Transfer Matches (C++ Backend)
//'
//' Find potential transfer matches between two accounts based on
//' inverted amounts, date proximity, and description similarity.
//'
//' @param book_ptr External pointer from gc_open()
//' @param account_a_guid First account GUID
//' @param account_b_guid Second account GUID
//' @param from_date Start date (YYYY-MM-DD)
//' @param to_date End date (YYYY-MM-DD)
//' @param date_window Max days between matching transactions (default 3)
//' @param min_similarity Minimum match confidence 0-1 (default 0.5)
//' @return Data frame with match details
//' @export
// [[Rcpp::export]]
DataFrame gc_find_transfer_matches(SEXP book_ptr,
                                    std::string account_a_guid,
                                    std::string account_b_guid,
                                    std::string from_date,
                                    std::string to_date,
                                    int date_window = 3,
                                    double min_similarity = 0.5) {
    auto& book = check_book(book_ptr);
    auto matches = gnucash::find_cross_institution_matches(
        book, account_a_guid, account_b_guid,
        from_date, to_date, date_window, min_similarity);

    int n = matches.size();
    CharacterVector split_a(n), split_b(n), tx_a(n), tx_b(n);
    CharacterVector date_a(n), date_b(n), desc_a(n), desc_b(n);
    NumericVector amount(n), similarity(n);

    for (int i = 0; i < n; i++) {
        split_a[i] = matches[i].split_a_guid;
        split_b[i] = matches[i].split_b_guid;
        tx_a[i] = matches[i].tx_a_guid;
        tx_b[i] = matches[i].tx_b_guid;
        amount[i] = matches[i].amount.to_double();
        date_a[i] = matches[i].date_a;
        date_b[i] = matches[i].date_b;
        desc_a[i] = matches[i].desc_a;
        desc_b[i] = matches[i].desc_b;
        similarity[i] = matches[i].similarity;
    }

    return DataFrame::create(
        Named("split_a_guid") = split_a,
        Named("split_b_guid") = split_b,
        Named("tx_a_guid") = tx_a,
        Named("tx_b_guid") = tx_b,
        Named("amount") = amount,
        Named("date_a") = date_a,
        Named("date_b") = date_b,
        Named("desc_a") = desc_a,
        Named("desc_b") = desc_b,
        Named("similarity") = similarity,
        Named("stringsAsFactors") = false
    );
}

// --- Identity operations ---

//' Resolve Identity (C++ Backend)
//'
//' Determine the current user identity using CLI flag, GNUCASH_USER env,
//' or system username fallback.
//'
//' @param cli_identity Optional identity override (e.g. from CLI flag)
//' @return Named list with user_id, display_name, node_name, source
//' @export
// [[Rcpp::export]]
List gc_resolve_identity(Nullable<std::string> cli_identity = R_NilValue) {
    std::optional<std::string> cli;
    if (cli_identity.isNotNull())
        cli = Rcpp::as<std::string>(cli_identity);

    auto id = gnucash::resolve_identity(cli);
    return List::create(
        Named("user_id") = id.user_id,
        Named("display_name") = id.display_name,
        Named("node_name") = id.node_name,
        Named("source") = id.source
    );
}

//' Get System Username (C++ Backend)
//'
//' @return System username string
//' @export
// [[Rcpp::export]]
std::string gc_system_username() {
    return gnucash::get_system_username();
}

// --- Audit operations ---

// R external pointer type for AuditLogger
typedef Rcpp::XPtr<gnucash::audit::AuditLogger> AuditPtr;

static gnucash::audit::AuditLogger& check_audit(SEXP ptr) {
    AuditPtr audit(ptr);
    if (!audit)
        Rcpp::stop("Audit logger handle is invalid or closed");
    return *audit;
}

//' Open Audit Database (C++ Backend)
//'
//' Opens or creates an audit database for a GnuCash book.
//' The database is stored at <book_path>.audit.db.
//'
//' @param book_path Path to the GnuCash book file
//' @return External pointer to AuditLogger object
//' @export
// [[Rcpp::export]]
SEXP gc_audit_open(std::string book_path) {
    auto result = gnucash::audit::AuditLogger::open(book_path);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto* logger = new gnucash::audit::AuditLogger(std::move(result.unwrap()));
    AuditPtr ptr(logger, true);
    return ptr;
}

//' Close Audit Database (C++ Backend)
//'
//' @param audit_ptr External pointer from gc_audit_open()
//' @export
// [[Rcpp::export]]
void gc_audit_close(SEXP audit_ptr) {
    AuditPtr ptr(audit_ptr);
    ptr.release();
}

//' Log Audit Record (C++ Backend)
//'
//' @param audit_ptr External pointer from gc_audit_open()
//' @param tool_name MCP tool name
//' @param book_path GnuCash file path
//' @param classification "read" or "write"
//' @param result_status "success" or "error"
//' @param entity_guid Optional entity GUID
//' @param error_message Optional error message
//' @param duration_ms Optional execution time in milliseconds
//' @export
// [[Rcpp::export]]
void gc_audit_log(SEXP audit_ptr,
                  std::string tool_name,
                  std::string book_path,
                  std::string classification = "read",
                  std::string result_status = "success",
                  Nullable<std::string> entity_guid = R_NilValue,
                  Nullable<std::string> error_message = R_NilValue,
                  Nullable<int> duration_ms = R_NilValue) {
    auto& logger = check_audit(audit_ptr);

    gnucash::audit::AuditRecord record;
    record.timestamp = gnucash::audit::now_iso8601();
    record.tool_name = tool_name;
    record.book_path = book_path;
    record.classification = gnucash::audit::parse_classification(classification);
    record.operation = gnucash::audit::Operation::NONE;
    record.entity_type = gnucash::audit::EntityType::NONE;
    record.result_status = gnucash::audit::parse_result_status(result_status);
    record.arguments = nlohmann::json::object();

    if (entity_guid.isNotNull())
        record.entity_guid = Rcpp::as<std::string>(entity_guid);
    if (error_message.isNotNull())
        record.error_message = Rcpp::as<std::string>(error_message);
    if (duration_ms.isNotNull())
        record.duration_ms = Rcpp::as<int>(duration_ms);

    auto result = logger.log(record);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

//' Query Audit Log (C++ Backend)
//'
//' @param audit_ptr External pointer from gc_audit_open()
//' @param since Optional start timestamp (ISO 8601)
//' @param until Optional end timestamp (ISO 8601)
//' @param tool_name Optional tool name filter
//' @param limit Maximum records to return (default 100)
//' @return Data frame of audit records
//' @export
// [[Rcpp::export]]
DataFrame gc_audit_query(SEXP audit_ptr,
                         Nullable<std::string> since = R_NilValue,
                         Nullable<std::string> until = R_NilValue,
                         Nullable<std::string> tool_name = R_NilValue,
                         int limit = 100) {
    auto& logger = check_audit(audit_ptr);

    gnucash::audit::AuditLogger::QueryFilters filters;
    if (since.isNotNull()) filters.since = Rcpp::as<std::string>(since);
    if (until.isNotNull()) filters.until = Rcpp::as<std::string>(until);
    if (tool_name.isNotNull()) filters.tool_name = Rcpp::as<std::string>(tool_name);
    filters.limit = limit;

    auto result = logger.query(filters);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& records = result.unwrap();
    int n = records.size();
    CharacterVector timestamp(n), tool(n), book_path(n), classif(n);
    CharacterVector status(n), entity_guid(n), error_msg(n);
    IntegerVector dur_ms(n);

    for (int i = 0; i < n; i++) {
        timestamp[i] = records[i].timestamp;
        tool[i] = records[i].tool_name;
        book_path[i] = records[i].book_path;
        classif[i] = gnucash::audit::classification_to_string(records[i].classification);
        status[i] = gnucash::audit::result_status_to_string(records[i].result_status);
        entity_guid[i] = records[i].entity_guid.value_or("");
        error_msg[i] = records[i].error_message.value_or("");
        dur_ms[i] = records[i].duration_ms.value_or(NA_INTEGER);
    }

    return DataFrame::create(
        Named("timestamp") = timestamp,
        Named("tool_name") = tool,
        Named("book_path") = book_path,
        Named("classification") = classif,
        Named("result_status") = status,
        Named("entity_guid") = entity_guid,
        Named("error_message") = error_msg,
        Named("duration_ms") = dur_ms,
        Named("stringsAsFactors") = false
    );
}

//' Export Audit Log in Aperture Format (C++ Backend)
//'
//' Export audit records as JSONL compatible with Tailscale Aperture.
//'
//' @param audit_ptr External pointer from gc_audit_open()
//' @param since Optional start timestamp (ISO 8601)
//' @param until Optional end timestamp (ISO 8601)
//' @param limit Maximum records (default 100)
//' @return JSONL string
//' @export
// [[Rcpp::export]]
std::string gc_audit_export_aperture(SEXP audit_ptr,
                                      Nullable<std::string> since = R_NilValue,
                                      Nullable<std::string> until = R_NilValue,
                                      int limit = 100) {
    auto& logger = check_audit(audit_ptr);

    gnucash::audit::AuditLogger::QueryFilters filters;
    if (since.isNotNull()) filters.since = Rcpp::as<std::string>(since);
    if (until.isNotNull()) filters.until = Rcpp::as<std::string>(until);
    filters.limit = limit;

    auto result = logger.export_aperture(filters);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
    return result.unwrap();
}

// --- Agent State operations ---

// R external pointer type for AgentStateDB
typedef Rcpp::XPtr<gnucash::agent::AgentStateDB> AgentStatePtr;

static gnucash::agent::AgentStateDB& check_agent_state(SEXP ptr) {
    AgentStatePtr state(ptr);
    if (!state)
        Rcpp::stop("Agent state handle is invalid or closed");
    return *state;
}

//' Open Agent State Database (C++ Backend)
//'
//' Opens or creates a per-agent state database.
//' Stored at <book_path>.agent.<agent_name>.db.
//'
//' @param book_path Path to the GnuCash book file
//' @param agent_name Agent identifier (e.g. "spend-monitor")
//' @return External pointer to AgentStateDB object
//' @export
// [[Rcpp::export]]
SEXP gc_agent_state_open(std::string book_path, std::string agent_name) {
    auto result = gnucash::agent::AgentStateDB::open(book_path, agent_name);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto* db = new gnucash::agent::AgentStateDB(std::move(result.unwrap()));
    AgentStatePtr ptr(db, true);
    return ptr;
}

//' Close Agent State Database (C++ Backend)
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @export
// [[Rcpp::export]]
void gc_agent_state_close(SEXP state_ptr) {
    AgentStatePtr ptr(state_ptr);
    ptr.release();
}

//' Set Agent State Value (C++ Backend)
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @param key State key
//' @param value State value
//' @export
// [[Rcpp::export]]
void gc_agent_state_set(SEXP state_ptr, std::string key, std::string value) {
    auto& db = check_agent_state(state_ptr);
    auto result = db.set(key, value);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

//' Get Agent State Value (C++ Backend)
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @param key State key
//' @return Value string, or NULL if key not found
//' @export
// [[Rcpp::export]]
SEXP gc_agent_state_get(SEXP state_ptr, std::string key) {
    auto& db = check_agent_state(state_ptr);
    auto result = db.get(key);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& val = result.unwrap();
    if (!val.has_value())
        return R_NilValue;
    return wrap(*val);
}

//' Enqueue Review Item (C++ Backend)
//'
//' Add a transaction categorization suggestion to the review queue.
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @param transaction_guid Transaction GUID
//' @param suggested_category Target account path
//' @param confidence Confidence score 0-1
//' @param reason Reasoning for the suggestion
//' @return Integer ID of the created review item
//' @export
// [[Rcpp::export]]
int gc_agent_state_enqueue_review(SEXP state_ptr,
                                   std::string transaction_guid,
                                   std::string suggested_category,
                                   double confidence,
                                   std::string reason) {
    auto& db = check_agent_state(state_ptr);
    auto result = db.enqueue_review(transaction_guid, suggested_category, confidence, reason);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
    return result.unwrap();
}

//' Get Pending Reviews (C++ Backend)
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @param limit Maximum items to return (default 50)
//' @return Data frame of pending review items
//' @export
// [[Rcpp::export]]
DataFrame gc_agent_state_pending_reviews(SEXP state_ptr, int limit = 50) {
    auto& db = check_agent_state(state_ptr);
    auto result = db.pending_reviews(limit);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto& items = result.unwrap();
    int n = items.size();
    IntegerVector id(n);
    CharacterVector tx_guid(n), category(n), reason(n), status(n), created(n);
    NumericVector confidence(n);

    for (int i = 0; i < n; i++) {
        id[i] = items[i].id;
        tx_guid[i] = items[i].transaction_guid;
        category[i] = items[i].suggested_category;
        confidence[i] = items[i].confidence;
        reason[i] = items[i].reason;
        status[i] = gnucash::agent::review_status_to_string(items[i].status);
        created[i] = items[i].created_at;
    }

    return DataFrame::create(
        Named("id") = id,
        Named("transaction_guid") = tx_guid,
        Named("suggested_category") = category,
        Named("confidence") = confidence,
        Named("reason") = reason,
        Named("status") = status,
        Named("created_at") = created,
        Named("stringsAsFactors") = false
    );
}

//' Update Review Status (C++ Backend)
//'
//' @param state_ptr External pointer from gc_agent_state_open()
//' @param id Review item ID
//' @param status New status: "pending", "approved", or "rejected"
//' @export
// [[Rcpp::export]]
void gc_agent_state_update_review(SEXP state_ptr, int id, std::string status) {
    auto& db = check_agent_state(state_ptr);
    auto s = gnucash::agent::parse_review_status(status);
    auto result = db.update_review(id, s);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

// ========================================================================
// Security (Week 17)
// ========================================================================

//' Classify MCP Tool Authorization Level (C++ Backend)
//'
//' @param tool_name MCP tool name (e.g., "gnucash_post_transaction")
//' @return String: "auto", "review", or "approve"
//' @export
// [[Rcpp::export]]
std::string gc_classify_tool(std::string tool_name) {
    auto level = gnucash::classify_tool(tool_name);
    return gnucash::audit::authorization_level_to_string(level);
}

//' Run Security Check (C++ Backend)
//'
//' @param policy_list R list with enforcement_enabled, agent_name, agent_tier, rules
//' @param tool_name MCP tool name
//' @param arguments_json JSON string of tool arguments
//' @return Named list: decision, reason, approval_id (or NULL)
//' @export
// [[Rcpp::export]]
Rcpp::List gc_security_check(Rcpp::List policy_list,
                              std::string tool_name,
                              std::string arguments_json) {
    gnucash::SecurityPolicy policy;
    policy.enforcement_enabled = Rcpp::as<bool>(policy_list["enforcement_enabled"]);
    policy.agent_name = Rcpp::as<std::string>(policy_list["agent_name"]);

    std::string tier_str = Rcpp::as<std::string>(policy_list["agent_tier"]);
    policy.agent_tier = gnucash::audit::parse_authorization_level(tier_str);

    gnucash::json args;
    try {
        args = gnucash::json::parse(arguments_json);
    } catch (...) {
        args = gnucash::json::object();
    }

    // Create a fresh rate limiter per check (stateless from R side)
    gnucash::RateLimiter limiter;
    auto result = gnucash::security_check(policy, tool_name, args, limiter);

    return Rcpp::List::create(
        Named("decision") = gnucash::security_decision_to_string(result.decision),
        Named("reason") = result.reason,
        Named("approval_id") = result.approval_id.has_value()
            ? Rcpp::wrap(result.approval_id.value())
            : R_NilValue
    );
}

//' Create Rate Limiter (C++ Backend)
//'
//' @return External pointer to a RateLimiter
//' @export
// [[Rcpp::export]]
SEXP gc_rate_limiter_create() {
    auto* limiter = new gnucash::RateLimiter();
    Rcpp::XPtr<gnucash::RateLimiter> ptr(limiter, true);
    return ptr;
}

//' Check Rate Limit (C++ Backend)
//'
//' @param limiter_ptr External pointer from gc_rate_limiter_create()
//' @param agent Agent identifier
//' @param operation Operation name
//' @param max_per_hour Maximum calls allowed per hour
//' @return Named list: allowed (logical), remaining (integer)
//' @export
// [[Rcpp::export]]
Rcpp::List gc_rate_limiter_check(SEXP limiter_ptr, std::string agent,
                                  std::string operation, int max_per_hour) {
    Rcpp::XPtr<gnucash::RateLimiter> limiter(limiter_ptr);
    if (!limiter) Rcpp::stop("Invalid rate limiter pointer");

    bool allowed = limiter->check_and_record(agent, operation, max_per_hour);
    int remaining = limiter->remaining(agent, operation, max_per_hour);

    return Rcpp::List::create(
        Named("allowed") = allowed,
        Named("remaining") = remaining
    );
}

//' Check Transaction Anomaly (C++ Backend)
//'
//' @param arguments_json JSON string of tool arguments
//' @param amount_threshold Anomaly threshold in cents (default 500000 = $5000)
//' @return Named list: is_anomalous, reason, severity
//' @export
// [[Rcpp::export]]
Rcpp::List gc_check_anomaly(std::string arguments_json,
                             double amount_threshold = 500000) {
    gnucash::json args;
    try {
        args = gnucash::json::parse(arguments_json);
    } catch (...) {
        args = gnucash::json::object();
    }

    auto result = gnucash::check_transaction_anomaly(args,
                      static_cast<int64_t>(amount_threshold));

    return Rcpp::List::create(
        Named("is_anomalous") = result.is_anomalous,
        Named("reason") = result.reason,
        Named("severity") = result.severity
    );
}

// ========================================================================
// Approval Queue (Week 17)
// ========================================================================

// R external pointer type for ApprovalDB
typedef Rcpp::XPtr<gnucash::ApprovalDB> ApprovalPtr;

static gnucash::ApprovalDB& check_approval(SEXP ptr) {
    ApprovalPtr adb(ptr);
    if (!adb)
        Rcpp::stop("Approval DB handle is invalid or closed");
    return *adb;
}

//' Open Approval Database (C++ Backend)
//'
//' @param book_path Path to GnuCash file (approval DB at <book>.approvals.db)
//' @return External pointer to ApprovalDB
//' @export
// [[Rcpp::export]]
SEXP gc_approval_open(std::string book_path) {
    auto result = gnucash::ApprovalDB::open(book_path);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto* adb = new gnucash::ApprovalDB(std::move(result.unwrap()));
    ApprovalPtr ptr(adb, true);
    return ptr;
}

//' Close Approval Database (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @export
// [[Rcpp::export]]
void gc_approval_close(SEXP approval_ptr) {
    ApprovalPtr ptr(approval_ptr);
    ptr.release();
}

//' Create Approval Request (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @param agent_name Agent creating the request
//' @param tool_name Tool requiring approval
//' @param arguments_json JSON string of tool arguments
//' @param requesting_user User identity
//' @param reason Why approval is needed
//' @return Request ID (GUID string)
//' @export
// [[Rcpp::export]]
std::string gc_approval_create(SEXP approval_ptr,
                                std::string agent_name,
                                std::string tool_name,
                                std::string arguments_json,
                                std::string requesting_user,
                                std::string reason) {
    auto& adb = check_approval(approval_ptr);

    gnucash::json args;
    try {
        args = gnucash::json::parse(arguments_json);
    } catch (...) {
        args = gnucash::json::object();
    }

    auto result = adb.create_request(agent_name, tool_name, args,
                                      requesting_user, reason);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    return result.unwrap();
}

//' List Pending Approval Requests (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @param limit Maximum number of requests to return
//' @return Data frame of pending requests
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame gc_approval_pending(SEXP approval_ptr, int limit = 50) {
    auto& adb = check_approval(approval_ptr);
    auto result = adb.pending_requests(limit);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto requests = result.unwrap();
    int n = static_cast<int>(requests.size());

    Rcpp::CharacterVector id(n), agent(n), tool(n), user(n);
    Rcpp::CharacterVector reason_vec(n), created(n), status(n);

    for (int i = 0; i < n; ++i) {
        id[i] = requests[i].id;
        agent[i] = requests[i].agent_name;
        tool[i] = requests[i].tool_name;
        user[i] = requests[i].requesting_user;
        reason_vec[i] = requests[i].reason;
        created[i] = requests[i].created_at;
        status[i] = requests[i].status;
    }

    return Rcpp::DataFrame::create(
        Named("id") = id,
        Named("agent_name") = agent,
        Named("tool_name") = tool,
        Named("requesting_user") = user,
        Named("reason") = reason_vec,
        Named("created_at") = created,
        Named("status") = status,
        Named("stringsAsFactors") = false
    );
}

//' Approve a Pending Request (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @param id Request ID to approve
//' @param approver Identity of the approver
//' @export
// [[Rcpp::export]]
void gc_approval_approve(SEXP approval_ptr, std::string id, std::string approver) {
    auto& adb = check_approval(approval_ptr);
    auto result = adb.approve(id, approver);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

//' Reject a Pending Request (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @param id Request ID to reject
//' @param approver Identity of the approver
//' @param reason Reason for rejection
//' @export
// [[Rcpp::export]]
void gc_approval_reject(SEXP approval_ptr, std::string id,
                         std::string approver, std::string reason) {
    auto& adb = check_approval(approval_ptr);
    auto result = adb.reject(id, approver, reason);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());
}

//' Get a Specific Approval Request (C++ Backend)
//'
//' @param approval_ptr External pointer from gc_approval_open()
//' @param id Request ID
//' @return Named list or NULL if not found
//' @export
// [[Rcpp::export]]
SEXP gc_approval_get(SEXP approval_ptr, std::string id) {
    auto& adb = check_approval(approval_ptr);
    auto result = adb.get_request(id);
    if (result.is_err())
        Rcpp::stop(result.unwrap_err());

    auto opt = result.unwrap();
    if (!opt.has_value()) {
        return R_NilValue;
    }

    auto& req = *opt;
    return Rcpp::List::create(
        Named("id") = req.id,
        Named("agent_name") = req.agent_name,
        Named("tool_name") = req.tool_name,
        Named("arguments") = req.arguments.dump(),
        Named("requesting_user") = req.requesting_user,
        Named("reason") = req.reason,
        Named("created_at") = req.created_at,
        Named("status") = req.status,
        Named("approver") = req.approver.has_value()
            ? Rcpp::wrap(req.approver.value()) : R_NilValue,
        Named("resolved_at") = req.resolved_at.has_value()
            ? Rcpp::wrap(req.resolved_at.value()) : R_NilValue,
        Named("rejection_reason") = req.rejection_reason.has_value()
            ? Rcpp::wrap(req.rejection_reason.value()) : R_NilValue
    );
}
