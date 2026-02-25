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
