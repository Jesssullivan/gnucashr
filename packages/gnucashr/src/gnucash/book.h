#pragma once
// GnuCash Book - SQLite database reader/writer
// Core interface for all GnuCash database operations

#include "types.h"
#include "result.h"
#include <string>
#include <vector>
#include <optional>
#include <memory>

struct sqlite3;  // forward declare

namespace gnucash {

class Book {
public:
    ~Book();

    // Open a GnuCash SQLite database
    static Result<Book> open(const std::string& path, bool read_only = true);

    // Close the database connection
    void close();

    // Connection state
    bool is_valid() const;
    bool is_read_only() const;
    const std::string& path() const;
    const std::string& book_guid() const;
    const std::string& root_account_guid() const;
    const std::string& default_currency() const;

    // --- Read operations ---

    std::vector<Account> get_accounts() const;
    std::vector<Transaction> get_transactions(
        std::optional<std::string> from_date = std::nullopt,
        std::optional<std::string> to_date = std::nullopt) const;
    std::vector<Split> get_splits_for_account(const std::string& account_guid) const;
    std::vector<Commodity> get_commodities() const;
    std::vector<Price> get_prices() const;

    // Single-entity lookups
    std::optional<Account> get_account(const std::string& guid) const;
    std::optional<Account> get_account_by_path(const std::string& path) const;
    std::optional<Transaction> get_transaction(const std::string& guid) const;

    // Account tree with full paths
    std::vector<Account> account_tree() const;

    // --- Balance & report operations ---

    double get_account_balance(const std::string& account_guid,
                               std::optional<std::string> as_of = std::nullopt) const;
    TrialBalance trial_balance(std::optional<std::string> as_of = std::nullopt) const;

    // --- Write operations ---

    Result<std::string> create_account(
        const std::string& name,
        AccountType type,
        const std::string& parent_guid,
        const std::string& description = "",
        const std::string& code = "",
        bool hidden = false,
        bool placeholder = false);

    Result<std::string> post_transaction(const Transaction& txn);

    // Update a split's account assignment (for recategorization)
    Result<void> update_split(const std::string& split_guid,
                              const std::string& new_account_guid);

    Result<void> delete_transaction(const std::string& guid);
    Result<void> void_transaction(const std::string& guid, const std::string& reason);

    // Raw database handle (for slots/bank_feed modules)
    sqlite3* raw_db() const;

    // Move semantics only (no copy)
    Book(Book&& other) noexcept;
    Book& operator=(Book&& other) noexcept;
    Book(const Book&) = delete;
    Book& operator=(const Book&) = delete;

private:
    Book();

    sqlite3* db_;
    std::string path_;
    std::string book_guid_;
    std::string root_account_guid_;
    std::string default_currency_;
    bool read_only_;

    // Internal helpers
    Result<void> init_metadata();
    void build_account_paths(std::vector<Account>& accounts) const;
    std::string resolve_currency_guid() const;
};

} // namespace gnucash
