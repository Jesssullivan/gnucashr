# Phase 2: GnuCash Interface Layer (Weeks 3-4)

**Goal**: Extract a standalone C++ library for GnuCash DB operations that agents can use
without R. Enhance the R package's write capabilities. Both share the same C++ core.

---

## Week 3: Standalone C++ Library

### Tasks

- [ ] **3.1 Design `lib/gnucash-core/` C++ library**
  - Headers-only or static lib, no R dependency
  - Core API surface:
    ```cpp
    namespace gnucash {
      // Database connection
      class Book {
        static Book open(const std::string& path);  // SQLite
        void close();
        bool is_valid() const;
      };

      // Read operations
      std::vector<Account> get_accounts(const Book& book);
      std::vector<Transaction> get_transactions(
        const Book& book,
        std::optional<Date> from,
        std::optional<Date> to
      );
      std::vector<Split> get_splits_for_account(
        const Book& book,
        const std::string& account_guid
      );

      // Balance operations
      Fraction get_account_balance(const Book& book, const std::string& guid);
      TrialBalance trial_balance(const Book& book, Date as_of);

      // Write operations (return Result<void, Error>)
      Result<void> post_transaction(Book& book, const Transaction& txn);
      Result<void> create_account(Book& book, const Account& acct);

      // Validation
      bool validate_transaction(const Transaction& txn);
      bool validate_book_integrity(const Book& book);
    }
    ```
  - Use SQLite C API directly (sqlite3.h) -- no ORM
  - Reuse existing fraction.cpp, guid.cpp, validation.cpp logic
  - **Success**: `bazel build //lib/gnucash-core:gnucash_core` compiles

- [ ] **3.2 Implement GnuCash SQLite schema reader**
  - Map all GnuCash 5.x tables:
    - `accounts` (guid, name, account_type, commodity_guid, parent_guid, ...)
    - `transactions` (guid, currency_guid, post_date, enter_date, description)
    - `splits` (guid, tx_guid, account_guid, memo, value_num, value_denom, quantity_num, quantity_denom)
    - `commodities` (guid, namespace, mnemonic, fullname, fraction)
    - `prices` (guid, commodity_guid, currency_guid, date, value_num, value_denom, source)
    - `slots` (obj_guid, name, slot_type, string_val, numeric_val_num, ...)
    - `budgets`, `budget_amounts`
    - `schedxactions`, `recurrences`
  - Schema version detection (check `versions` table)
  - **Success**: Can read a real GnuCash 5.x SQLite file and enumerate all accounts

- [ ] **3.3 Implement Result<T, E> in C++**
  - Monadic error handling matching the R package's pattern:
    ```cpp
    template<typename T, typename E = std::string>
    class Result {
      std::variant<T, E> value_;
    public:
      bool is_ok() const;
      bool is_err() const;
      T unwrap() const;  // throws on err
      T unwrap_or(T default_val) const;
      template<typename F> auto map(F&& f) -> Result<decltype(f(std::declval<T>())), E>;
      template<typename F> auto bind(F&& f) -> decltype(f(std::declval<T>()));
    };
    ```
  - **Success**: Unit tests pass for Result monad operations

- [ ] **3.4 C++ test suite with Catch2 or doctest**
  - Test against fixture databases (same ones used by R tests)
  - Copy `tests/testthat/fixtures/databases/*.gnucash` to `lib/gnucash-core/test/fixtures/`
  - Tests: account enumeration, transaction reading, balance calculation, fraction math
  - **Success**: `bazel test //lib/gnucash-core:all_tests` passes

- [ ] **3.5 Nix derivation for C++ library**
  - Add to flake.nix: `packages.gnucash-core`
  - Depends on sqlite, catch2/doctest
  - Produces static lib + headers
  - **Success**: `nix build .#gnucash-core` produces usable library

### User Interaction Point
> After 3.1-3.5: The C++ library can now read your real GnuCash books independently of R.
> Test with: `bazel run //lib/gnucash-core:read_book -- /path/to/your/book.gnucash`
> Does it correctly enumerate your accounts and recent transactions?

---

## Week 4: Enhanced Write Operations + Agent Bridge

### Tasks

- [ ] **4.1 Transaction posting in C++ lib**
  - Full double-entry validation before write
  - GUID generation for new transactions/splits
  - Fraction arithmetic for amount handling (reuse existing fraction.cpp)
  - Atomic writes with SQLite transactions (BEGIN/COMMIT/ROLLBACK)
  - Backup-before-write (copy DB before modification)
  - **Success**: Can create a transaction in C++ and read it back in GnuCash desktop app

- [ ] **4.2 Account creation in C++ lib**
  - Parent account resolution by path ("Assets:Bank:Checking")
  - Account type validation (asset, liability, income, expense, equity)
  - Commodity assignment
  - **Success**: Can create accounts that GnuCash desktop recognizes

- [ ] **4.3 JSON API for C++ lib (agent bridge)**
  - JSON-in, JSON-out interface for agent consumption:
    ```json
    // Request
    {"action": "get_trial_balance", "book": "/path/to/book.gnucash", "as_of": "2026-02-24"}

    // Response
    {"status": "ok", "data": {"accounts": [{"path": "Assets:Bank", "balance": 1234.56}, ...]}}
    ```
  - Uses nlohmann/json or simdjson
  - Stdin/stdout mode for MCP tool use
  - **Success**: `echo '{"action":"list_accounts",...}' | gnucash-core-cli` returns JSON

- [ ] **4.4 Wire C++ lib back into R package via Rcpp**
  - packages/gnucashr/src/ includes headers from lib/gnucash-core/
  - Rcpp wrappers call shared C++ implementation
  - No code duplication between standalone lib and R package
  - Bazel handles the include path; Makevars uses PKG_CPPFLAGS
  - **Success**: R package tests still pass using shared C++ code

- [ ] **4.5 OFX import in C++ lib (extract from R package)**
  - Move ofx-parser.cpp logic to lib/gnucash-core/
  - Add QFX variant support
  - Standalone CLI: `gnucash-core-cli import-ofx bank-statement.ofx --book my.gnucash`
  - **Success**: Can import a bank statement into GnuCash DB from CLI

- [ ] **4.6 Dhall schema for GnuCash operations**
  - Define allowed operations per agent:
    ```dhall
    let SpendMonitorOps =
      { can_read_accounts : Bool
      , can_read_transactions : Bool
      , can_categorize : Bool
      , can_write_transactions : Bool
      , can_modify_accounts : Bool
      , can_export_reports : Bool
      , max_transaction_amount : Optional Natural
      }
    ```
  - Each agent gets a Dhall config defining its capability envelope
  - **Success**: `dhall-to-json` produces valid agent capability configs

### User Interaction Point
> After Week 4: The C++ lib is the critical piece. Try importing an OFX statement:
> `just lib::import-ofx /path/to/statement.ofx --book /path/to/book.gnucash`
> Open the book in GnuCash desktop -- are the imported transactions correct?
> This validates the write path that agents will use.

---

## Go/No-Go Gate G2

| Criterion | How to Verify | Required? |
|-----------|--------------|-----------|
| C++ lib reads GnuCash SQLite without R | `bazel run //lib/gnucash-core:read_book` | Yes |
| C++ lib writes transactions that GnuCash opens | Manual verification in GnuCash GUI | Yes |
| JSON API works via stdin/stdout | `echo '...' \| gnucash-core-cli` | Yes |
| R package still passes check | `just gnucashr::check` | Yes |
| C++ test suite passes | `bazel test //lib/gnucash-core:all_tests` | Yes |
| No code duplication between lib and R package | Code review | Yes |

**Decision**: If C++ lib can read AND write to GnuCash DBs correctly, proceed to agents.
If writes corrupt the DB, stop and fix -- data integrity is non-negotiable.
