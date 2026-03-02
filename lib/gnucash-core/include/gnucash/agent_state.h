#pragma once
// Agent State Database
// Per-agent key-value state + review queue stored in
// {book_path}.agent.{agent_name}.db

#include <string>
#include <optional>
#include <vector>
#include <nlohmann/json.hpp>
#include "result.h"

namespace gnucash {
namespace agent {

using json = nlohmann::json;

// ========================================================================
// Review Queue Types
// ========================================================================

enum class ReviewStatus { PENDING, APPROVED, REJECTED };

struct ReviewItem {
    int id;
    std::string transaction_guid;
    std::string suggested_category;     // Target account path
    double confidence;
    std::string reason;                 // Why this category was suggested
    ReviewStatus status;
    std::string created_at;
};

std::string review_status_to_string(ReviewStatus s);
ReviewStatus parse_review_status(const std::string& s);

// ========================================================================
// Agent State Database
// ========================================================================

class AgentStateDB {
public:
    static Result<AgentStateDB> open(const std::string& book_path,
                                      const std::string& agent_name);
    ~AgentStateDB();

    AgentStateDB(const AgentStateDB&) = delete;
    AgentStateDB& operator=(const AgentStateDB&) = delete;
    AgentStateDB(AgentStateDB&&) noexcept;
    AgentStateDB& operator=(AgentStateDB&&) noexcept;

    // Key-value state
    Result<void> set(const std::string& key, const std::string& value);
    Result<std::optional<std::string>> get(const std::string& key);

    // Review queue
    Result<int> enqueue_review(const std::string& transaction_guid,
                                const std::string& suggested_category,
                                double confidence,
                                const std::string& reason);
    Result<std::vector<ReviewItem>> pending_reviews(int limit = 50);
    Result<void> update_review(int id, ReviewStatus status);

    const std::string& db_path() const { return db_path_; }

private:
    AgentStateDB(void* db, std::string path);
    void* db_;
    std::string db_path_;
};

// Schema
constexpr const char* AGENT_STATE_SCHEMA = R"SQL(
CREATE TABLE IF NOT EXISTS agent_state (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS review_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    transaction_guid TEXT NOT NULL,
    suggested_category TEXT NOT NULL,
    confidence REAL NOT NULL,
    reason TEXT,
    status TEXT NOT NULL DEFAULT 'pending',
    created_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_review_status ON review_queue(status);
CREATE INDEX IF NOT EXISTS idx_review_txn ON review_queue(transaction_guid);
)SQL";

} // namespace agent
} // namespace gnucash
