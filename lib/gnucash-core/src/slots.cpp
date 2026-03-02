// GnuCash slots table - key-value metadata on entities
// Uses sqlite3 C API directly (matches book.cpp pattern)

#include "gnucash/slots.h"
#include <sqlite3.h>
#include <sstream>

namespace gnucash {

namespace {
    struct StmtGuard {
        sqlite3_stmt* stmt;
        StmtGuard(sqlite3_stmt* s) : stmt(s) {}
        ~StmtGuard() { if (stmt) sqlite3_finalize(stmt); }
    };

    std::string col_text(sqlite3_stmt* stmt, int col) {
        const char* p = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        return p ? p : "";
    }

    Slot read_slot_row(sqlite3_stmt* stmt) {
        Slot s;
        s.id = sqlite3_column_int(stmt, 0);
        s.obj_guid = col_text(stmt, 1);
        s.name = col_text(stmt, 2);
        s.slot_type = static_cast<SlotType>(sqlite3_column_int(stmt, 3));
        s.int64_val = sqlite3_column_int64(stmt, 4);
        s.string_val = col_text(stmt, 5);
        s.double_val = sqlite3_column_double(stmt, 6);
        s.guid_val = col_text(stmt, 7);
        return s;
    }
} // anon

Result<void> ensure_slots_table(sqlite3* db) {
    // GnuCash 5.x slots schema
    // The id column is INTEGER PRIMARY KEY AUTOINCREMENT
    const char* sql =
        "CREATE TABLE IF NOT EXISTS slots ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  obj_guid TEXT NOT NULL,"
        "  name TEXT NOT NULL,"
        "  slot_type INTEGER NOT NULL,"
        "  int64_val INTEGER,"
        "  string_val TEXT,"
        "  double_val REAL,"
        "  guid_val TEXT,"
        "  timespec_val TEXT,"
        "  gdate_val TEXT,"
        "  numeric_val_num INTEGER,"
        "  numeric_val_denom INTEGER"
        ")";

    char* errmsg = nullptr;
    int rc = sqlite3_exec(db, sql, nullptr, nullptr, &errmsg);
    if (rc != SQLITE_OK) {
        std::string err = errmsg ? errmsg : "unknown error";
        sqlite3_free(errmsg);
        return Result<void>::err("Failed to create slots table: " + err);
    }

    // Index for fast lookups by obj_guid
    sqlite3_exec(db,
        "CREATE INDEX IF NOT EXISTS idx_slots_guid ON slots(obj_guid)",
        nullptr, nullptr, nullptr);

    // Index for FITID dedup: (name, string_val) for online_id lookups
    sqlite3_exec(db,
        "CREATE INDEX IF NOT EXISTS idx_slots_name_val ON slots(name, string_val)",
        nullptr, nullptr, nullptr);

    return Result<void>::ok();
}

std::optional<Slot> get_slot(sqlite3* db, const std::string& obj_guid,
                             const std::string& name) {
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db,
        "SELECT id, obj_guid, name, slot_type, int64_val, string_val, "
        "double_val, guid_val FROM slots WHERE obj_guid = ? AND name = ?",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return std::nullopt;
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, obj_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, name.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) == SQLITE_ROW) {
        return read_slot_row(stmt);
    }
    return std::nullopt;
}

Result<void> set_slot(sqlite3* db, const std::string& obj_guid,
                      const std::string& name, const std::string& value) {
    // Check if slot already exists
    auto existing = get_slot(db, obj_guid, name);
    if (existing) {
        // Update
        sqlite3_stmt* stmt = nullptr;
        int rc = sqlite3_prepare_v2(db,
            "UPDATE slots SET string_val = ?, slot_type = ? WHERE id = ?",
            -1, &stmt, nullptr);
        if (rc != SQLITE_OK)
            return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
        StmtGuard sg(stmt);

        sqlite3_bind_text(stmt, 1, value.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_int(stmt, 2, static_cast<int>(SlotType::STRING));
        sqlite3_bind_int(stmt, 3, existing->id);

        if (sqlite3_step(stmt) != SQLITE_DONE)
            return Result<void>::err(std::string("Update slot failed: ") + sqlite3_errmsg(db));
    } else {
        // Insert
        sqlite3_stmt* stmt = nullptr;
        int rc = sqlite3_prepare_v2(db,
            "INSERT INTO slots (obj_guid, name, slot_type, string_val) VALUES (?, ?, ?, ?)",
            -1, &stmt, nullptr);
        if (rc != SQLITE_OK)
            return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
        StmtGuard sg(stmt);

        sqlite3_bind_text(stmt, 1, obj_guid.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, name.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_int(stmt, 3, static_cast<int>(SlotType::STRING));
        sqlite3_bind_text(stmt, 4, value.c_str(), -1, SQLITE_STATIC);

        if (sqlite3_step(stmt) != SQLITE_DONE)
            return Result<void>::err(std::string("Insert slot failed: ") + sqlite3_errmsg(db));
    }
    return Result<void>::ok();
}

Result<void> set_slot_int(sqlite3* db, const std::string& obj_guid,
                          const std::string& name, int64_t value) {
    auto existing = get_slot(db, obj_guid, name);
    if (existing) {
        sqlite3_stmt* stmt = nullptr;
        int rc = sqlite3_prepare_v2(db,
            "UPDATE slots SET int64_val = ?, slot_type = ? WHERE id = ?",
            -1, &stmt, nullptr);
        if (rc != SQLITE_OK)
            return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
        StmtGuard sg(stmt);

        sqlite3_bind_int64(stmt, 1, value);
        sqlite3_bind_int(stmt, 2, static_cast<int>(SlotType::INTEGER));
        sqlite3_bind_int(stmt, 3, existing->id);

        if (sqlite3_step(stmt) != SQLITE_DONE)
            return Result<void>::err(std::string("Update slot failed: ") + sqlite3_errmsg(db));
    } else {
        sqlite3_stmt* stmt = nullptr;
        int rc = sqlite3_prepare_v2(db,
            "INSERT INTO slots (obj_guid, name, slot_type, int64_val) VALUES (?, ?, ?, ?)",
            -1, &stmt, nullptr);
        if (rc != SQLITE_OK)
            return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
        StmtGuard sg(stmt);

        sqlite3_bind_text(stmt, 1, obj_guid.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, name.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_int(stmt, 3, static_cast<int>(SlotType::INTEGER));
        sqlite3_bind_int64(stmt, 4, value);

        if (sqlite3_step(stmt) != SQLITE_DONE)
            return Result<void>::err(std::string("Insert slot failed: ") + sqlite3_errmsg(db));
    }
    return Result<void>::ok();
}

Result<void> delete_slot(sqlite3* db, const std::string& obj_guid,
                         const std::string& name) {
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db,
        "DELETE FROM slots WHERE obj_guid = ? AND name = ?",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK)
        return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, obj_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, name.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_DONE)
        return Result<void>::err(std::string("Delete slot failed: ") + sqlite3_errmsg(db));

    return Result<void>::ok();
}

std::vector<Slot> get_all_slots(sqlite3* db, const std::string& obj_guid) {
    std::vector<Slot> slots;
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db,
        "SELECT id, obj_guid, name, slot_type, int64_val, string_val, "
        "double_val, guid_val FROM slots WHERE obj_guid = ? ORDER BY name",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return slots;
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, obj_guid.c_str(), -1, SQLITE_STATIC);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        slots.push_back(read_slot_row(stmt));
    }
    return slots;
}

std::optional<std::string> find_split_by_fitid(sqlite3* db,
                                                const std::string& account_guid,
                                                const std::string& fitid) {
    // Join slots + splits: find a split in the given account whose
    // online_id slot matches the FITID
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db,
        "SELECT s.guid FROM splits s "
        "INNER JOIN slots sl ON sl.obj_guid = s.guid "
        "WHERE s.account_guid = ? "
        "AND sl.name = 'online_id' "
        "AND sl.string_val = ?",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return std::nullopt;
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, account_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, fitid.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) == SQLITE_ROW) {
        return col_text(stmt, 0);
    }
    return std::nullopt;
}

std::map<std::string, std::string> check_fitids(sqlite3* db,
                                                  const std::string& account_guid,
                                                  const std::vector<std::string>& fitids) {
    std::map<std::string, std::string> result;
    for (const auto& fitid : fitids) {
        auto split = find_split_by_fitid(db, account_guid, fitid);
        if (split) {
            result[fitid] = *split;
        }
    }
    return result;
}

} // namespace gnucash
