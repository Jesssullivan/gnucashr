#pragma once
// GnuCash slots table - key-value metadata on entities
// Slots store online_id (FITID), notes, and other per-entity metadata
// Uses the exact GnuCash 5.x schema for compatibility

#include "result.h"
#include <string>
#include <vector>
#include <optional>
#include <map>

struct sqlite3;  // forward declare

namespace gnucash {

// Slot value types (GnuCash schema: slot_type column)
// 1=int64, 2=double, 4=string, 5=guid, 9=list, 10=frame
enum class SlotType {
    INTEGER  = 1,
    DOUBLE   = 2,
    STRING   = 4,
    GUID     = 5,
    LIST     = 9,
    FRAME    = 10
};

struct Slot {
    int id;
    std::string obj_guid;
    std::string name;
    SlotType slot_type;
    std::string string_val;       // slot_type == STRING
    int64_t int64_val = 0;        // slot_type == INTEGER
    double double_val = 0.0;      // slot_type == DOUBLE
    std::string guid_val;         // slot_type == GUID
};

// Ensure the slots table exists (CREATE TABLE IF NOT EXISTS)
// Safe to call on databases that already have the table
Result<void> ensure_slots_table(sqlite3* db);

// Get a single slot value by entity GUID and slot name
std::optional<Slot> get_slot(sqlite3* db, const std::string& obj_guid,
                             const std::string& name);

// Set a string slot value (creates or updates)
Result<void> set_slot(sqlite3* db, const std::string& obj_guid,
                      const std::string& name, const std::string& value);

// Set an integer slot value (creates or updates)
Result<void> set_slot_int(sqlite3* db, const std::string& obj_guid,
                          const std::string& name, int64_t value);

// Delete a slot
Result<void> delete_slot(sqlite3* db, const std::string& obj_guid,
                         const std::string& name);

// Get all slots for an entity
std::vector<Slot> get_all_slots(sqlite3* db, const std::string& obj_guid);

// Find a split GUID by FITID (online_id) within an account
// This is the core dedup function: joins slots + splits to find existing imports
// The slot key is "online_id" and the value contains the FITID
std::optional<std::string> find_split_by_fitid(sqlite3* db,
                                                const std::string& account_guid,
                                                const std::string& fitid);

// Batch check: which FITIDs already exist for an account?
// Returns a map of fitid -> split_guid for matches
std::map<std::string, std::string> check_fitids(sqlite3* db,
                                                  const std::string& account_guid,
                                                  const std::vector<std::string>& fitids);

} // namespace gnucash
