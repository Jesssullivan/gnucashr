// GnuCash type conversions

#include "gnucash/types.h"
#include <unordered_map>
#include <algorithm>

namespace gnucash {

namespace {
    const std::unordered_map<std::string, AccountType> type_map = {
        {"NONE", AccountType::NONE},
        {"BANK", AccountType::BANK},
        {"CASH", AccountType::CASH},
        {"CREDIT", AccountType::CREDIT},
        {"ASSET", AccountType::ASSET},
        {"LIABILITY", AccountType::LIABILITY},
        {"STOCK", AccountType::STOCK},
        {"MUTUAL", AccountType::MUTUAL},
        {"CURRENCY", AccountType::CURRENCY},
        {"INCOME", AccountType::INCOME},
        {"EXPENSE", AccountType::EXPENSE},
        {"EQUITY", AccountType::EQUITY},
        {"RECEIVABLE", AccountType::RECEIVABLE},
        {"PAYABLE", AccountType::PAYABLE},
        {"ROOT", AccountType::ROOT},
        {"TRADING", AccountType::TRADING},
    };

    const std::unordered_map<AccountType, std::string> type_reverse = {
        {AccountType::NONE, "NONE"},
        {AccountType::BANK, "BANK"},
        {AccountType::CASH, "CASH"},
        {AccountType::CREDIT, "CREDIT"},
        {AccountType::ASSET, "ASSET"},
        {AccountType::LIABILITY, "LIABILITY"},
        {AccountType::STOCK, "STOCK"},
        {AccountType::MUTUAL, "MUTUAL"},
        {AccountType::CURRENCY, "CURRENCY"},
        {AccountType::INCOME, "INCOME"},
        {AccountType::EXPENSE, "EXPENSE"},
        {AccountType::EQUITY, "EQUITY"},
        {AccountType::RECEIVABLE, "RECEIVABLE"},
        {AccountType::PAYABLE, "PAYABLE"},
        {AccountType::ROOT, "ROOT"},
        {AccountType::TRADING, "TRADING"},
    };
}

AccountType parse_account_type(const std::string& s) {
    std::string upper = s;
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);
    auto it = type_map.find(upper);
    if (it != type_map.end()) return it->second;
    return AccountType::NONE;
}

std::string account_type_to_string(AccountType t) {
    auto it = type_reverse.find(t);
    if (it != type_reverse.end()) return it->second;
    return "NONE";
}

} // namespace gnucash
