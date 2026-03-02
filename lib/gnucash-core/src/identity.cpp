#include "gnucash/identity.h"
#include <cstdlib>
#include <unistd.h>
#include <pwd.h>
#include <climits>

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 256
#endif

namespace gnucash {

std::string get_system_username() {
    // Try USER env first (most portable)
    const char* user = std::getenv("USER");
    if (user && user[0] != '\0') {
        return user;
    }

    // Fall back to getpwuid
    struct passwd* pw = getpwuid(getuid());
    if (pw && pw->pw_name) {
        return pw->pw_name;
    }

    return "unknown";
}

std::string get_hostname() {
    char hostname[HOST_NAME_MAX + 1] = {};
    if (gethostname(hostname, sizeof(hostname)) == 0) {
        hostname[HOST_NAME_MAX] = '\0';
        return hostname;
    }
    return "localhost";
}

Identity resolve_identity(const std::optional<std::string>& cli_identity) {
    Identity id;
    id.node_name = get_hostname();

    // Priority 1: CLI flag
    if (cli_identity.has_value() && !cli_identity->empty()) {
        id.user_id = *cli_identity;
        id.display_name = *cli_identity;
        id.source = "cli";
        return id;
    }

    // Priority 2: GNUCASH_USER env var
    const char* env_user = std::getenv("GNUCASH_USER");
    if (env_user && env_user[0] != '\0') {
        id.user_id = env_user;
        id.display_name = env_user;
        id.source = "env";
        return id;
    }

    // Priority 3: System username
    id.user_id = get_system_username();
    id.display_name = id.user_id;
    id.source = "system";
    return id;
}

} // namespace gnucash
