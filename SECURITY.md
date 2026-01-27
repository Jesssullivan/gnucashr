# Security Policy

## Supported Versions

The following versions of gnucashr receive security updates:

| Version | Supported |
|---------|-----------|
| 0.2.x   | ✅        |
| 0.1.x   | ❌        |

We recommend always using the latest release.

## Reporting a Vulnerability

If you discover a security vulnerability in gnucashr, please report it
responsibly:

1.  **Do NOT open a public issue** for security vulnerabilities
2.  **Email:** Send details to <jess@sulliwood.org>
3.  **Include:**
    - Description of the vulnerability
    - Steps to reproduce
    - Potential impact
    - Any suggested fixes (optional)

### Response Timeline

- **Acknowledgment:** Within 48 hours
- **Initial assessment:** Within 1 week
- **Resolution timeline:** Depends on severity, typically 2-4 weeks

### What to Expect

- We will acknowledge receipt of your report
- We will investigate and assess the severity
- We will work with you to understand the issue
- We will develop and test a fix
- We will credit you in the release notes (unless you prefer anonymity)

## Security Considerations for Users

### GnuCash File Handling

gnucashr reads GnuCash files directly. Be aware of these security
considerations:

#### File Permissions

GnuCash files may contain sensitive financial data. Ensure appropriate
file permissions:

``` r
# Check file permissions before reading
file.info("my-finances.gnucash")$mode
```

#### SQLite Files

- gnucashr opens SQLite files in read-only mode by default
- Write operations require explicit opt-in
- Always back up files before enabling write operations

``` r
# Read-only (default, safe)
db <- GnuCashDB$new("finances.gnucash")

# Write-enabled (use with caution)
db <- GnuCashDB$new("finances.gnucash", read_only = FALSE)
```

#### XML Files

- XML parsing uses the `xml2` package
- gnucashr does not fetch external entities (XXE protection)
- Large XML files are loaded into memory; consider available RAM

### Credential Management

If you use gnucashr with remote databases or APIs:

- **Never hardcode credentials** in scripts
- Use environment variables or secure credential stores
- Consider the `keyring` package for secure credential storage

``` r
# Good: Use keyring
library(keyring)
password <- key_get("gnucash_db", "username")

# Bad: Hardcoded credentials
password <- "my_secret_password"  # Never do this!
```

### Multi-User Environments

When using gnucashr in shared environments:

- Restrict file permissions appropriately
- Be cautious with write operations
- Consider user isolation for sensitive analyses

### Logging and Output

Be careful when logging or printing data that might contain:

- Account names that reveal financial institutions
- Transaction descriptions with sensitive information
- Balance amounts

``` r
# Be cautious with output in shared environments
# This might expose sensitive data:
print(account_tree(db))

# Consider filtering sensitive accounts before display
```

## Security in Rcpp Code

### Memory Safety

The C++ components (Rcpp, RcppParallel, RcppArmadillo) are designed with
safety in mind:

- Bounds checking on vector access
- RAII patterns for resource management
- No use of raw `new`/`delete`

### Input Validation

All user inputs are validated before processing in C++ code:

- Numeric ranges are checked
- String inputs are sanitized
- NULL/NA values are handled explicitly

## Dependencies

gnucashr depends on well-maintained packages. We monitor for security
advisories in:

- Rcpp ecosystem (Rcpp, RcppParallel, RcppArmadillo)
- Database interfaces (DBI, RSQLite)
- XML parsing (xml2)

Update dependencies regularly:

``` r
# Update all dependencies
update.packages(ask = FALSE)

# Or with renv
renv::update()
```

## Security Best Practices

1.  **Keep gnucashr updated** to receive security fixes
2.  **Protect your GnuCash files** with appropriate permissions
3.  **Back up before write operations**
4.  **Use read-only mode** unless writes are necessary
5.  **Avoid logging sensitive data** in shared environments
6.  **Use secure credential storage** for any authentication
