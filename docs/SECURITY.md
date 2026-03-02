# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.2.x   | :white_check_mark: |
| < 0.2   | :x:                |

Only the latest minor version receives security updates. We recommend always using the most recent release.

## Reporting a Vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

### Reporting Process

1. **Email**: Send details to the maintainer privately
2. **Subject**: Include "SECURITY: gnucashr" in the subject line
3. **Include**:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact assessment
   - Suggested fix (if available)

### Response Timeline

- **Initial response**: Within 48 hours
- **Status update**: Within 7 days
- **Fix timeline**: Depends on severity (see below)

### Severity Levels

| Severity | Response Time | Examples |
|----------|---------------|----------|
| Critical | 24-48 hours | Remote code execution, data exfiltration |
| High | 1 week | SQL injection, authentication bypass |
| Medium | 2 weeks | Information disclosure, denial of service |
| Low | Next release | Minor information leaks, hardening improvements |

## Security Considerations for Financial Data

gnucashr handles sensitive financial data. Users and developers should be aware of:

### Data at Rest

- GnuCash files (SQLite/XML) may contain sensitive financial information
- Always store GnuCash files with appropriate file system permissions
- Consider encrypting the underlying storage

### Data in Transit

- gnucashr operates locally and does not transmit data over networks
- If integrating with remote services, use encrypted connections (HTTPS/TLS)

### Memory Handling

- Financial data is loaded into R memory during operations
- Clear sensitive objects when no longer needed:
  ```r
  gc <- read_gnucash("books.gnucash")
  # ... use the data ...
  close(gc)
  rm(gc)
  gc()  # Force garbage collection
  ```

### Logging and Output

- Be cautious with debugging output that may contain account names or balances
- Avoid logging transaction details to shared log files
- The `Logged` monad provides audit trails - store securely

## No Secrets in Code Policy

### Prohibited Content

The following must NEVER be committed to this repository:

- API keys or tokens
- Passwords or passphrases
- Private keys (SSH, GPG, TLS)
- Database connection strings with credentials
- Real financial data or GnuCash files with actual transactions
- Personal identification information

### Test Data Requirements

- Use synthetic/generated test data only
- Test GnuCash files must contain fictional accounts and transactions
- Anonymize any data derived from real sources

### Pre-commit Checks

Contributors should use git hooks to prevent accidental commits:

```bash
# Example pre-commit hook patterns to block
*.gnucash        # Real GnuCash files (use fixtures/ only)
.env*            # Environment files
*_secret*        # Secret files
*_key*           # Key files
*.pem            # Certificates
```

## Secure Development Practices

### Dependencies

- Regularly update dependencies for security patches
- Review new dependencies before adding
- Prefer well-maintained packages with security track records

### C++ Code (Rcpp)

- Validate all inputs from R before processing
- Use bounds checking on arrays and vectors
- Avoid raw pointers where possible
- Handle memory allocation failures gracefully

### SQL Queries

- gnucashr uses parameterized queries via RSQLite
- Never construct SQL strings with user input directly
- Validate account paths before database queries

## Acknowledgments

We appreciate responsible disclosure of security issues. Contributors who report valid security vulnerabilities will be acknowledged (unless they prefer to remain anonymous).

## Updates to This Policy

This security policy may be updated periodically. Check the repository for the latest version.
