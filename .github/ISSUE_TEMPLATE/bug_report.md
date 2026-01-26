---
name: Bug Report
about: Report a bug or unexpected behavior
title: '[BUG] '
labels: bug
assignees: ''
---

## Bug Description

A clear and concise description of the bug.

## Environment

- **R version**: [e.g., 4.3.2]
- **gnucashr version**: [e.g., 0.2.0]
- **Operating System**: [e.g., Ubuntu 22.04, macOS 14.0, Windows 11]
- **GnuCash version** (if relevant): [e.g., 5.4]
- **GnuCash file format**: [SQLite / XML / Compressed XML]

## Steps to Reproduce

1. Open GnuCash file with `read_gnucash("...")`
2. Call function `...`
3. See error

## Minimal Reproducible Example

```r
library(gnucashr)

# Minimal code that reproduces the issue
gc <- read_gnucash("example.gnucash")
result <- problematic_function(gc, ...)
```

## Expected Behavior

What you expected to happen.

## Actual Behavior

What actually happened. Include error messages if applicable:

```
Error in function_name():
  error message here
```
## Additional Context

- Does this happen with all GnuCash files or specific ones?
- Did this work in a previous version?
- Any relevant account structure or transaction details (anonymized)?

## Checklist

- [ ] I have searched existing issues for duplicates
- [ ] I have included a minimal reproducible example
- [ ] I have included the full error message/traceback
- [ ] I am using the latest version of gnucashr
