# CRAN Submission Workflow for gnucashr

## Overview

This document outlines the CRAN submission process for gnucashr, including automated checks via GitLab CI and manual submission steps.

## Pre-Submission Checklist

### Automated Checks (GitLab CI)

These run on every push to `main`:

```yaml
# .gitlab-ci.yml stages
stages:
  - build
  - test
  - check
  - deploy
```

| Check | Stage | Command |
|-------|-------|---------|
| R CMD check | check | `devtools::check(cran = TRUE)` |
| Test suite | test | `devtools::test()` |
| Coverage | test | `covr::package_coverage()` |
| Spell check | check | `spelling::spell_check_package()` |
| URL validation | check | `urlchecker::url_check()` |

### Manual Pre-Submission Checks

Run these locally before submitting:

```r
# 1. Full CRAN check (strict)
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)

# 2. Check on multiple platforms via r-hub
rhub::check_for_cran()

# 3. Check on win-builder
devtools::check_win_devel()
devtools::check_win_release()

# 4. Check on mac-builder (if available)
devtools::check_mac_release()

# 5. Reverse dependency check (after first release)
revdepcheck::revdep_check()

# 6. goodpractice audit
goodpractice::gp()
```

## Submission Process

### Step 1: Version Bump

```r
# In DESCRIPTION, update:
Version: 0.3.0

# Update NEWS.md with release notes
# Update cran-comments.md with test environments
```

### Step 2: Final Local Check

```bash
R CMD build .
R CMD check gnucashr_0.3.0.tar.gz --as-cran
```

### Step 3: Submit to CRAN

Option A - Web submission (recommended for first submission):
1. Go to https://cran.r-project.org/submit.html
2. Upload tarball
3. Provide maintainer email
4. Paste cran-comments.md content

Option B - devtools submission:
```r
devtools::submit_cran()
```

### Step 4: Monitor Submission

- Check email for CRAN feedback
- Monitor https://cransays.itsalocke.com/articles/dashboard.html
- Typical turnaround: 1-5 business days

## GitLab CI CRAN-Ready Pipeline

### Current Configuration

```yaml
# .gitlab-ci.yml - CRAN check job
cran-check:
  stage: check
  image: rocker/tidyverse:latest
  script:
    - |
      Rscript -e '
        options(repos = c(CRAN = "https://cloud.r-project.org"))

        # Install dependencies
        remotes::install_deps(dependencies = TRUE)

        # Run CRAN check
        results <- devtools::check(cran = TRUE, error_on = "error")

        # Fail on warnings for CRAN submission
        if (length(results$warnings) > 0) {
          message("Warnings found:")
          print(results$warnings)
          quit(status = 1)
        }
      '
  artifacts:
    paths:
      - "*.Rcheck/"
    when: always
    expire_in: 1 week
  rules:
    - if: $CI_COMMIT_BRANCH == "main"
    - if: $CI_COMMIT_TAG
```

### Release Pipeline (Tag-Triggered)

Add to `.gitlab-ci.yml`:

```yaml
release-prep:
  stage: deploy
  image: rocker/tidyverse:latest
  script:
    - |
      Rscript -e '
        # Build source package
        pkg <- devtools::build()

        # Verify package name
        message("Built: ", pkg)

        # Run final checks
        devtools::check_built(pkg, cran = TRUE)

        # Check on r-hub (optional, requires RHUB_TOKEN)
        # rhub::check_for_cran(pkg)
      '
  artifacts:
    paths:
      - "gnucashr_*.tar.gz"
    expire_in: 30 days
  rules:
    - if: $CI_COMMIT_TAG =~ /^v[0-9]+\.[0-9]+\.[0-9]+$/
```

## Continuous Delivery Strategy

### Semi-Automated CD (Recommended)

CRAN does not support fully automated submissions. The recommended workflow:

1. **Development** (`main` branch)
   - Continuous R CMD check
   - Coverage reporting
   - pkgdown site deployment

2. **Release Candidate** (tag `v0.3.0-rc1`)
   - Triggers extended checks (r-hub, win-builder)
   - Builds release tarball
   - Generates submission artifacts

3. **Release** (tag `v0.3.0`)
   - Manual CRAN submission with artifacts
   - Update GitHub/GitLab releases
   - Announce release

### GitLab Release Integration

```yaml
create-release:
  stage: deploy
  image: registry.gitlab.com/gitlab-org/release-cli:latest
  script:
    - echo "Creating release for $CI_COMMIT_TAG"
  release:
    tag_name: $CI_COMMIT_TAG
    name: "gnucashr $CI_COMMIT_TAG"
    description: |
      ## gnucashr $CI_COMMIT_TAG

      See NEWS.md for changes.

      ### Installation
      ```r
      remotes::install_gitlab("tinyland/projects/gnucashr@$CI_COMMIT_TAG")
      ```
    assets:
      links:
        - name: "Source Package"
          url: "$CI_PROJECT_URL/-/jobs/artifacts/$CI_COMMIT_TAG/raw/gnucashr_*.tar.gz?job=release-prep"
  rules:
    - if: $CI_COMMIT_TAG =~ /^v[0-9]+\.[0-9]+\.[0-9]+$/
```

## CRAN Policies to Remember

### File Size Limits
- Source package: < 5MB (we're well under)
- Installed package: < 50MB

### Check Requirements
- 0 errors, 0 warnings required
- Notes acceptable but should be explained in cran-comments.md

### Timing
- First submission: Allow 1-2 weeks for review
- Updates: 1-5 business days typical
- Don't resubmit within 24 hours unless requested

### Common Rejection Reasons
1. Examples that modify user environment
2. Missing LICENSE file for MIT
3. URLs that don't resolve
4. Non-standard file permissions
5. Overly long check times (>10 min)

## Post-Acceptance Tasks

1. **Tag release in git**
   ```bash
   git tag -a v0.3.0 -m "CRAN release 0.3.0"
   git push origin v0.3.0
   git push upstream v0.3.0
   ```

2. **Update pkgdown site**
   - CI automatically deploys on tag

3. **Announce**
   - R-bloggers (optional)
   - Mastodon/Twitter
   - R Weekly submission

4. **Monitor reverse dependencies**
   - Set up alerts for packages depending on gnucashr

## Quick Reference

### Submission Commands
```r
# Build tarball
devtools::build()

# Check locally
devtools::check(cran = TRUE)

# Submit (after manual approval)
devtools::submit_cran()
```

### Monitoring URLs
- CRAN incoming: https://cransays.itsalocke.com/articles/dashboard.html
- CRAN check results: https://cran.r-project.org/web/checks/check_results_gnucashr.html
- Package page (after acceptance): https://cran.r-project.org/package=gnucashr

## Version History

| Version | Date | Status |
|---------|------|--------|
| 0.2.0 | 2026-01-15 | Initial development |
| 0.3.0 | TBD | First CRAN submission planned |
