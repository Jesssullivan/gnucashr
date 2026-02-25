# Repository Setup Guide

This document describes the git remote configuration, mirroring strategy, and repository management for gnucashr.

## Remote Configuration

gnucashr uses a dual-remote setup with GitHub as the primary repository and GitLab as a mirror.

### Remote Overview

| Remote | URL | Purpose |
|--------|-----|---------|
| `origin` | `github.com/Jesssullivan/gnucashr` | Primary development |
| `gitlab` | `gitlab.com/tinyland/oss/gnucashr` | Mirror / CI |

### Setting Up Remotes

After cloning from GitHub:

```bash
# Clone from GitHub (primary)
git clone https://github.com/Jesssullivan/gnucashr.git
cd gnucashr

# Add GitLab as secondary remote
git remote add gitlab https://gitlab.com/tinyland/oss/gnucashr.git

# Verify remotes
git remote -v
```

Expected output:
```
origin  https://github.com/Jesssullivan/gnucashr.git (fetch)
origin  https://github.com/Jesssullivan/gnucashr.git (push)
gitlab  https://gitlab.com/tinyland/oss/gnucashr.git (fetch)
gitlab  https://gitlab.com/tinyland/oss/gnucashr.git (push)
```

## Mirroring Strategy

### Automatic Mirroring (Recommended)

GitLab can automatically pull from GitHub using repository mirroring:

1. In GitLab project settings, navigate to Repository > Mirroring repositories
2. Add GitHub URL as pull mirror
3. Set update frequency (hourly recommended)
4. Enable "Mirror only protected branches" for production

### Manual Push to Both Remotes

For contributors with push access to both remotes:

```bash
# Push to GitHub (primary)
git push origin main

# Push to GitLab (mirror)
git push gitlab main
```

### Push to All Remotes Simultaneously

Configure git to push to multiple remotes:

```bash
# Add GitLab as additional push URL for origin
git remote set-url --add --push origin https://github.com/Jesssullivan/gnucashr.git
git remote set-url --add --push origin https://gitlab.com/tinyland/oss/gnucashr.git

# Now 'git push origin main' pushes to both
```

Verify with:
```bash
git remote -v
# Should show multiple push URLs for origin
```

## GitLab Public Repository Requirements

### Visibility Hierarchy

GitLab enforces a strict visibility hierarchy:

- Projects cannot be more visible than their parent group
- A public project requires all parent groups to be public

### Recommended Structure

For public open-source projects, create a dedicated public subgroup:

```
gitlab.com/tinyland           (private group - org default)
  |
  +-- oss/                    (public subgroup)
       |
       +-- gnucashr           (public project)
       +-- other-oss-project  (public project)
```

### Creating the Public Subgroup

1. Navigate to the parent group (e.g., `gitlab.com/tinyland`)
2. Click "New subgroup"
3. Name: `oss` (or `open-source`)
4. Visibility: **Public**
5. Create subgroup
6. Create or transfer projects into this subgroup

## Branch Protection

### Recommended Branch Protection Rules

#### GitHub

Settings > Branches > Branch protection rules:

**For `main` branch:**
- Require pull request reviews before merging
- Require status checks to pass (R CMD check, tests)
- Require branches to be up to date
- Include administrators
- Do not allow force pushes
- Do not allow deletions

#### GitLab

Settings > Repository > Protected branches:

**For `main` branch:**
- Allowed to merge: Maintainers
- Allowed to push: No one (force PR workflow)
- Allowed to force push: No one

## CI/CD Configuration

### GitHub Actions

Primary CI runs on GitHub Actions:
- R CMD check on multiple platforms
- Test coverage reporting
- CRAN submission checks

### GitLab CI

Mirror CI on GitLab (optional):
- Can use shared runners
- Useful for integration with other GitLab projects
- Configure in `.gitlab-ci.yml`

## Authentication

### SSH Keys

For push access, configure SSH keys for both platforms:

```bash
# GitHub
ssh-keygen -t ed25519 -C "github-gnucashr"
# Add to GitHub > Settings > SSH keys

# GitLab
ssh-keygen -t ed25519 -C "gitlab-gnucashr"
# Add to GitLab > Preferences > SSH keys
```

Use SSH URLs for remotes:
```bash
git remote set-url origin git@github.com:Jesssullivan/gnucashr.git
git remote set-url gitlab git@gitlab.com:tinyland/oss/gnucashr.git
```

### Personal Access Tokens

For HTTPS with 2FA enabled:

**GitHub:**
- Settings > Developer settings > Personal access tokens
- Scope: `repo`

**GitLab:**
- Preferences > Access Tokens
- Scope: `read_repository`, `write_repository`

## Troubleshooting

### Push Rejected on GitLab

If GitLab rejects pushes due to visibility:
```
remote: GitLab: You are not allowed to push code to this project.
```

Verify:
1. Project visibility matches group visibility
2. You have Maintainer or Owner role
3. Branch is not protected against your role

### Sync Conflicts

If mirrors diverge:
```bash
# Reset GitLab to match GitHub
git fetch origin
git push gitlab origin/main:main --force
```

**Warning:** Only force push to fix mirror sync issues, never for normal development.

### Mirror Lag

GitLab pull mirrors may have up to 5-minute delay. For immediate sync:
1. Go to GitLab project
2. Settings > Repository > Mirroring repositories
3. Click "Update now" button
