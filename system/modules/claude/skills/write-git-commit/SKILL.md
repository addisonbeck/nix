---
name: write-git-commit
description: |
  Creates high-quality git commits from uncommitted changes following conventional
  commit standards. Analyzes diffs, requires user-provided "why" context, matches
  local repository style, detects splitting opportunities, and executes pre-commit
  verification. Use when committing changes during development or reflect phases.
allowed-tools: Bash(~/.claude/skills/write-git-commit/*)
---

# write-git-commit Skill

When invoked, use this skill's instructions to guide the commit creation workflow. This is an instruction-only skill that provides the complete git commit workflow as a behavioral framework.

The skill orchestrates the full commit lifecycle: context gathering, diff analysis, commit message generation, pre-commit verification (hooks + formatting), staging, execution, and verification.

## Input Contract

Required:
- **why** (string): The motivation, reasoning, or context for this change. This is the core of the commit body. Without this, BLOCK and request it from the user.

Optional:
- **scope** (string): The component, module, or area affected (e.g., `auth`, `api`, `docs`). If not provided, infer from diff or omit.
- **type** (string): The conventional commit type (e.g., `feat`, `fix`, `refactor`, `docs`, `test`, `chore`). If not provided, infer from diff.
- **validate** (boolean): Set to `false` to skip secret detection (use cautiously, still warn user). Default: `true`.
- **split** (boolean): Set to `true` to enable split advisory mode (analyze and advise but do not commit). Default: `false`.
- **trailers** (array of strings): Additional commit trailers (e.g., `Refs: #123`, `Co-authored-by: ...`).

Validation Rules:
- `why` is required. If missing, BLOCK and ask user to provide it.
- `type` must be one of: feat, fix, refactor, docs, test, chore, perf, style (if provided)
- `memory_type` validation is enforced during diff analysis

## Output Contract

### Success Response

A git commit created with:
- **Commit hash**: SHA-1 hash of the created commit
- **Subject**: Conventional commit subject line (type(scope): description)
- **Body**: Multi-paragraph explanation of "why" the change was made
- **Verification**: Output of `git show HEAD` confirming commit creation

### Split Advisory Response (when split=true)

Recommendations for splitting uncommitted changes:
- **Concerns detected**: List of distinct concerns found in uncommitted changes
- **Recommended splits**: How to split changes into separate commits
- **Staging commands**: Specific `git add` commands for selective staging
- **Commit order**: Suggested order for creating split commits

### Error Response

Blocking conditions prevent commit creation:
- **error** (string): Description of blocking condition
- **reason** (string): Why commit was blocked
- **remediation** (array of strings): Steps to resolve the issue

Blocking conditions:
- No uncommitted changes (`git diff HEAD` is empty)
- Missing "why" context
- Secrets detected in uncommitted files
- Pre-commit hooks failed
- Formatting violations detected

### Examples

Success:
```
✓ Commit created successfully

Commit: a1b2c3d
Type: feat
Scope: auth
Subject: add FIDO2 authentication for Firefox

The commit has been verified with `git show HEAD`.
```

Split advisory:
```
I recommend splitting these uncommitted changes into 3 commits:

**Commit 1: API authentication logic** (feat)
- src/api/auth.ts
Why: Core authentication feature

**Commit 2: Login UI styling** (style)
- src/ui/login.css
Why: Visual improvements to login form

**Commit 3: Documentation update** (docs)
- docs/README.md
Why: Documentation for new auth feature
```

Error (missing why):
```
{"error": "Cannot create commit without 'why' context", "reason": "Diffs show 'how', only humans know 'why'", "remediation": ["Provide motivation, reasoning, or context for this change"]}
```

Error (secrets detected):
```
{"error": "Secrets detected in uncommitted files", "reason": "File .env contains API_KEY=sk_live_abc123...", "remediation": ["Remove secret from .env", "Add .env to .gitignore", "Use SOPS or git-crypt for secrets"]}
```

## Implementation Architecture

This is an **instruction-only skill** that provides a complete behavioral framework for git commit creation. There is no bash script implementation. Agents invoking this skill follow the workflow phases defined in this documentation.

### Workflow Structure

1. **Input Validation**: Extract parameters, check for "why" context, identify mode (commit vs split advisory)
2. **Context Gathering**: Run git commands in parallel to collect uncommitted changes, recent commit history, branch context
3. **Analysis**: Diff analysis, splitting detection, type inference
4. **Message Generation**: Construct conventional commit message following 50/72 rule and imperative mood
5. **Pre-Commit Verification**: Execute pre-commit hooks and formatting checks
6. **Message Presentation**: Show generated message to user (proceed directly without approval-seeking)
7. **Execution**: Stage all uncommitted changes with `git add -A` and commit with `--no-gpg-sign`
8. **Verification**: Confirm commit creation with `git show HEAD`

### Key Patterns

- **Error handling**: BLOCK on missing "why", no changes, or secrets detected
- **Staging strategy**: Always `git add -A` to stage all uncommitted changes before committing
- **Git flags**: Always use `--no-gpg-sign` (repository convention)
- **HEREDOC format**: Use HEREDOC for multi-line commit messages to handle bodies correctly
- **Style matching**: Reference recent `git log` to match local commit conventions
- **Amendment safety**: NEVER use `git commit --amend` by default (see Amendment Safety Pattern)
- **Formatter discovery**: Priority-ordered scan for project formatters (Nix treefmt, Rust cargo, Node.js prettier/eslint, Python ruff/black, Go gofmt)
- **Retry limits**: Maximum 2 formatting fix-and-retry cycles before escalating as configuration issue

### Amendment Safety Pattern

**No-Amend Default**: This skill NEVER uses `git commit --amend` by default. When a pre-commit hook fails, the commit did NOT happen -- so `--amend` would modify the PREVIOUS commit, not the failed one. This can silently destroy previous work.

**Bobert Authorization Override**: Bobert may explicitly authorize amendment with situational rationale. Authorization must be:
1. **Situational**: Specific reason why amendment is safe in this context
2. **Explicit**: The word "amend" must appear in delegation message
3. **Non-blanket**: General permission is NOT sufficient -- each amendment requires its own justification

**Proven Effectiveness**: Validated during Cycle 3 of 2026-03-04 retrospective workflow. The no-amend default prevented overwriting unrelated Cargo.lock changes when a pre-commit hook failed.

## Environment Dependencies

- **Git repository**: Must be executed within a git repository
- **Git tools**: git (for diff, log, status, add, commit, show commands)
- **jq**: For JSON parsing in bash scripts (if bash implementation is added later)
- **Pre-commit hooks** (optional): If configured in repository (git-hooks.nix, .pre-commit-config.yaml)
- **Project formatters** (optional): Nix treefmt, cargo, prettier, eslint, ruff, black, gofmt (discovered automatically)

No environment variables required.

## Usage & Testing Guidance

### Invocation from Agents

Agents invoke this skill by following the documented workflow phases. The skill provides instructions, not a bash script to execute.

**Example invocation (individual agent)**:
```
I need to create a commit for these changes.

Why: implements user authentication for Firefox extension using FIDO2 protocol

Following the write-git-commit skill workflow...
[Agent follows Phase 1-8 as documented]
```

**Example invocation from Bobert (orchestrator)**:
```
Bobert delegates to git-historian to create a high-quality commit.

Why: [work motivation and learnings from Reflect phase analysis]

git-historian will follow the write-git-commit skill workflow.
```

**Example split advisory**:
```
I want to check if these uncommitted changes should be split into multiple commits.

Following write-git-commit skill with split=true...
[Agent analyzes uncommitted changes and provides splitting recommendations]
```

### Workflow Phase Details

#### Phase 1: Input Validation

Extract parameters from conversational input:
- Check for "why" context
- If missing, BLOCK: "I cannot create a commit without understanding **why** this change is being made. Please provide the motivation, reasoning, or context for this work."
- Identify mode: `split=true` → advisory mode only, default → commit creation mode

#### Phase 2: Context Gathering

Run these git commands in parallel:

```bash
# See all uncommitted changes (both staged and unstaged)
git diff HEAD

# Understand recent commit style
git log --oneline -10

# Get detailed recent commits for style analysis
git log --format=fuller -3

# Check current branch
git branch --show-current

# See what files have uncommitted changes
git diff HEAD --name-only
```

Extract from output:
- Uncommitted file list
- Diff hunks by file
- Recent commit subjects (for style matching)
- Recent commit bodies (for style matching)
- Scope usage patterns
- Subject length patterns
- Body formatting conventions

#### Phase 3: Analysis

**A. Diff Analysis**
Parse uncommitted diff to understand:
- What files changed
- What kind of changes (new feature, bug fix, refactor, docs, tests)
- How many distinct concerns are present
- Whether changes are cohesive or scattered

**B. Splitting Detection**

Run heuristics to detect splitting opportunities:
1. **The "And" Test**: If you need "and" to describe changes, they may span multiple commits
2. **Type Test**: Do changes span multiple types? (feat + fix, refactor + docs)
3. **File Cohesion Test**: Are changed files related? (auth module + unrelated CSS change)
4. **Scope Test**: Do changes span multiple components/modules?
5. **Revertability Test**: Could you revert one concern without reverting another?

If 2+ heuristics trigger, ADVISE splitting:
- List detected concerns
- Suggest how to split
- Ask user: "Would you like me to explain how to split these changes, or proceed with a single commit?"

If `split=true`, provide splitting recommendations and STOP (do not commit).

**C. Type Inference**

If user did not provide `type`, infer from diff patterns:
- New files or features → `feat`
- Bug fixes, error handling → `fix`
- Restructuring without behavior change → `refactor`
- Documentation changes → `docs`
- Test additions/changes → `test`
- Build, CI, tooling → `chore`
- Performance improvements → `perf`
- Code style, formatting → `style`

#### Phase 4: Message Generation

Construct commit message following this structure:

```
<type>(<scope>): <subject>

<body paragraph 1: why this change>

<body paragraph 2: additional context, tradeoffs, or details>

<trailers>
```

**Subject line:**
- Start with type (inferred or provided)
- Add scope in parens if applicable: `feat(auth):` or omit if not: `docs:`
- Write imperative mood, present tense: "Add", "Fix", "Refactor"
- Keep ≤50 characters
- Do NOT end with period
- Focus on WHAT at high level, not HOW (save implementation details for body)

**Body:**
- Start with user-provided "why" context (motivation, reasoning)
- Wrap lines at 72 characters
- Use blank line between paragraphs
- Add bullet lists for complex details if appropriate
- Reference issue numbers, tickets, or related work if mentioned by user
- Explain tradeoffs, alternative approaches considered, or non-obvious decisions

**Trailers:**
- Add `Refs: #<issue>` if user mentioned issue numbers
- Add `Co-authored-by:` if user mentioned collaborators
- Add any custom trailers provided by user

**Style Matching:**
Reference recent commit history from Phase 2 to match local conventions:
- If recent commits omit scope, consider omitting it
- If recent commits use bullet lists in body, use that format
- If recent subjects average 40 chars, aim for that length
- Match the tone (terse vs. verbose)

#### Phase 5: Pre-Commit Verification

Execute verification in three sub-phases:

**Phase 5A: Pre-Commit Hooks**

Execute pre-commit hooks configured in git-hooks.nix:

```bash
# For Nix repositories with git-hooks.nix
nix flake check

# Or direct pre-commit execution
pre-commit run --all-files
```

- Exit code 0: All checks passed, proceed to Phase 5B
- Exit code ≠ 0: Checks failed, block commit creation

If checks fail:
- Report failures with full context (which hooks failed, error messages)
- Suggest fixes based on failure type
- Present failure details to user
- Do NOT create commit until checks pass

**Phase 5B: Formatter Discovery**

Discover project-specific formatting tools by scanning repository root:

| Priority | Ecosystem | Config File(s) | Check Command | Fix Command (for reporting only) |
|----------|-----------|-----------------|---------------|----------------------------------|
| 1 | Nix (treefmt) | `flake.nix` containing `treefmt` or `formatting` | `nix develop .#formatting --command check formatting` | `nix develop .#formatting --command apply formatting` |
| 2 | Rust | `Cargo.toml` | `cargo fmt --check` | `cargo fmt` |
| 3 | Node.js (prettier) | `package.json` containing `prettier` in devDependencies, or `.prettierrc*` | `npx prettier --check .` | `npx prettier --write .` |
| 4 | Node.js (eslint) | `package.json` containing `eslint` in devDependencies, or `.eslintrc*` | `npx eslint --max-warnings 0 .` | `npx eslint --fix .` |
| 5 | Python (ruff) | `pyproject.toml` containing `ruff`, or `ruff.toml` | `ruff format --check .` | `ruff format .` |
| 6 | Python (black) | `pyproject.toml` containing `black` | `black --check .` | `black .` |
| 7 | Go | `go.mod` | `gofmt -l .` (non-empty output = violations) | `gofmt -w .` |

Discovery process:
1. Scan repository root for configuration files
2. For each detected config file, confirm formatter is actually configured
3. Record all discovered formatters with their check commands
4. If no formatters discovered: Skip Phase 5C and proceed to Phase 6

**Phase 5C: Formatting Check Execution**

Execute all formatters discovered in Phase 5B in check/dry-run mode. This sub-phase MUST NOT modify any files.

Execution process:
1. Run each discovered formatter's check command in sequence
2. Collect results: Track which formatters passed and which failed
3. If all formatters pass: Proceed to Phase 6
4. If any formatter fails: Invoke Formatting Failure Protocol

**Formatting Failure Protocol:**

When formatting violations detected:

**Standalone Mode** (skill invoked directly by user):
1. Report specific formatting violations with full output
2. Report fix command for each failing formatter
3. Block commit creation -- do NOT proceed to Phase 6
4. Track retry count for this commit attempt

**Pipeline Mode** (skill invoked by coordinator or another agent):
1. Report formatting violations to coordinating agent
2. Include specific fix command for each failing formatter
3. Recommend coordinator delegate fix to code-monkey
4. Block commit creation -- return failure status to coordinator

**Retry Limits:**
- Maximum 2 formatting fix-and-retry cycles per commit attempt
- After 2 failed cycles, escalate as non-retriable configuration issue
- Do NOT attempt a third retry

**Timeout Handling:**
- If formatting check does not complete within 60 seconds, kill process
- Log warning: "Formatter X timed out after 60 seconds, skipping"
- Proceed with remaining formatters
- Do NOT block commit solely due to timeout (treat as warning)

#### Phase 6: Message Presentation

Present generated commit message to user:

```
I've generated this commit message:

```
<type>(<scope>): <subject>

<body>

<trailers>
```

This follows the repository's commit style based on recent history.

Does this accurately capture the change? Reply "yes" to commit, or provide feedback to revise.
```

**Default behavior**: Proceed directly to Phase 7 (Execution) without waiting for approval. The message is shown for transparency and user can interrupt if needed, but approval-seeking creates unnecessary friction.

**Exception**: If user explicitly requests review mode (e.g., "draft a commit message for review"), present message and wait for explicit approval before executing.

#### Phase 7: Execution

Stage all uncommitted changes and execute commit:

```bash
# Stage all uncommitted changes
git add -A

# Create the commit
git commit --no-gpg-sign -m "$(cat <<'EOF'
<type>(<scope>): <subject>

<body>

<trailers>
EOF
)"
```

Important:
- First run `git add -A` to stage all uncommitted changes (both tracked modifications and untracked files)
- Then execute commit command
- Always use HEREDOC format for commit message to handle multi-line bodies correctly
- Always include `--no-gpg-sign` flag (repository convention)
- Capture stderr in case of commit hook failures

If staging or commit fails:
- Report error to user
- If `git add -A` failed, explain what went wrong (merge conflicts, permission issues)
- If pre-commit hook failed, explain what needs to be fixed
- Do NOT attempt to fix files yourself (read-only constraint)
- Suggest user fix issue and re-invoke

#### Phase 8: Verification

After successful commit, verify by running:

```bash
git show HEAD
```

Present commit details to user:
- Commit hash
- Author and date
- Full commit message
- Abbreviated diff

Confirm: "Commit created successfully: `<hash>`"

### Conventional Commit Format Reference

**Structure:**
```
<type>(<scope>): <subject>

<body paragraph 1>

<body paragraph 2>

<trailers>
```

**Types:**
- **feat**: New feature or enhancement
- **fix**: Bug fix
- **refactor**: Code restructuring without behavior change
- **docs**: Documentation changes
- **test**: Test additions or modifications
- **chore**: Build, tooling, dependencies, CI
- **perf**: Performance improvements
- **style**: Code style, formatting (no logic change)

**Scope Examples:**
- Component: `auth`, `api`, `ui`, `cli`
- Module: `parser`, `validator`, `router`
- Area: `docs`, `tests`, `config`
- Omit scope if change is global or scope is unclear

**Subject Line Rules:**
1. Imperative mood: "Add feature" not "Added feature"
2. No period at end
3. ≤50 characters
4. Capitalize first word
5. High-level WHAT, not HOW

**Body Rules:**
1. Explain WHY, not what (diff shows what)
2. Wrap at 72 characters
3. Blank line between subject and body
4. Blank lines between paragraphs
5. Bullet lists acceptable for complex details
6. Reference issues with `Refs: #123`

### Blocking and Warning Conditions

**Blocking Conditions (Refuse to Commit):**
1. **No uncommitted changes**: `git diff HEAD` is empty
2. **Missing "why" context**: User has not provided motivation/reasoning
3. **Secrets detected**: Uncommitted files contain patterns suggesting secrets:
   - File names: `.env`, `.env.*`, `credentials.json`, `*_secret`, `*_key`, `id_rsa`, `*.pem`
   - Content patterns: `API_KEY=`, `SECRET=`, `password=`, `token=`, `-----BEGIN PRIVATE KEY-----`

**Warning Conditions (Warn but Allow):**
1. **Large commit**: >500 lines changed (suggest splitting if cohesive)
2. **Many files**: >10 files with uncommitted changes (suggest splitting by concern)
3. **Mixed concerns detected**: Splitting heuristics triggered but user wants single commit
4. **Ambiguous type**: Cannot confidently infer type from diff

### Error Handling Examples

**No uncommitted changes:**
```
No uncommitted changes to commit. The working tree is clean.

If you have changes you want to commit, make your modifications and
re-invoke me with the "why" context for this commit.
```

**Staging or commit fails:**
```
Commit failed due to pre-commit hook error:

[stderr output]

This is a formatting issue. Please run the formatter:
  nix develop .#formatting --command apply formatting

Then re-invoke me to retry the commit.
```

**Ambiguous type:**
```
I'm uncertain whether this is a "feat" or "refactor" change.

The diff shows [explanation of ambiguity].

Please clarify the type, or I can proceed with "refactor" as default.
```

### Installation

This skill is installed automatically via Nix system rebuild:

```bash
# Rebuild to install skill
nix develop .#building --command rebuild <hostname>

# Verify installation
ls ~/.claude/skills/write-git-commit/

# The skill is instruction-only, so there is no .sh script to test
# Agents follow the documented workflow when invoking this skill
```
