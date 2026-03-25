---
name: extract-nx-lib
description: |
  Orchestration playbook for extracting a leaf library from the bitwarden-clients Nx monorepo. Encodes all patterns, team composition, decision frameworks, and gotchas for taking a candidate file from libs/common (or another broad library) into its own @bitwarden/<name> Nx leaf library with a re-export shim and at least one updated consumer. Use when asked to extract a class, utility, or type from bitwarden-clients into a standalone Nx leaf lib using Task Group A.
---

# extract-nx-lib Skill

This skill provides orchestration guidance for extracting a leaf library from the bitwarden-clients Nx monorepo. A leaf library is a small, self-contained domain with zero or near-zero dependencies on other `@bitwarden/*` packages — it sits at the bottom of the dependency graph.

When invoked, Bobert follows this instruction playbook to coordinate intake, research/design, implementation, finalization, and publishing phases using the full-lifecycle-delivery team pattern, augmented with extraction-specific guidance for each phase.

**Critical domain knowledge**: This skill encodes hard-won patterns from the RangeWithDefault extraction session. Every section below reflects decisions and gotchas discovered doing this work. Treat them as authoritative.

## Input Contract

This is an instruction-only skill. No JSON input required.

**Bobert receives** (from context):
- Extraction candidate: either a specific file path or a broad description ("extract something from platform/misc")
- Worktree context: which bitwarden-clients worktree to work in
- Gap analysis responses from Addison (naming, team ownership, import strategy are standard deferred-to-research items)

## Output Contract

This skill produces **guidance behavior**, not data output.

**The extraction workflow produces**:
- A scaffolded Nx library at `libs/<name>/` with populated `src/index.ts` and moved spec file
- A re-export shim at the original file path (if not barrel-exported) or a barrel update (if barrel-exported)
- At least one consumer updated to import from `@bitwarden/<name>`
- ADRs documenting library name, team ownership, and import strategy
- A draft PR with clean commit history (prerequisite fix commit if needed, scaffold commit, wire commit)
- CI passing (modulo pre-existing Windows build failures unrelated to TypeScript changes)

## Implementation Architecture

This is an **instruction-only skill** — no bash script. Bobert loads this skill and follows the guidance below, delegating execution through the full-lifecycle-delivery team pattern.

### Skill vs full-lifecycle-delivery Relationship

This skill **specializes** full-lifecycle-delivery for the extraction domain. Bobert:
1. Loads this skill for extraction-specific guidance
2. Uses the full-lifecycle-delivery team pattern (Task Group A) for execution
3. Applies extraction-specific constraints at each phase (documented below)

The full-lifecycle-delivery PhaseContext/PhaseResult protocol, roster request/response protocol, and coordinator delegation model all apply unchanged.

### Candidate Identification

When the extraction candidate is NOT already specified, direct explore-agent to find it using these criteria:

**What makes a good leaf candidate** (all criteria should be met):
1. **Zero or near-zero external `@bitwarden/*` imports** — only imports from npm packages or TypeScript builtins. This is the most important criterion.
2. **Small surface area** — typically < 100 lines, ideally < 50.
3. **Single well-defined concept** — a class, a utility, a type. Not a grab-bag.
4. **Stable, rarely-changing code** — the extraction itself won't conflict with ongoing work.
5. **Identifiable consumers** — ideally 1-3 consumers. Files with dozens of importers require massive cross-repo updates.

**Best hunting grounds** in bitwarden-clients:
- `libs/common/src/platform/misc/`
- `libs/common/src/platform/utils/`

**Candidate validation commands** (for explore-agent):
```bash
# Check imports in a file:
head -20 libs/common/src/platform/misc/some-file.ts

# Find files with no @bitwarden imports in a directory:
grep -rL "@bitwarden" libs/common/src/platform/misc/*.ts

# Count consumers of a specific file:
grep -r "from.*platform/misc/range-with-default" libs/ --include="*.ts" -l
```

### Barrel Export Check (Critical — Must Run Before Implementation)

Whether a file is barrel-exported determines the shim strategy. This check MUST happen in Phase 1 (explore-agent):

```bash
grep -r "<filename-without-extension>" libs/common/src/ --include="*.ts"
```

**If barrel-exported** (found in an `index.ts`): Add a re-export in that barrel after extraction.

**If NOT barrel-exported** (consumers use deep paths like `@bitwarden/common/platform/misc/<file>`): Place a re-export shim at the original file path:
```typescript
// libs/common/src/platform/misc/<original-file>.ts
// Re-export from the new canonical location for backwards compatibility
export { ClassName } from "@bitwarden/<name>";
```

### Library Naming Convention

Names in bitwarden-clients follow a **single lowercase concept word** pattern:
- `logging`, `serialization`, `guid`, `client-type`, `messaging`
- NOT `platform-range`, NOT `range-with-default`, NOT `common-utils`

To validate naming, explore-agent should run:
```bash
ls libs/ | grep -v common | grep -v platform | grep -v key-management
```

The `--team` flag (not the name) determines domain ownership. The name encodes concept only.

**Package name**: `@bitwarden/<name>` (set automatically by generator from `package.json`)
**Path alias**: `@bitwarden/<name>` → `libs/<name>/src/index.ts` (set automatically in `tsconfig.base.json`)

### Team Ownership (CODEOWNERS Lookup)

explore-agent MUST look up CODEOWNERS before naming the library:

```bash
grep "libs/common/src/platform" .github/CODEOWNERS
# → /libs/common/src/platform @bitwarden/team-platform-dev
```

The `--team` flag uses the short form: `@bitwarden/team-platform-dev` → `--team=platform`.

When the source file is in `libs/common/src/`:
- `libs/common/src/platform/` → `--team=platform`
- `libs/common/src/auth/` → `--team=auth`
- Use the most specific matching pattern

### The Nx `basic-lib` Generator

The ONLY correct scaffolding mechanism. Runs in Phase 2 (code-monkey, chunk 1):

```bash
npx nx generate @bitwarden/nx-plugin:basic-lib <name> --description="<description>" --team=<team> --directory=libs
```

- `<name>` is the short concept name (NOT a path)
- `--directory=libs` places the library at `libs/<name>/`
- Auto-creates: `project.json`, `package.json`, `tsconfig*.json`, `jest.config.js`, `eslint.config.mjs`, `README.md`, `src/index.ts`, CODEOWNERS entry, `tsconfig.base.json` path alias

**Post-generator verification** (code-monkey must run):
```bash
npx nx show project <name>
grep "@bitwarden/<name>" tsconfig.base.json
```

**Critical**: `node_modules` must be installed before running the generator. In git worktrees, `node_modules` is NOT automatically shared. If a fresh worktree is being used, run `npm ci` before the generator.

### Pre-existing Nx Project Graph Issue

A common blocker that may appear before the generator can run:

```
NX   Failed to process project graph.
The projects in the following directories have no name provided:
  - scripts/reverse-proxy-emulator
```

This happens when `scripts/` contains `package.json` files without a `name` field. The fix is a prerequisite commit (separate from the extraction work):

**Fix**: Add `scripts/` to `.nxignore` at workspace root:
```
scripts/
```

**Commit message**: `chore(nx): exclude scripts/ from project discovery`

This is a pre-existing bug, not introduced by the extraction. Commit it separately as a prerequisite.

### Commit Structure

The implementation plan should target this commit structure (phase 2, code-monkey):

1. **Prerequisite fix** (only if `ProjectsWithNoNameError` appears):
   `chore(nx): exclude scripts/ from project discovery`

2. **Scaffold commit** (generator output + populated src/ + spec moved):
   `feat(<name>): scaffold @bitwarden/<name> leaf lib`
   - Generator output files
   - `libs/<name>/src/index.ts` populated with the extracted class/function
   - Spec file moved from source location to `libs/<name>/src/`
   - Original source file in `libs/common/` deleted (NOT yet replaced by shim)

3. **Wire commit** (shim + consumer update + orphaned spec deletion):
   `refactor(<name>): wire shim and update consumer`
   - Re-export shim added at original file path (if not barrel-exported)
   - At least one consumer import updated to `@bitwarden/<name>`
   - Orphaned spec file deleted from `libs/common/` (if it wasn't already moved)

**Never combine** scaffold and wire in the same commit — it makes the diff harder to review.

### Spec File Handling

When moving a `.ts` file to the new lib:
- Move the corresponding `.spec.ts` file too
- The spec must import from the new public API (`@bitwarden/<name>`) not from a relative path
- Delete the original spec from `libs/common/` — do NOT leave orphaned test files
- Update any import paths in the spec that reference the old location

### What Must NOT Change

The extraction must be a pure structural change:
- Preserve all logic inside the class/function (zero behavior modifications)
- Preserve all JSDoc and inline comments
- Preserve the class/function name and exported interface
- Preserve spec file contents (only update import paths)

Reviewers must be able to diff and confirm no logic changed.

### Windows CI Failures

Windows Build and Windows Beta Build failures are frequently pre-existing main branch instability unrelated to TypeScript extraction work.

When these appear in Phase 4 (ci-reader):
1. Check if the failure existed before this PR on recent main branch PRs
2. If the failure is clearly unrelated to TypeScript/Nx changes (PowerShell scripts, desktop-specific build tooling), treat as pre-existing and proceed to quality review
3. Do NOT add correction commits for platform-specific failures not introduced by the extraction

### Chromatic (UI Review) Checks

`UI Review: clients` and `UI Tests: clients` are long-running and often still pending when other checks complete. This is expected. Their pending status does not block quality review. Draft PRs may not require Chromatic approval.

## Phase-by-Phase Extraction Guidance

Use the full-lifecycle-delivery skill for team formation and PhaseContext/PhaseResult protocol. Apply the extraction-specific guidance below at each phase.

### Phase 0 (Intake)

**Standard intake applies.** work-starter should identify the extraction candidate and create a TODO scoped to:
- The specific file being extracted
- The new library name (DEFERRED_TO_RESEARCH if not specified)
- The team ownership (DEFERRED_TO_RESEARCH)
- The import strategy (DEFERRED_TO_RESEARCH)

**Gap analysis standard answers** for extraction work:
- Library name → DEFERRED_TO_RESEARCH
- Team ownership → DEFERRED_TO_RESEARCH
- Import strategy → DEFERRED_TO_RESEARCH
- All other extraction-specific questions → DEFERRED_TO_RESEARCH

Addison's standard response is "Defer all gap analysis items to research, accept the ADR plan." When this response comes, map all items to DEFERRED_TO_RESEARCH and proceed to Phase 1 without escalation.

### Phase 1 (Research/Design)

**explore-agent** must answer all four of these questions before adr-maintainer records decisions:

1. **Zero import validation**: Does the candidate file import from any `@bitwarden/*` packages? (Check with `head -20 <file>`)
2. **Consumer count**: How many consumers use this file? (Check with `grep -r "from.*<path>" libs/ --include="*.ts" -l`)
3. **Barrel export check**: Is this file re-exported through an `index.ts`? (Critical — determines shim strategy)
4. **CODEOWNERS lookup**: Which team owns the source directory? (Check `.github/CODEOWNERS`)

**deep-researcher** answers naming convention questions by examining existing extracted libs:
```bash
ls libs/ | grep -v common | grep -v platform | grep -v key-management
```

**adr-maintainer** records THREE decisions:
- ADR 1: Library name rationale (why `<name>` over alternatives)
- ADR 2: Team ownership (which team, evidence from CODEOWNERS)
- ADR 3: Import strategy (shim vs barrel update, based on barrel export check)

**technical-breakdown-maintainer** produces v1.0.0 breakdown covering:
- Candidate validation results (import scan, consumer count, barrel check)
- Generator command (exact flags)
- Files to create, move, modify, delete
- Re-export shim location and content

**implementation-plan-maintainer** produces a 2-chunk spec for code-monkey:
- Chunk 1: Run generator + populate `src/index.ts` + move spec file + delete original source file
- Chunk 2: Add re-export shim + update at least one consumer + delete orphaned spec (if applicable)

Each chunk is a separate commit.

### Phase 2 (Implementation)

**code-monkey** executes in exactly 2 chunks (2 commits, or 3 if prerequisite fix needed).

**Before chunk 1**: Verify `node_modules` is installed. If the worktree is fresh, run `npm ci` first.

**Chunk 1 — Scaffold**:
1. Run the generator: `npx nx generate @bitwarden/nx-plugin:basic-lib <name> --description="..." --team=<team> --directory=libs`
2. If `ProjectsWithNoNameError` appears, create prerequisite commit first (add `scripts/` to `.nxignore`), then re-run generator
3. Verify: `npx nx show project <name>` and `grep "@bitwarden/<name>" tsconfig.base.json`
4. Populate `libs/<name>/src/index.ts` with the extracted class/function
5. Move spec file to `libs/<name>/src/` and update import paths in spec
6. Delete the original source file from `libs/common/`
7. Commit: `feat(<name>): scaffold @bitwarden/<name> leaf lib`

**Chunk 2 — Wire**:
1. Add re-export shim at original file path (if not barrel-exported)
2. Update at least one consumer import from deep path to `@bitwarden/<name>`
3. Commit: `refactor(<name>): wire shim and update consumer`

**git-historian** commits each chunk with the `write-git-commit` skill.

**Test scope**: Run tests for the new library only (`npx nx test <name>`). Do not require full monorepo test suite — extraction is structural, not behavioral.

### Phase 3 (Finalization)

**Standard finalization applies.** pr-maintainer MUST read `PULL_REQUEST_TEMPLATE.md` before creating the PR.

PR description should make clear:
- What was extracted and why (the extraction motivation — leaf lib goal)
- That the change is purely structural (no behavior changes)
- That re-export shim preserves backward compatibility for existing consumers
- Which consumer was updated as proof-of-concept

### Phase 4 (Publishing)

**ci-reader**: Apply Windows CI failure triage (see Windows CI Failures section above). Pre-existing failures do not block quality review.

**pull-request-reviewer**: Verify the extraction checklist:
- [ ] No logic changes in extracted class/function
- [ ] Spec file moved and imports updated
- [ ] Re-export shim present at original location
- [ ] At least one consumer updated
- [ ] No orphaned spec files in `libs/common/`
- [ ] `tsconfig.base.json` path alias added
- [ ] CODEOWNERS entry added by generator

## Environment Dependencies

- **bitwarden-clients worktree**: Required. The extraction runs inside a git worktree of bitwarden-clients.
- **node_modules**: Must be installed (`npm ci`) before running the Nx generator.
- **npx/nx**: Available after `npm ci`.
- **No bash dependencies**: Instruction-only skill.

## Usage & Testing Guidance

### Invocation Pattern

```
Addison: "Extract RangeWithDefault from common into its own leaf lib"

Bobert: [Loads extract-nx-lib skill]
        [Loads full-lifecycle-delivery skill for team pattern]

## Plan

Goal: Extract RangeWithDefault from libs/common/src/platform/misc/range-with-default.ts
      into a standalone @bitwarden/<name> Nx leaf library.

Skill Loaded: extract-nx-lib (specialized), full-lifecycle-delivery (team pattern)

Delegation Strategy: Task Group A with extraction-specific phase constraints
  - Phase 0: intake-coordinator (work-starter, worktree-manager)
  - Phase 1: research-design-coordinator (explore-agent validates candidate,
             deep-researcher names library, adr-maintainer records 3 decisions,
             technical-breakdown-maintainer + implementation-plan-maintainer)
  - Phase 2: implementation-coordinator (code-monkey runs generator + moves files,
             git-historian commits 2-3 commits)
  - Phase 3: finalization-coordinator (pr-maintainer reads PULL_REQUEST_TEMPLATE.md)
  - Phase 4: publishing-coordinator (ci-reader applies Windows CI triage)
```

### Scope Anchor for Extraction Work

When constructing PhaseContext, use this scope anchor pattern:

```json
{
  "scopeAnchor": {
    "ticketRequirement": "Extract <ClassName> from libs/common into a standalone Nx leaf library",
    "deliverableScope": "A new libs/<name>/ directory with scaffolded Nx lib, populated src/, moved spec, and re-export shim at original path",
    "prerequisiteContext": "The existing class at libs/common/src/.../file.ts provides context and is the source — it is NOT the deliverable",
    "fulfillmentTest": "libs/<name>/src/index.ts exports <ClassName>, original file path re-exports from @bitwarden/<name>, at least one consumer imports from @bitwarden/<name>"
  }
}
```

### Installation

```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/extract-nx-lib/
```

## Common Failure Modes and Mitigations

| Failure | Cause | Mitigation |
|---------|-------|------------|
| `ProjectsWithNoNameError` before generator | `scripts/` has `package.json` without `name` | Add `scripts/` to `.nxignore`, commit separately |
| Generator fails with missing deps | `node_modules` not installed in worktree | Run `npm ci` before generator |
| `npx nx show project <name>` not found | Generator didn't run cleanly | Check generator output for errors, re-run |
| Consumer still has broken import | Shim placed at wrong path | Verify shim path matches consumer's import path exactly |
| Orphaned spec in libs/common/ | Spec not deleted after move | Delete original spec in wire commit |
| Windows CI failure | Pre-existing main branch instability | Triage as pre-existing, proceed to quality review |
| Chromatic still pending | Long-running UI check | Expected behavior, does not block quality review |

## Anti-Patterns

- **Modifying logic during extraction**: Extract bytes only — no refactoring, no behavior changes
- **Choosing a compound name**: `platform-range`, `range-with-default` violate naming convention — use single concept word
- **Combining scaffold and wire commits**: Mixing generator output with import updates makes diffs unreadable
- **Updating ALL consumers in the PR**: Update ONE consumer to prove viability; the shim handles the rest
- **Running generator without node_modules**: Fails silently or with cryptic errors — always `npm ci` first in a fresh worktree
- **Skipping barrel export check**: Wrong shim strategy breaks consumers — always run the check before implementation
- **Adding correction commits for pre-existing Windows failures**: Wastes effort, pollutes commit history

---

This skill encodes the complete extraction workflow for bitwarden-clients Nx leaf library creation. It specializes full-lifecycle-delivery with extraction-specific candidate validation, naming conventions, generator usage, commit strategy, and CI triage guidance. All hard-won patterns from the RangeWithDefault extraction session are encoded here.
