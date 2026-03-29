---
name: interview-the-human
description: Use when an agent needs to create a structured Q&A gap analysis memory for human review during work intake. Given a work title and a list of questions (or an empty list), formats and persists an org-mode memory node that Addison can review and answer before Phase 1 research begins.
---

# interview-the-human Skill

This skill instructs you to take a structured list of intake gaps and persist them as an org-roam memory node for human review. The memory node is the handoff artifact between automated intake analysis and the human decision point that precedes Phase 1 research.

When invoked, you will format the provided questions as org-mode content, ensure the subfolder exists, invoke the `create_memory` skill, and return a structured completion report to your coordinator.

An empty `questions` array is a valid and required invocation path. You must never skip memory creation because no gaps were found — the absence of gaps is itself a reportable outcome.

## Input Contract

You will receive the following fields when this skill is invoked:

- **work_title** (string, required): Human-readable title of the work being intaked. Used verbatim in the memory title and alias.
- **questions** (array, required): List of gap question objects. May be empty. Each object contains:
  - **id** (string, required): Short identifier, e.g. `"Q1"`, `"Q2"`
  - **summary** (string, required): One-line label used as the org heading
  - **question** (string, required): The full question text
  - **blocker** (boolean, required): Whether this question blocks forward progress
  - **bobert_default** (string, required): What to proceed with if Addison does not respond

## Output Contract

After `create_memory` completes successfully, return this exact report to your coordinator:

```
GAP ANALYSIS COMPLETE
UUID: [uuid from create_memory response]
File: [file path from create_memory response]
Title: Work Starter Gap Analysis: [WORK_TITLE]
Questions: [N] (or "no gaps found" if questions array was empty)
```

If `create_memory` returns an error, surface it immediately:

```
GAP ANALYSIS FAILED
Error: [error message from create_memory]
```

## Implementation Architecture

This is an instruction-only skill. No shell script is required. You use your own tools to invoke the `create_memory` skill.

### Execution Steps

1. Receive `work_title` and `questions` from your coordinator.
2. Ensure the `gap-analysis` subfolder exists by running:
   ```bash
   mkdir -p "$ORG_ROAM_DIR/gap-analysis"
   ```
3. Build the org-mode content string using the rules in the Org Content section below.
4. Invoke `create_memory` with the parameters specified in the Memory Metadata section below.
5. Parse the JSON response from `create_memory` to extract `id`, `file`, and `title`.
6. Return the completion report to your coordinator.

### Org Content Rules

The content you pass to `create_memory` must follow one of two templates depending on whether `questions` is non-empty or empty.

**When `questions` is non-empty**, build this structure:

```org
* Status
AWAITING_RESPONSE

* Questions

** [ID]: [QUESTION_SUMMARY]
*** Type
[BLOCKER if blocker is true, else INFORMATIONAL]

*** Question
#+begin_quote
[THE QUESTION TEXT]
#+end_quote

*** Bobert's Default
#+begin_quote
[BOBERT_DEFAULT TEXT]
#+end_quote

*** Answer
#+begin_quote

#+end_quote

*** Resolution
```

Repeat the `** [ID]: [QUESTION_SUMMARY]` block for each question in the array, in order.

**When `questions` is empty**, use this content instead:

```org
* Status
NO_GAPS_FOUND

* Questions

** No Significant Gaps Identified
The agent reviewed the input thoroughly and found no gaps requiring clarification before research begins. Phase 1 may proceed without a human pause.
```

The `Resolution` field under each question is intentionally left blank. Addison fills it in with one of: `ANSWERED`, `DEFERRED_TO_RESEARCH`, or `ACCEPTED_DEFAULT`.

### Memory Metadata

Invoke `create_memory` with these exact parameters:

- **title**: `Work Starter Gap Analysis: [WORK_TITLE]`
- **memory_type**: `working`
- **tags**: `["gap-analysis", "intake", "interview"]`
- **aliases**: `["Gap Analysis: [WORK_TITLE]"]`
- **content**: the org-mode string built above
- **subfolder**: `gap-analysis`

## Environment Dependencies

- **ORG_ROAM_DIR** (required): Path to the org-roam directory. Used when creating the subfolder. Must be set in the environment before invocation.
- **create_memory skill** (required): Must be installed at `~/.claude/skills/create_memory/`. This skill delegates all file I/O and UUID generation to `create_memory`.
- **Bash tool** (required): Used to run `mkdir -p` for the subfolder.

## Usage & Testing Guidance

### Example Invocation — Questions Present

```
/interview-the-human {
  "work_title": "Migrate auth service to Rust",
  "questions": [
    {
      "id": "Q1",
      "summary": "Target Rust edition",
      "question": "Which Rust edition should the new auth service target: 2021 or 2024?",
      "blocker": true,
      "bobert_default": "Default to 2021 for broader toolchain compatibility until Addison specifies otherwise."
    },
    {
      "id": "Q2",
      "summary": "Backward compatibility requirement",
      "question": "Must the migrated service maintain API compatibility with the existing C# client, or is a breaking change acceptable?",
      "blocker": true,
      "bobert_default": "Assume backward compatibility is required unless Addison confirms otherwise."
    }
  ]
}
```

Expected output:
```
GAP ANALYSIS COMPLETE
UUID: <generated>
File: $ORG_ROAM_DIR/gap-analysis/<timestamp>.work-starter-gap-analysis-migrate-auth-service-to-rust.org
Title: Work Starter Gap Analysis: Migrate auth service to Rust
Questions: 2
```

### Example Invocation — No Gaps Found

```
/interview-the-human {
  "work_title": "Fix typo in login button label",
  "questions": []
}
```

Expected output:
```
GAP ANALYSIS COMPLETE
UUID: <generated>
File: $ORG_ROAM_DIR/gap-analysis/<timestamp>.work-starter-gap-analysis-fix-typo-in-login-button-label.org
Title: Work Starter Gap Analysis: Fix typo in login button label
Questions: no gaps found
```

### Verify the Memory Was Created

```bash
ls "$ORG_ROAM_DIR/gap-analysis/"
```

### Installation

```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/interview-the-human/
```
