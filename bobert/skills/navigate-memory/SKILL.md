---
name: navigate-memory
description: |
  Queries the org-roam knowledge base. Supports two execution paths: batch mode
  (when BOBERT_EMACS is set — running inside bobert-with-emacs) and daemon mode
  (when BOBERT_EMACS is not set — running inside standard bobert). Supports four
  interaction layers: connectivity checks, node lookup by ID or title,
  org-roam-ql predicate filtering, and graph aggregation queries. Use when an
  agent needs to search, filter, or traverse the org-roam node graph without
  modifying any content.
---

# navigate-memory Skill

When invoked, this skill provides instructions and the `navigate-memory.sh` helper
script for querying the org-roam knowledge base. All queries are read-only — this
skill never modifies nodes, never calls `org-roam-db-update-file`, and never writes
files.

**When to use**: Search for nodes by tag, title, or backlink relationship; traverse
the node graph; compute graph metrics such as most-linked nodes.

**When NOT to use**: Creating or updating nodes (use `create_memory`), reading a
single known node by UUID (use `read_memory`).

## Execution Paths

navigate-memory operates in two modes detected automatically by `navigate-memory.sh`:

### Batch Mode (bobert-with-emacs)

Active when `BOBERT_EMACS` is set. Uses the bundled `emacs-nox` binary (Nix store
path) with `-Q --batch` to query the isolated org-roam database at
`BOBERT_ORG_ROAM_DB` (~/.bobert/org-roam.db). No running Emacs daemon required.

### Daemon Mode (standard bobert)

Active when `BOBERT_EMACS` is not set. Queries via `emacsclient` connecting to the
running host Emacs daemon. Requires the daemon to be active and org-roam loaded.

### Direct emacsclient (daemon mode only)

For interactive or complex multi-step daemon-mode queries, agents may call
`emacsclient` directly. Always use the non-default socket path:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "..."
```

## Critical: Socket Path (Daemon Mode)

The Emacs daemon uses a non-default socket path:

```elisp
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
```

The socket lives at `~/.emacs.d/server/server`. Plain `emacsclient` without
`--socket-name` looks in `/tmp/emacs$(id -u)/` and silently fails. **Every direct
`emacsclient` call must use:**

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "..."
```

## Input Contract

Pass an elisp expression string as `$1` to `navigate-memory.sh`. The script handles
path initialization (sets `org-roam-directory` and `org-roam-db-location`) before
executing the query.

Environment variables:
- **BOBERT_EMACS**: Nix store path to bundled emacs binary; presence triggers batch mode
- **BOBERT_ORG_ROAM_DB**: Path to isolated org-roam database (batch mode); defaults to `~/.bobert/org-roam.db`
- **ORG_ROAM_DIR**: Path to org-roam notes directory (set in bobert env block)

## Output Contract

All queries return human-readable strings from the Emacs process printed to stdout.
Results are not JSON — they are elisp-formatted strings, lists, or multi-line text.
Agents must parse output as plain text.

## Implementation Architecture

Two-file skill: `SKILL.md` (this file) + `navigate-memory.sh` (detection and routing).

### Key Implementation Rules

1. **Use `navigate-memory.sh`** for all queries — it handles detection automatically.
2. **In daemon mode direct calls, always use `--socket-name="$HOME/.emacs.d/server/server"`**.
3. **Use org-roam-ql predicates** (Layer 3) for tag, todo, and backlink filtering.
4. **Use elisp-side accumulation** (Layer 4) for aggregates — never use EmacSQL
   `(funcall count ...)` inside `:select` vectors (throws `Wrong type argument: symbolp`).
5. **Apply the tags fallback** whenever reading tags — `org-roam-node-tags` returns nil
   for nodes using `ROAM_TAGS` property instead of `#+filetags:`.
6. **Prefer structured output** — format as `"title | tags:... | metric:..."` strings.

### Tags Fallback Pattern

```elisp
(or (org-roam-node-tags node)
    (alist-get "ROAM_TAGS" (org-roam-node-properties node) nil nil #'equal))
```

### Node Accessor Reference

| Accessor | Returns |
|---|---|
| `(org-roam-node-id node)` | UUID string |
| `(org-roam-node-title node)` | Title string |
| `(org-roam-node-file node)` | Absolute file path |
| `(org-roam-node-tags node)` | List of tag strings (may be nil — use fallback) |
| `(org-roam-node-properties node)` | Alist of PROPERTIES drawer entries |
| `(org-roam-node-level node)` | Heading level (0 = file-level node) |
| `(org-roam-node-aliases node)` | List of alias strings |
| `(org-roam-node-todo node)` | TODO keyword string or nil |

## Layer 1 — Connectivity Check

**Batch mode**: Not needed — navigate-memory.sh validates the DB file exists before querying.

**Daemon mode**: Run before any query to confirm the daemon is alive.

```bash
# Check daemon is alive and org-roam is loaded
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam)"
# Expected: t

# Check whether org-roam-ql is already loaded
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam-ql)"
# Expected: t or nil

# If the above returned nil, load it explicitly before using Layer 3 queries
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(require 'org-roam-ql)"
# Expected: org-roam-ql
```

If the connectivity check fails, do not proceed. Report the failure and stop.

## Layer 2 — Node Lookup by ID or Title

```bash
# Find a node by exact title
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(let ((node (seq-find (lambda (n) (string= (org-roam-node-title n) "Bobert"))
                      (org-roam-node-list))))
  (when node
    (format "id:%s file:%s tags:%s"
            (org-roam-node-id node)
            (file-name-nondirectory (org-roam-node-file node))
            (or (org-roam-node-tags node)
                (alist-get "ROAM_TAGS" (org-roam-node-properties node) nil nil (quote equal))))))'

# Get full file content of a node by UUID
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(let* ((node (org-roam-node-from-id "SOME-UUID-HERE"))
       (file (org-roam-node-file node)))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))'

# List all nodes — returns (title id) pairs for browsing
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(mapcar (lambda (n) (list (org-roam-node-title n) (org-roam-node-id n)))
        (org-roam-node-list))'
```

Replace `"SOME-UUID-HERE"` with the actual UUID.

## Layer 3 — org-roam-ql Filtering

**Daemon mode**: Ensure `org-roam-ql` is loaded (Layer 1) before using these.
**Batch mode**: `org-roam-ql` is bundled but not auto-required — add `(require 'org-roam-ql)` at the start of your query expression.

```bash
# All nodes tagged "project"
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (tags "project"))))'

# Nodes with a specific TODO keyword
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (todo "TODO"))))'

# Logical composition: tagged "reference" but NOT tagged "daily"
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (and (tags "reference") (not (tags "daily"))))))'

# Nodes that link TO a specific node (backlinks)
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (backlink-to "NODE-TITLE-HERE"))))'

# Nodes linked FROM a specific node (forward links)
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (backlink-from "NODE-TITLE-HERE"))))'

# Nodes matching a file path substring
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (file "adr"))))'

# Nodes with a specific property value
~/.bobert/skills/navigate-memory/navigate-memory.sh \
  '(mapcar (function org-roam-node-title) (org-roam-ql-nodes (quote (property "MEMORY_TYPE" "procedural"))))'
```

### org-roam-ql Predicate Reference

| Predicate | Usage | Notes |
|---|---|---|
| `(tags "value")` | Nodes tagged with value | Case-sensitive |
| `(todo "KEYWORD")` | Nodes with TODO state | e.g. `"TODO"`, `"DONE"` |
| `(backlink-to "Title")` | Nodes that link to Title | Inbound references |
| `(backlink-from "Title")` | Nodes that Title links to | Outbound references |
| `(file "substr")` | Nodes whose file path contains substr | Useful for subfolder filtering |
| `(property "KEY" "val")` | Nodes with matching property | Checks PROPERTIES drawer |
| `(scheduled ...)` | Nodes with scheduled date | Date comparison forms |
| `(deadline ...)` | Nodes with deadline date | Date comparison forms |
| `(and ...)` | Logical AND of predicates | Variadic |
| `(or ...)` | Logical OR of predicates | Variadic |
| `(not ...)` | Logical NOT of predicate | Single argument |

## Layer 4 — Graph Aggregation

EmacSQL aggregate syntax is broken: `(funcall count dest)` inside `:select` vectors
throws `Wrong type argument: symbolp`. Pull raw rows and accumulate counts in elisp.

### Available Link Types

The `links` table `type` column values: `"id"`, `"https"`, `"file"`, `"fuzzy"`, `"http"`.
Use `type = "id"` to count only org-roam node-to-node links.

```bash
# Top 5 most-linked nodes (highest inbound id-type link count)
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(let* ((rows (org-roam-db-query [:select [dest] :from links :where (= type "id")]))
       (counts (make-hash-table :test (quote equal))))
  (dolist (row rows)
    (puthash (car row) (1+ (gethash (car row) counts 0)) counts))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (pair)
       (let* ((node (org-roam-node-from-id (car pair))))
         (if node
             (format "%s | tags:%s | inbound-links:%d"
                     (org-roam-node-title node)
                     (or (org-roam-node-tags node)
                         (alist-get "ROAM_TAGS" (org-roam-node-properties node) nil nil (quote equal)))
                     (cadr pair))
           (format "<unknown:%s> | links:%d" (car pair) (cadr pair)))))
     (seq-take pairs 5) "\n")))'

# Top 5 most-connected nodes (highest outbound id-type link count)
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(let* ((rows (org-roam-db-query [:select [source] :from links :where (= type "id")]))
       (counts (make-hash-table :test (quote equal))))
  (dolist (row rows)
    (puthash (car row) (1+ (gethash (car row) counts 0)) counts))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (pair)
       (let ((node (org-roam-node-from-id (car pair))))
         (if node
             (format "%s | outbound-links:%d" (org-roam-node-title node) (cadr pair))
           (format "<unknown:%s> | outbound:%d" (car pair) (cadr pair)))))
     (seq-take pairs 5) "\n")))'
```

### Layer 4b — org-roam-ql Filter Combined with Graph Metric

```bash
# Among "reference" nodes, which are most linked?
~/.bobert/skills/navigate-memory/navigate-memory.sh '
(let* ((ref-nodes (org-roam-ql-nodes (quote (tags "reference"))))
       (ref-ids (mapcar (function org-roam-node-id) ref-nodes))
       (rows (org-roam-db-query [:select [dest] :from links :where (= type "id")]))
       (counts (make-hash-table :test (quote equal))))
  (dolist (row rows)
    (when (member (car row) ref-ids)
      (puthash (car row) (1+ (gethash (car row) counts 0)) counts)))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (p)
       (let ((node (org-roam-node-from-id (car p))))
         (format "%s | links:%d" (org-roam-node-title node) (cadr p))))
     (seq-take pairs 5) "\n")))'
```

## Known Gotchas

### Tags Dual-Source

`org-roam-node-tags` returns nil for nodes that store tags in the `ROAM_TAGS` property
drawer rather than `#+filetags:`. Always apply the fallback:

```elisp
(or (org-roam-node-tags node)
    (alist-get "ROAM_TAGS" (org-roam-node-properties node) nil nil #'equal))
```

### EmacSQL Aggregate Syntax is Broken

Do NOT write:

```elisp
;; WRONG — throws: Wrong type argument: symbolp
(org-roam-db-query [:select [(funcall count dest)] :from links])
```

Always pull raw rows and count in elisp as shown in Layer 4.

### org-roam-ql Lazy Loading (Daemon Mode)

In daemon mode, `org-roam-ql` is loaded lazily (`:after org-roam`). If
`(featurep 'org-roam-ql)` returns nil, call `(require 'org-roam-ql)` first.

### emacsclient Without Socket Silently Fails

Without `--socket-name`, `emacsclient` looks in `/tmp/emacs$(id -u)/` and fails with a
misleading error. Always pass `--socket-name="$HOME/.emacs.d/server/server"`.

### Batch Mode: org-roam-ql Require

In batch mode, `navigate-memory.sh` wraps your query in `(progn (require 'org-roam) ...)`.
`org-roam-ql` is bundled but NOT auto-required. For Layer 3 queries in batch mode, add
`(require 'org-roam-ql)` at the start of the elisp expression passed to navigate-memory.sh.

## Environment Dependencies

- **BOBERT_EMACS**: Nix store path to bundled emacs binary (set by bobert-with-emacs; absent in standard bobert)
- **BOBERT_ORG_ROAM_DB**: Isolated org-roam database path; defaults to `~/.bobert/org-roam.db`
- **ORG_ROAM_DIR**: Notes directory; set by bobert env block in settings.json
- **Socket (daemon mode)**: `~/.emacs.d/server/server` (non-default; always specify explicitly)
- **emacsclient (daemon mode)**: `~/.nix-profile/bin/emacsclient`
- **Running daemon (daemon mode only)**: Must be active with org-roam loaded

## Claude Code Invocation

```
/navigate-memory
```

Loads query patterns and gotcha documentation into agent context.
