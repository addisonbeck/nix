---
name: navigate-memory
description: |
  Queries the org-roam knowledge base by communicating with the running Emacs daemon
  via emacsclient. Supports four interaction layers: connectivity checks, node lookup
  by ID or title, org-roam-ql predicate filtering, and graph aggregation queries.
  Use when an agent needs to search, filter, or traverse the org-roam node graph
  without modifying any content.
---

# navigate-memory Skill

When invoked, this skill provides instructions and ready-to-run `emacsclient` commands
for querying the org-roam knowledge base through the running Emacs daemon. All queries
are read-only — this skill never modifies nodes, never calls `org-roam-db-update-file`,
and never writes files.

**When to use**: An agent needs to search for nodes by tag, title, or backlink
relationship; traverse the node graph; or compute graph metrics such as most-linked
nodes. All queries go through the live daemon so the org-roam database is always
current.

**When NOT to use**: Creating or updating nodes (use `create_memory`), reading a single
known node by UUID (use `read_memory`), or when the Emacs daemon is confirmed not
running and no fallback exists.

## Critical: Socket Path

The Emacs daemon uses a non-default socket path set in the user config:

```elisp
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
```

The socket lives at `~/.emacs.d/server/server`. Plain `emacsclient` without
`--socket-name` looks in `/tmp/emacs$(id -u)/` and silently fails with a confusing
error. **Every `emacsclient` call must use:**

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "..."
```

The `ec` wrapper at `~/.nix-profile/bin/ec` also knows this path and can be used to
confirm the daemon is alive before running queries.

## Input Contract

This is an instruction-only skill. There is no JSON input schema. Agents read the
query patterns documented below and execute `emacsclient` commands directly via the
Bash tool.

The relevant environment context is:

- **ORG_ROAM_DIR**: `/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam`
- **Org-roam DB**: `~/.emacs.d/org-roam.db`
- **Socket**: `~/.emacs.d/server/server`

## Output Contract

All queries return human-readable strings from the Emacs process (printed to stdout by
`emacsclient --eval`). Results are not JSON — they are elisp-formatted strings, lists,
or multi-line text. Agents must parse the output as plain text.

Error responses from `emacsclient` appear on stderr and typically read:

```
/usr/bin/emacsclient: can't find socket; have you started the server?
```

If that occurs, the daemon is not running. Stop and report the error rather than
attempting fallback file reads.

## Implementation Architecture

Instruction-only skill. No bash helper script. Agents execute `emacsclient` commands
directly using the Bash tool with the exact invocations shown in the four layers below.

### Key Implementation Rules

1. **Always check connectivity first** (Layer 1) before attempting any query.
2. **Always use `--socket-name="$HOME/.emacs.d/server/server"`** — never bare `emacsclient`.
3. **Use org-roam-ql predicates** (Layer 3) for tag, todo, and backlink filtering —
   do not reinvent these with raw SQL.
4. **Use elisp-side accumulation** (Layer 4) for aggregates — never attempt EmacSQL
   `(funcall count ...)` syntax inside `:select` vectors (it throws `Wrong type
   argument: symbolp`).
5. **Apply the tags fallback** whenever reading tags from a node — `org-roam-node-tags`
   returns nil for nodes using the `ROAM_TAGS` property instead of `#+filetags:`.
6. **Prefer structured output** — format results as `"title | tags:... | metric:..."`
   strings joined with `\n` so results are easy to parse line by line.

### Tags Fallback Pattern

Always use this pattern when displaying or filtering on tags:

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

Run this before any query to confirm the daemon is alive and packages are loaded.

```bash
# Check daemon is alive and org-roam is loaded
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam)"
# Expected: t
# On failure: error about missing socket — daemon is not running

# Check whether org-roam-ql is already loaded
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam-ql)"
# Expected: t or nil

# If the above returned nil, load it explicitly before using Layer 3 queries
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(require 'org-roam-ql)"
# Expected: org-roam-ql
```

If the connectivity check fails, do not proceed with queries. Report the failure and
stop.

## Layer 2 — Node Lookup by ID or Title

Direct access to a specific node using the org-roam node API.

```bash
# Find a node by exact title (returns first match — title, id, file, tags)
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let ((node (seq-find (lambda (n) (string= (org-roam-node-title n) \"Bobert\"))
                      (org-roam-node-list))))
  (when node
    (format \"id:%s file:%s tags:%s\"
            (org-roam-node-id node)
            (file-name-nondirectory (org-roam-node-file node))
            (or (org-roam-node-tags node)
                (alist-get \"ROAM_TAGS\" (org-roam-node-properties node) nil nil #'equal)))))"

# Get full file content of a node by UUID
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let* ((node (org-roam-node-from-id \"SOME-UUID-HERE\"))
       (file (org-roam-node-file node)))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))"

# List all nodes — returns (title id) pairs for browsing
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar (lambda (n) (list (org-roam-node-title n) (org-roam-node-id n)))
        (org-roam-node-list))"
```

Replace `"SOME-UUID-HERE"` with the actual UUID. UUIDs come from `create_memory`
output, from `* Required Reading` links in node content, or from Layer 2/3 queries.

## Layer 3 — org-roam-ql Filtering

Use org-roam-ql predicates to filter nodes. This is the idiomatic high-level API for
tag, todo, and backlink queries. Ensure `org-roam-ql` is loaded (Layer 1) before using
these.

```bash
# All nodes tagged "project"
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(tags \"project\")))"

# Nodes with a specific TODO keyword
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(todo \"TODO\")))"

# Logical composition: tagged "reference" but NOT tagged "daily"
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(and (tags \"reference\") (not (tags \"daily\")))))"

# Count result set size without pulling all titles
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(length (org-roam-ql-nodes '(and (tags \"reference\") (not (tags \"daily\")))))"

# Nodes that link TO a specific node (backlinks — other nodes citing this one)
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(backlink-to \"NODE-TITLE-HERE\")))"

# Nodes linked FROM a specific node (forward links — nodes this one cites)
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(backlink-from \"NODE-TITLE-HERE\")))"

# Nodes matching a file path substring
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(file \"adr\")))"

# Nodes with a specific property value
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title
        (org-roam-ql-nodes '(property \"MEMORY_TYPE\" \"procedural\")))"
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

For metrics like "most-linked nodes", EmacSQL aggregate syntax is broken: `(funcall
count dest)` inside `:select` vectors throws `Wrong type argument: symbolp`. Pull raw
rows and accumulate counts in elisp instead.

### Available Link Types

The `links` table contains rows with a `type` column. Known values: `"id"`, `"https"`,
`"file"`, `"fuzzy"`, `"http"`. Use `type = "id"` to count only org-roam node-to-node
links.

```bash
# Top 5 most-linked nodes (highest inbound id-type link count)
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let* ((rows (org-roam-db-query [:select [dest] :from links :where (= type \"id\")]))
       (counts (make-hash-table :test 'equal)))
  (dolist (row rows)
    (puthash (car row) (1+ (gethash (car row) counts 0)) counts))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (pair)
       (let* ((node (org-roam-node-from-id (car pair))))
         (if node
             (format \"%s | tags:%s | inbound-links:%d\"
                     (org-roam-node-title node)
                     (or (org-roam-node-tags node)
                         (alist-get \"ROAM_TAGS\" (org-roam-node-properties node) nil nil #'equal))
                     (cadr pair))
           (format \"<unknown:%s> | links:%d\" (car pair) (cadr pair)))))
     (seq-take pairs 5) \"\n\")))"

# Top 5 most-connected nodes (highest outbound id-type link count)
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let* ((rows (org-roam-db-query [:select [source] :from links :where (= type \"id\")]))
       (counts (make-hash-table :test 'equal)))
  (dolist (row rows)
    (puthash (car row) (1+ (gethash (car row) counts 0)) counts))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (pair)
       (let ((node (org-roam-node-from-id (car pair))))
         (if node
             (format \"%s | outbound-links:%d\" (org-roam-node-title node) (cadr pair))
           (format \"<unknown:%s> | outbound:%d\" (car pair) (cadr pair)))))
     (seq-take pairs 5) \"\n\")))"
```

### Layer 4b — org-roam-ql Filter Combined with Graph Metric

Filter a candidate set with org-roam-ql, then compute link counts only on that subset.

```bash
# Among "reference" nodes, which are most linked?
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let* ((ref-nodes (org-roam-ql-nodes '(tags \"reference\")))
       (ref-ids (mapcar #'org-roam-node-id ref-nodes))
       (rows (org-roam-db-query [:select [dest] :from links :where (= type \"id\")]))
       (counts (make-hash-table :test 'equal)))
  (dolist (row rows)
    (when (member (car row) ref-ids)
      (puthash (car row) (1+ (gethash (car row) counts 0)) counts)))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat
     (lambda (p)
       (let ((node (org-roam-node-from-id (car p))))
         (format \"%s | links:%d\" (org-roam-node-title node) (cadr p))))
     (seq-take pairs 5) \"\n\")))"
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

### org-roam-ql Lazy Loading

`org-roam-ql` is loaded lazily (`:after org-roam`). If `(featurep 'org-roam-ql)`
returns nil, call `(require 'org-roam-ql)` first (Layer 1 step 3) before using any
`org-roam-ql-nodes` call.

### emacsclient Without Socket Silently Fails

Without `--socket-name`, `emacsclient` looks in `/tmp/emacs$(id -u)/` and fails with a
misleading error about no server. Always pass `--socket-name="$HOME/.emacs.d/server/server"`.

## Environment Dependencies

- **Socket**: `~/.emacs.d/server/server` (non-default path; always specify explicitly)
- **ORG_ROAM_DIR**: `/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam`
- **Org-roam DB**: `~/.emacs.d/org-roam.db` (managed by org-roam; do not query with sqlite3 directly)
- **Required packages**: `org-roam` (always loaded), `org-roam-ql` (lazy — require if needed)
- **Required tools**: `emacsclient` (available at `~/.nix-profile/bin/emacsclient`)
- **Platform**: macOS (darwin) — socket path uses `$HOME`, not `/tmp`
- **Running Emacs daemon**: Must be active. Verify with `ec` wrapper or Layer 1 check.

## Usage & Testing Guidance

### Step 1: Confirm daemon is running

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam)"
# Expected output: t
```

### Step 2: Load org-roam-ql if needed

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(featurep 'org-roam-ql)"
# If nil:
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "(require 'org-roam-ql)"
```

### Step 3: Run a simple title search

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let ((node (seq-find (lambda (n) (string= (org-roam-node-title n) \"Bobert\"))
                      (org-roam-node-list))))
  (when node (org-roam-node-id node)))"
```

### Step 4: Filter by tag

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(mapcar #'org-roam-node-title (org-roam-ql-nodes '(tags \"project\")))"
```

### Step 5: Find most-linked nodes

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" --eval "
(let* ((rows (org-roam-db-query [:select [dest] :from links :where (= type \"id\")]))
       (counts (make-hash-table :test 'equal)))
  (dolist (row rows) (puthash (car row) (1+ (gethash (car row) counts 0)) counts))
  (let ((pairs nil))
    (maphash (lambda (k v) (push (list k v) pairs)) counts)
    (setq pairs (sort pairs (lambda (a b) (> (cadr a) (cadr b)))))
    (mapconcat (lambda (p)
      (let ((node (org-roam-node-from-id (car p))))
        (if node (format \"%s | links:%d\" (org-roam-node-title node) (cadr p))
          (format \"<unknown> | links:%d\" (cadr p)))))
    (seq-take pairs 5) \"\n\")))"
```

### Claude Code Invocation

```
/navigate-memory
```

This loads the query patterns and gotcha documentation into the agent's context.

### Installation

```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/navigate-memory/
```
