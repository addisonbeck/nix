---
name: full-lifecycle-delivery
description: |
  Orchestration playbook for taking work from input (Jira ticket, memory stub, or prompt) through intake, research/design, implementation, finalization, and publishing to validated PR. Defines team composition (11 work agents + 5 phase coordinators), phase-by-phase workflow, PhaseContext/PhaseResult protocol, and completion criteria for full-lifecycle delivery. Use when asked to research, design, and publish a validated PR from a Jira ticket, or when prompted to use Task Group A.
---

# full-lifecycle-delivery Skill

This skill provides orchestration guidance for **Task Group A**, Bobert's canonical team composition for full-lifecycle work delivery. Task Group A takes work from initial input (Jira ticket, memory stub, or plain prompt) through intake, research, design, implementation, finalization, and publishing to produce a validated pull request ready for human review.

When invoked, Bobert follows this instruction playbook to:
1. Form the team via TeamCreate
2. For each phase: Spawn coordinator as teammate, send PhaseContext, receive roster specification, spawn work agents as teammates
3. Execute five sequential phases (Intake → Research/Design → Implementation → Finalization → Publishing)
4. Construct PhaseContext for each phase (goal, criteria, constraints - NOT roster)
5. Coordinators specify roster needs, Bobert spawns agents, coordinators execute autonomously
6. Validate PhaseResults before advancing to next phase
7. Maintain continuous TODO updates throughout all phases

**Critical**: ALL agents (coordinators + work agents) MUST be spawned as teammates in the same team to ensure consistent message infrastructure.

## Purpose & When to Use

Use Task Group A when:
- **Jira Ticket Work**: Taking a ticket from assignment through to draft PR
- **Full-Lifecycle Delivery**: Work requires intake, research, design, implementation, and finalization
- **Multi-Phase Workflow**: Task spans discovery → planning → execution → delivery
- **Team Coordination**: Work benefits from specialized agents handling distinct phases
- **Context Isolation**: Each specialist benefits from focused context window for their phase

Do NOT use Task Group A when:
- **Single-phase work**: Task is just implementation (use code-monkey directly)
- **Research-only**: No implementation needed (use deep-researcher)
- **Quick modifications**: Under 30 minutes of work (direct delegation more appropriate)

This is Addison's most common workflow pattern for substantive work.

## Input Contract

Task Group A expects:
- **input** (string|object): One of:
  - Jira ticket ID (string): `"TICKET-123"`
  - Memory UUID (string): `"ABC12345-DEF6-7890-GHIJ-KLMNOPQRSTUV"`
  - Plain prompt (string): Freeform description of work
  - Structured object with `{type: "jira"|"memory"|"prompt", value: string}`
- **variations** (optional, string): One of:
  - `"standard"` (default): Full 11-agent team
  - `"lite"`: Skip deep-researcher if domain knowledge sufficient (10 agents)

No explicit input schema - this is an instruction playbook loaded into Bobert's context.

## Output Contract

Task Group A produces:
- **Phase 0 Outputs**: TODO UUID, worktree path, clarified requirements, gap analysis memory UUID, gap analysis question count
- **Phase 1 Outputs**: Technical breakdown UUID (v1.0.0), implementation plan UUID
- **Phase 2 Outputs**: Commit SHAs, clean working tree confirmation, passing tests (new/modified tests by default; all package tests only when ticket explicitly requires full suite remediation)
- **Phase 3 Outputs**: Draft PR URL, completed TODO, finalized technical breakdown
- **Phase 4 Outputs**: CI status (passing), quality review status, correction cycle count

### Success Criteria
All five phases complete (Phases 0-4) with status: COMPLETE and required outputs present.

### Error Conditions
- PhaseResult with status: FAILED triggers Bobert to decide: retry, adjust, or abort
- Coordinator escalations (scope change, scope misalignment, goal conflict) require Bobert's strategic decision
- Scope validation failure (scopeValidation fields false) blocks phase advancement until Bobert investigates
- "Implementation complete + open ticket" contradiction requires Addison consultation
- Missing required outputs block phase advancement

## Implementation Architecture

This is an **instruction-only skill** - no bash script implementation. Bobert loads this skill and follows the guidance.

### Team Composition (11 Work Agents + 5 Phase Coordinators)

**Work Agents**:
1. **work-starter**: Intake specialist - transforms input into structured TODO with clarified requirements
2. **worktree-manager**: Worktree lifecycle management - creates and validates isolated work environment
3. **todo-spec-memory-maintainer**: Continuous TODO maintenance - active in ALL phases, keeps TODO current
4. **deep-researcher**: Domain research - investigates best practices, patterns, and external knowledge
5. **Explore**: Codebase investigation - discovers existing patterns, dependencies, and constraints
6. **adr-maintainer**: Architecture decisions - documents significant design choices
7. **implementation-plan-maintainer**: Executable specifications - translates architecture into step-by-step implementation guide
8. **technical-breakdown-maintainer**: Context synthesis - maintains living technical documentation
9. **code-monkey**: Implementation - fast code modifications following specification
10. **git-historian**: Commit creation - analyzes diffs and creates conventional commits
11. **pr-maintainer**: Draft PR creation - generates PR title, body, and test plan

**Phase Coordinators**:
1. **intake-coordinator**: Manages Phase 0 tactical execution (spawn, track, validate)
2. **research-design-coordinator**: Manages Phase 1 tactical execution with iterative loop
3. **implementation-coordinator**: Manages Phase 2 tactical execution with commit loop
4. **finalization-coordinator**: Manages Phase 3 tactical execution
5. **publishing-coordinator**: Manages Phase 4 tactical execution with CI monitoring and correction cycles

### Workflow Pattern

```
                    TASK GROUP A WORKFLOW

Input Source (Jira, memory stub, or prompt)
    |
    v
+---------------------------------------------------------+
| PHASE 0: INTAKE (once)                                  |
|  Delegated to: intake-coordinator                       |
|  Agents: work-starter, worktree-manager,                |
|          todo-spec-memory-maintainer                    |
|  Bobert provides: PhaseContext with input (NO roster)   |
|  Coordinator returns: PhaseResult with TODO UUID,       |
|                       worktree path                     |
+---------------------------------------------------------+
             |
             v
+---------------------------------------------------------+
| PHASE 1: RESEARCH/DESIGN/SYNTHESIS/PLANNING LOOP        |
|  Delegated to: research-design-coordinator              |
|  Agents: deep-researcher, Explore, adr-maintainer,      |
|          technical-breakdown-maintainer,                 |
|          implementation-plan-maintainer                  |
|  Bobert provides: PhaseContext with TODO UUID (NO roster)|
|  Coordinator returns: PhaseResult with breakdown UUID,   |
|                       implementation plan UUID           |
|                                                          |
|  COMPLETION: Breakdown v1.0.0 AND impl plan complete    |
+---------------------------------------------------------+
             |
             v
+---------------------------------------------------------+
| PHASE 2: IMPLEMENTATION/COMMIT LOOP (iterative)         |
|  Delegated to: implementation-coordinator               |
|  Agents: code-monkey, git-historian                     |
|  Bobert provides: PhaseContext with impl plan + worktree|
|  Coordinator returns: PhaseResult with commit SHAs,     |
|                       clean working tree confirmation    |
|                                                          |
|  COMPLETION: All functionality committed, tree clean     |
+---------------------------------------------------------+
             |
             v
+---------------------------------------------------------+
| PHASE 3: FINALIZATION                                   |
|  Delegated to: finalization-coordinator                 |
|  Agents: technical-breakdown-maintainer,                |
|          todo-spec-memory-maintainer, pr-maintainer     |
|  Bobert provides: PhaseContext with commits + worktree  |
|                   + branchPushed validation             |
|  Coordinator returns: PhaseResult with PR URL           |
|                                                          |
|  PREREQUISITE: Branch pushed to remote (human quality   |
|                gate for SSH hardware keys)              |
|  COMPLETION: Draft PR created                           |
+---------------------------------------------------------+
             |
             v
+---------------------------------------------------------+
| PHASE 4: PUBLISHING                                     |
|  Delegated to: publishing-coordinator                   |
|  Agents: ci-reader, ci-correction-planner,              |
|          pull-request-reviewer, code-monkey (REUSE),    |
|          git-historian (REUSE),                         |
|          todo-spec-memory-maintainer (REUSE)            |
|  Bobert provides: PhaseContext with PR URL, PR number,  |
|                   worktree path, commits                |
|  Coordinator returns: PhaseResult with CI status,       |
|                       quality review status             |
|                                                          |
|  COMPLETION: CI passing + quality review complete       |
+---------------------------------------------------------+
             |
             v
    VALIDATED PR (CI PASSING + QUALITY REVIEWED)
```

### PhaseContext Structure

Bobert constructs PhaseContext for each phase containing:

```json
{
  "phaseId": "phase-N-name",
  "phaseGoal": "Clear statement of what this phase must accomplish",
  "completionCriteria": {
    "requiredOutputs": ["Output 1", "Output 2"],
    "validationCommands": ["command to verify output 1", "command to verify output 2"]
  },
  "constraints": {
    "scopeBoundaries": ["What is IN scope", "What is OUT of scope"],
    "timeBox": "Expected duration"
  },
  "prerequisites": {
    "todoMemoryUUID": "UUID from prior phase",
    "gapAnalysisMemoryUUID": "UUID from Phase 0 gap analysis",
    "answeredGaps": [
      {"id": "Q1", "summary": "...", "answer": "Addison's answer text"}
    ],
    "resolutionQuestions": [
      {
        "id": "Q2",
        "summary": "...",
        "question": "Full question text",
        "requiredOutput": "Documented recommendation with rationale"
      }
    ]
  },
  "scopeAnchor": {
    "ticketRequirement": "What the ticket specifically asks to be BUILT/CHANGED/ADDED",
    "deliverableScope": "The NEW thing that must be produced -- not the existing thing",
    "prerequisiteContext": "What already exists and provides context, but is NOT the deliverable",
    "fulfillmentTest": "How we verify deliverables address the ticket, not just describe the codebase"
  }
}
```

**PhaseContext establishes WHAT (phase goal), not WHO (agent roster)**. PhaseContext does NOT include agentRoster -- coordinators specify roster needs after receiving PhaseContext per ADR-053.

**Scope Anchor**: Bobert populates `scopeAnchor` during Plan phase and carries it forward through all PhaseContexts. This ensures every coordinator can validate that their phase's deliverables address the ticket's actual requirement rather than documenting existing code. The scope anchor is derived from the ticket requirement analysis performed during Plan phase.

### Roster Request/Response Protocol (ADR-053)

**Coordinators specify agent rosters -- Bobert does not pre-provide rosters in PhaseContext.**

This 4-step protocol separates strategic authority (which coordinator to use) from tactical authority (which agents the coordinator needs):

#### Step 1: Bobert Spawns Coordinator and Requests Roster

Bobert:
- Constructs PhaseContext with goal, completion criteria, constraints, prerequisites (WHAT needs to be done)
- Does NOT include agentRoster in PhaseContext
- Spawns coordinator as teammate via Task tool WITH team_name parameter (critical: coordinator must be team member to receive messages)
- Sends PhaseContext to coordinator via SendMessage with roster request
- Expects coordinator to specify roster before execution begins

**Critical**: Coordinators MUST be spawned as teammates (not pure subagents) BEFORE receiving PhaseContext. This ensures coordinators have team communication infrastructure (SendMessage, Mailbox, TaskList) when work agents attempt to message them later.

#### Step 2: Coordinator Specifies Roster with Justification

Coordinator:
- Analyzes phase requirements from PhaseContext
- Determines roster needs based on:
  - Typical roster for this phase (documented default below)
  - Phase-specific variations (complexity, parallelization, specialization)
  - Agent reuse opportunities (prefer reusing existing team members)
- Returns roster specification to Bobert with:
  - Agent roles needed (e.g., work-starter, worktree-manager, todo-spec-memory-maintainer)
  - Justification for variations from typical roster
  - Reuse preferences (e.g., reuse todo-spec-memory-maintainer from Phase 0)

**Example Roster Specification**:
```
Phase 1 roster request:
- deep-researcher (domain investigation)
- explore-agent (codebase reconnaissance)
- adr-maintainer (design decisions)
- technical-breakdown-maintainer (synthesis)
- implementation-plan-maintainer (execution specs)
- todo-spec-memory-maintainer (REUSE from Phase 0)

Justification: Standard Phase 1 roster. Reusing todo-spec-memory-maintainer to maintain continuity with Phase 0 TODO.
```

#### Step 3: Bobert Spawns Agents from Specification

Bobert:
- Receives roster specification from coordinator via mailbox message
- Validates roster (agents exist, roles appropriate for phase)
- Spawns agents as teammates:
  - New agents: Via Task tool WITH team_name parameter and role context
  - Reused agents: Sends notification via SendMessage (agent already exists on team)
- Confirms roster ready: Notifies coordinator via SendMessage that all agents are spawned and available

**Critical**: All work agents MUST be spawned with team_name parameter to ensure they can SendMessage to coordinator and receive messages from coordinator. Consistent spawn mechanism prevents addressing mismatches.

#### Step 4: Coordinator Proceeds Autonomously

Coordinator:
- Receives confirmation that roster is ready
- Proceeds autonomously with phase execution:
  - Delegates tasks to agents via SendMessage
  - Monitors agent progress via task lists and mailbox
  - Validates work products against completion criteria
- Does NOT wait for additional authorization (PhaseContext + roster = complete authorization)

#### Typical Rosters (Reference from TODO #6 Work)

These rosters represent validated patterns from Take 8 execution. Coordinators should use these as defaults and justify variations.

**Phase 0 (Intake)**: 3 agents
- work-starter (ticket intake, TODO creation, worktree setup, debris cleanup)
- worktree-manager (worktree lifecycle)
- todo-spec-memory-maintainer (TODO population)

**Phase 1 (Research/Design)**: 5 agents (6 with reuse)
- deep-researcher (domain investigation)
- explore-agent (codebase reconnaissance)
- adr-maintainer (design decisions)
- technical-breakdown-maintainer (synthesis)
- implementation-plan-maintainer (execution specs)
- todo-spec-memory-maintainer (REUSE from Phase 0)

**Phase 2 (Implementation)**: 2 agents (3 with reuse)
- code-monkey (implementation)
- git-historian (atomic commits)
- todo-spec-memory-maintainer (REUSE from Phase 0)

**Phase 3 (Finalization)**: 3 agents (all reused from prior phases)
- technical-breakdown-maintainer (REUSE from Phase 1, finalization)
- pr-maintainer (draft PR creation)
- todo-spec-memory-maintainer (REUSE from Phase 0, mark DONE)

**Rationale**: Take 8 validation showed coordinator-specified rosters eliminated 12-15 minutes of reactive spawning overhead (War Story #1: Role Inversion). Phases 1-3 executed with zero reactive spawn cycles when coordinators specified rosters upfront.

**Phase 4 (Publishing)**: 6 agents (4 reused from prior phases)
- ci-reader (CI status monitoring)
- ci-correction-planner (failure analysis and fix specs)
- pull-request-reviewer (quality validation)
- code-monkey (REUSE from Phase 2, correction implementation)
- git-historian (REUSE from Phase 2, correction commits)
- todo-spec-memory-maintainer (REUSE from Phase 0, final updates)

**Rationale**: Phase 4 completes the publish-to-production journey by validating PR health through CI and quality review. Reusing code-monkey and git-historian for correction cycles leverages existing implementation infrastructure. publishing-coordinator enforces max 3 correction cycles before escalating to Bobert.

### PhaseResult Structure

Coordinators return PhaseResult containing:

```json
{
  "status": "COMPLETE" | "FAILED" | "BLOCKED",
  "outputs": {
    "outputName": "output value or UUID or path"
  },
  "validation": {
    "criteriaChecked": ["criterion 1 verified", "criterion 2 verified"],
    "evidence": ["command output showing completion"]
  },
  "scopeValidation": {
    "deliverableAddressed": true,
    "prerequisiteVsDeliverableClassified": true,
    "ticketFulfillmentTestPassed": true
  },
  "escalations": [
    {"type": "scope_change" | "scope_misalignment" | "goal_conflict" | "resource_unavailable", "description": "..."}
  ],
  "phaseMetrics": {
    "agentsSpawned": 3,
    "tasksCompleted": 8,
    "durationMinutes": 45
  }
}
```

**Scope Validation**: Coordinators include `scopeValidation` in every PhaseResult. This confirms that deliverables address the ticket's requirement (not existing code), that research findings were classified as prerequisite vs deliverable, and that the ticket fulfillment test from the scope anchor passes. If any `scopeValidation` field is `false`, Bobert MUST investigate before advancing.

### Key Characteristics

1. **Coordinator-Managed Phases**: Coordinators handle tactical execution (spawning, tracking, validation). Bobert retains strategic authority (composition, transitions, escalations).

2. **Continuous TODO Maintenance**: `todo-spec-memory-maintainer` is active throughout ALL phases, keeping the TODO current with discoveries, decisions, and progress.

3. **Two Iterative Loops**:
   - **Phase 1 Loop**: Research/design/synthesis/planning repeats until technical breakdown reaches v1.0.0 AND implementation plan is complete
   - **Phase 2 Loop**: Implementation/commit repeats until all planned functionality is committed AND working tree is clean AND tests pass
     - **Test Passing Scope (Default)**: New/modified tests passing (tests added or modified during implementation)
     - **Test Passing Scope (Exception)**: All package tests passing (only when ticket explicitly mandates full suite remediation with phrases like "fix all tests", "remediate test suite", "achieve 100% test pass rate")

4. **Test Passing Criteria**: Phase 2 completion requires "tests passing" validation, with scope determined by ticket requirements:

   **Default Scope - New/Modified Tests Passing**:
   - Applies to: Tickets focused on adding features, fixing specific bugs, refactoring specific components
   - Definition: Tests that were added or modified during the implementation must pass
   - Validation: Run tests that the implementation plan identifies as relevant to the changes
   - Rationale: Implementation should not break what it touches, but is not responsible for pre-existing test suite failures unrelated to the work
   - Example commands: `cargo test <specific_test>`, `pytest tests/test_new_feature.py`, `npm test -- <test_file>`

   **Exception Scope - All Package Tests Passing**:
   - Applies to: Tickets that explicitly mandate full test suite remediation
   - Trigger phrases in ticket: "fix all tests", "remediate test suite", "achieve 100% test pass rate", "all tests must pass", "comprehensive test fixing"
   - Definition: Entire project test suite must pass, including pre-existing tests unrelated to implementation
   - Validation: Run full test suite without filters or exclusions
   - Rationale: Ticket specifically requires test suite health as a deliverable, not just feature implementation
   - Example commands: `cargo test --all`, `pytest`, `npm test`

   **Coordinator Guidance**:
   - implementation-coordinator examines ticket language during Phase 2 initialization to determine test scope
   - Default scope applies unless explicit full-suite language is present in ticket
   - When in doubt, default scope applies (new/modified tests only)
   - PhaseResult validation must document which scope was applied and provide evidence (test output showing relevant tests passed)

5. **Context Isolation**: Each specialist operates in own context window, activating per phase. This prevents context pollution and allows focused work.

6. **Specification Bridge**: `implementation-plan-maintainer` in Phase 1 translates architecture into executable step-by-step specifications before Phase 2 begins. This bridges strategic design to tactical implementation.

7. **Input Source Flexibility**: `work-starter` adapts intake to handle Jira tickets, memory stubs, or plain prompts uniformly.

### Critical Constraints

These constraints are non-negotiable and prevent catastrophic failures in Task Group A execution:

1. **Spawn Mechanism Consistency (Critical)**

   **Constraint**: ALL agents (coordinators + work agents) MUST be spawned as teammates in the same team using the Task tool WITH team_name parameter.

   **Why This Matters**: Hybrid spawn models where coordinators are spawned as pure subagents (Task without team_name) while work agents are spawned as teammates create addressing mismatches. Work agents attempt to SendMessage to coordinators who have no team mailbox infrastructure, resulting in 0% message delivery, exponential retry loops, and complete phase coordination failure.

   **Root Cause**: Coordinators outside team communication system cannot receive messages from work agents. The Task tool has two modes:
   - WITH team_name: Spawns agent as teammate with SendMessage/Mailbox/TaskList tools
   - WITHOUT team_name: Spawns agent as pure subagent with no team communication infrastructure

   **Bobert Action**:
   - Phase 0-3: Spawn ALL coordinators using `Task tool WITH team_name`
   - Phase 0-3: Spawn ALL work agents using `Task tool WITH team_name`
   - NEVER spawn coordinators as pure subagents (Task without team_name)
   - VERIFY all agents have team communication tools before delegation begins

   **Observable Symptoms of Violation**:
   - Work agents report "SendMessage failed" when attempting to contact coordinator
   - Coordinator never receives messages from work agents despite agents showing message as sent
   - Phase coordination collapses into retry loops as agents cannot report status
   - Result: 0% message delivery rate, phase cannot complete

   **Take 9 Lesson**: Inconsistent spawn mechanism (coordinators as subagents, work agents as teammates) caused complete message infrastructure failure. This constraint prevents recurrence.

### Operational Lessons

These lessons distill recurring failure modes observed across multiple Task Group A executions. Each lesson identifies where the capability lives in the agent ecosystem and provides a specific Bobert action directive to prevent recurrence.

1. **Build Verification is a Phase 2 Gate**

   `code-monkey` includes a Phase 4.5 Build Verification Gate that executes project-wide health checks (cargo check, cargo clippy, cargo fmt --check, cargo test) when the implementation plan includes a `## Build Verification Commands` section. Build verification failures block commit creation -- code-monkey will not proceed to git-historian until all verification commands pass. This capability lives in `code-monkey` (Phase 4.5: Build Verification Gate).

   **Bobert action**: When constructing Phase 2 PhaseContext, verify that the implementation plan passed from Phase 1 includes a `## Build Verification Commands` section. If missing for projects with a build system, flag to `implementation-plan-maintainer` before starting Phase 2 -- build verification cannot gate what it cannot see.

2. **Formatting Checks are Part of CI Simulation**

   `git-historian` discovers project-specific formatters by scanning configuration files (flake.nix, Cargo.toml, package.json, pyproject.toml) and executes formatting checks in check/dry-run mode as Phase 5C before every commit. Formatting failures block commit creation. In pipeline mode, git-historian reports violations to the coordinating agent and recommends delegating the fix to code-monkey. This capability lives in `git-historian` (Phase 5B: Formatter Discovery, Phase 5C: Formatting Check Execution).

   **Bobert action**: Do not treat formatting failures as surprising -- they are expected CI simulation behavior. When `implementation-coordinator` reports CI simulation FAIL with formatting violations, this is the system working correctly. Ensure the remediation loop (code-monkey fix -> git-historian retry) executes before escalating. Only escalate after 2 failed formatting fix-and-retry cycles.

3. **CI Simulation Validates Phase 2 Completion**

   `implementation-coordinator` synthesizes CI simulation status by probing both `code-monkey` (for build verification results) and `git-historian` (for formatting check results) via SendMessage. The coordinator does not re-run checks itself -- it extracts outcomes from the agents that already ran them. CI simulation status (PASS/FAIL/NOT_APPLICABLE) is a Phase 2 completion gate: if FAIL, the coordinator must not construct a PhaseResult with status COMPLETE. This capability lives in `implementation-coordinator` (Phase Validation 7-Point Checklist, item #7: CI Simulation Validation).

   **Bobert action**: When reviewing Phase 2 PhaseResults, verify that `validation.criteriaChecked` includes CI simulation entries. If the PhaseResult reports COMPLETE but omits CI simulation criteria, reject it and instruct `implementation-coordinator` to complete the 7-point checklist before returning.

4. **Debris Cleanup is a Phase 0 Prerequisite**

   `work-starter` includes a Phase 1.5 that detects existing related work (WIP branches, stale worktrees, orphaned branches) and cleans test debris from previous sessions. Debris cleanup MUST complete before worktree creation -- stale worktrees, orphaned branches, and lock files from previous sessions cause worktree creation failures and wasted iterations. This capability lives in `work-starter` (Phase 1.5: Existing Work Detection and Test Debris Cleanup).

   **Bobert action**: When constructing Phase 0 PhaseContext, include an explicit constraint that `work-starter` must complete debris cleanup before `worktree-manager` creates new worktrees. If Phase 0 PhaseResult reports worktree creation failures, investigate whether debris cleanup was skipped or incomplete before retrying.

5. **Memory UUIDs Route Through Coordinators**

   Phase coordinators pass memory UUIDs in SendMessage delegation messages to downstream agents -- they never load memory content directly. Agents that need memory content use `read_memory` to load it themselves, which triggers the Required Reading hook to load transitive dependencies. Mismatching this pattern causes context loss (coordinator loads content agents need) or wasted tokens (agents load content coordinators already summarized). This capability is documented in the Memory Access Patterns section of `/Users/me/nix/system/modules/claude/CLAUDE.md`.

   **Bobert action**: When constructing PhaseContext, include memory UUIDs (TODO UUID, breakdown UUID, implementation plan UUID) as string references, not as loaded content. Verify that coordinator delegation messages pass UUIDs for agents to load, not pre-digested content summaries. If a coordinator reports context issues, check whether UUID routing was followed correctly.

6. **Coordinators Execute Autonomously After PhaseContext**

   Once Bobert delegates a phase to a coordinator via Task tool with a complete PhaseContext, the coordinator manages all tactical execution autonomously: spawning agents from the roster, distributing tasks, monitoring progress, validating completion, and returning a structured PhaseResult. Bobert does not intervene during phase execution unless the coordinator escalates. Premature Bobert intervention disrupts coordinator autonomy and creates conflicting instructions. This pattern is the core Strategic vs Tactical Separation (see next section).

   **Bobert action**: After delegating to a coordinator, wait for the PhaseResult. Do not send additional messages to the coordinator or its agents during execution. If a phase takes longer than expected, wait for the coordinator's completion signal or escalation -- implementation tasks may take 20-40 minutes per chunk, and multiple implementation/commit cycles are expected behavior. Only intervene when a PhaseResult is returned with status FAILED or BLOCKED.

### Strategic vs Tactical Separation

**Bobert Retains (Strategic)**:
- Which coordinator to use for each phase (intake, research-design, implementation, finalization)
- Whether to advance to next phase (phase transition authorization)
- How to handle coordinator escalations (scope changes, goal conflicts)
- Whether to abort, retry, or adjust after FAILED PhaseResults
- Cross-phase state tracking and quality gates
- Team formation infrastructure (spawning agents, managing team lifecycle)

**Coordinators Handle (Tactical)**:
- Which agents within phase (roster composition based on phase needs)
- How agents execute tasks (delegation, sequencing, parallelization)
- When to validate work products (timing of completion checks)
- How to recover from tactical failures (retry, reassign, iterate)
- Structured PhaseResult reporting

**Escalation Boundary**:
- Coordinators escalate when roster needs change mid-phase (scope expansion requires additional agents)
- Coordinators escalate when typical roster proves insufficient (unexpected complexity)
- Coordinators DO NOT escalate for tactical decisions (which agent to assign task, how to sequence work)

## Environment Dependencies

- **ORG_ROAM_DIR**: Required for TODO and memory creation/maintenance
- **Required agents**: All 11 work agents + 5 phase coordinators (intake, research-design, implementation, finalization, publishing) must exist in `~/.claude/agents/`
- **Required skills**: `create_memory`, `read_memory`, `todo-writer` must exist for TODO maintenance
- **Git environment**: Worktree operations require git configuration
- **GitHub CLI**: `gh` command required for PR operations and CI monitoring in Phase 4
- **Jira MCP** (optional): Required if input is Jira ticket ID

## Usage & Testing Guidance

### Invocation Pattern

**From Addison to Bobert**:
```
"Get Task Group A on this: TICKET-123"
```

**From Addison with variation**:
```
"Get Task Group A-Lite on this: implement caching for the API client"
```

**From Addison with memory stub**:
```
"Get Task Group A on this: ABC12345-DEF6-7890-GHIJ-KLMNOPQRSTUV"
```

### Bobert's Execution Steps

1. **Form Team**: Use TeamCreate to create team named "Task Group A - [brief work description]"

2. **Phase 0 Execution**:
   ```
   Construct PhaseContext for Phase 0 (include scopeAnchor from Plan phase)
   Spawn intake-coordinator as teammate (Task tool WITH team_name="task-group-a-{ticket}")
   Send PhaseContext to intake-coordinator via SendMessage with roster request
   Receive roster specification from intake-coordinator (work-starter, worktree-manager, todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification
   Notify intake-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from intake-coordinator
   Validate: status == COMPLETE, TODO UUID present, worktree path present
   Validate: gapAnalysisMemoryUUID present in PhaseResult
   Validate: gapAnalysisQuestionCount present in PhaseResult
   Scope check: TODO memory's scope aligns with ticket requirement (scopeAnchor)
   ```

**GAP ANALYSIS GATE** (between Phase 0 and Phase 1):

1. Read `gapAnalysisQuestionCount` from Phase 0 PhaseResult
2. If `gapAnalysisQuestionCount == 0` (no gaps found):
   - Proceed to Phase 1 without pausing
   - Pass `gapAnalysisMemoryUUID` in Phase 1 PhaseContext prerequisites
   - `answeredGaps: []` and `resolutionQuestions: []` in PhaseContext

3. If `gapAnalysisQuestionCount > 0`:
   a. Present to Addison:
      ```
      Phase 0 complete. work-starter identified [N] question(s) that need your input before research begins.

      Please review and fill in your answers:
        [absolute file path from Phase 0 PhaseResult]

      For each question, fill in the Answer field and set Resolution to one of:
        ANSWERED            — you provided a direct answer
        DEFERRED_TO_RESEARCH — you want Phase 1 to figure it out
        ACCEPTED_DEFAULT    — you accept Bobert's suggested default

      Reply "done" when finished.
      ```
   b. PAUSE: Wait for Addison to reply "done"
   c. Load gap analysis memory: `read_memory(gapAnalysisMemoryUUID)`
   d. Parse each question's Resolution field:
      - `ANSWERED`             → add to `answeredGaps` for Phase 1 PhaseContext
      - `DEFERRED_TO_RESEARCH` → add to `resolutionQuestions` for Phase 1 PhaseContext
      - `ACCEPTED_DEFAULT` or blank → document as assumption in Phase 1 PhaseContext constraints; do NOT add to resolutionQuestions
   e. Instruct todo-spec-memory-maintainer (already active from Phase 0) via SendMessage:
      "Update the TODO memory [todoMemoryUUID] to include Addison's gap analysis answers as constraints. [pass answeredGaps content]"
   f. Proceed to Phase 1

3. **Phase 1 Execution**:
   ```
   Construct PhaseContext for Phase 1 (pass TODO UUID from Phase 0 + scopeAnchor + gapAnalysisMemoryUUID + answeredGaps + resolutionQuestions)
   Spawn research-design-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to research-design-coordinator via SendMessage with roster request
   Receive roster specification (deep-researcher, Explore, adr-maintainer, technical-breakdown-maintainer, implementation-plan-maintainer, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify research-design-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from research-design-coordinator
   Validate: status == COMPLETE, breakdown UUID present (v1.0.0), impl plan UUID present
   Scope check: scopeValidation.deliverableAddressed == true
   Scope check: Technical breakdown describes NEW work, not existing implementation
   If scopeValidation fails: Do NOT advance to Phase 2 -- investigate scope misalignment
   ```

4. **Phase 2 Execution**:
   ```
   Construct PhaseContext for Phase 2 (pass impl plan UUID, worktree path)
   Spawn implementation-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to implementation-coordinator via SendMessage with roster request
   Receive roster specification (code-monkey, git-historian, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify implementation-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from implementation-coordinator
   Validate: status == COMPLETE, commit SHAs present, working tree clean, tests passing
   Note: Test passing scope documented in Key Characteristics section #4 (default: new/modified tests)
   ```

5. **Phase 3 Execution**:
   ```
   PREREQUISITE: Branch must be pushed to remote before finalization begins
   - Validate: git ls-remote origin <branch>
   - This is a human-only operation when using SSH hardware keys (ED25519-SK)
   - SSH hardware keys require interactive approval for push operations
   - This is a security feature and quality gate, not a bug
   - If branch is not on remote, STOP and request human to push branch

   Construct PhaseContext for Phase 3 (pass commits, worktree path, branchPushed: true)
   Spawn finalization-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to finalization-coordinator via SendMessage with roster request
   Receive roster specification (reuse technical-breakdown-maintainer, pr-maintainer, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify finalization-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from finalization-coordinator
   Validate: status == COMPLETE, PR URL present
   ```

6. **Phase 4 Execution**:
   ```
   PREREQUISITE: Draft PR created in Phase 3

   Construct PhaseContext for Phase 4 (pass PR URL, PR number, worktree path, commits)
   Spawn publishing-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to publishing-coordinator via SendMessage with roster request
   Receive roster specification (ci-reader, ci-correction-planner, pull-request-reviewer, reuse code-monkey, reuse git-historian, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify publishing-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from publishing-coordinator
   Validate: status == COMPLETE, ciStatus == "passing", qualityReviewStatus present
   Note: Correction cycles (0-3) are normal; escalation only if limit exceeded
   ```

7. **Assert Phase**: Review all PhaseResults, verify deliverables, report completion to Addison

### When to Consult Addison

Bobert should consult Addison (not proceed autonomously) when:
- **Gap analysis review**: Phase 0 produced a gap analysis with questions. Bobert presents the file path to Addison and waits for "done" before constructing Phase 1 PhaseContext (this is the Gap Analysis Gate, not an escalation — it is expected behavior, not an error condition).
- **Scope ambiguity in initial input**: Cannot determine clear work boundaries from Jira ticket or prompt
- **Scope misalignment detected**: Coordinator reports deliverables describe existing implementation rather than new work required by ticket
- **"Implementation complete + open ticket" contradiction**: Research indicates work is already done but ticket remains open
- **scopeValidation failure in PhaseResult**: Any scopeValidation field is false
- **PhaseResult status: BLOCKED**: Coordinator reports external blocker (missing access, unclear requirements)
- **Multiple FAILED retries**: Phase fails 2+ times with different approaches
- **Escalation requiring policy decision**: Coordinator reports scope change that affects project timeline or architecture
- **Quality gate failure**: Deliverable does not meet acceptance criteria even after rework

Bobert should proceed autonomously when:
- **Input is clear**: Jira ticket or prompt has sufficient detail for intake
- **PhaseResult status: COMPLETE**: All criteria met, advance to next phase
- **Minor clarifications**: Coordinator asks question that work-starter or deep-researcher can answer
- **First FAILED attempt**: Retry with adjusted approach before escalating

### Variations

**Task Group A-Lite** (10 agents, skip deep research):
- Use when domain knowledge is sufficient
- Remove `deep-researcher` from Phase 1 roster
- Faster execution for well-understood problem domains
