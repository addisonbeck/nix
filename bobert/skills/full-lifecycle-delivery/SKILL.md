---
name: full-lifecycle-delivery
description: |
  Orchestration playbook for taking work from input (Jira ticket, memory stub, or prompt) through intake, research/design, implementation, finalization, and publishing to validated PR. Defines team composition (11 work agents + 5 phase coordinators), phase-by-phase workflow, PhaseContext/PhaseResult protocol, and completion criteria for full-lifecycle delivery. Use when asked to research, design, and publish a validated PR from a Jira ticket, or when prompted to use Task Group A.
---

# full-lifecycle-delivery Skill

This skill provides orchestration guidance for full-lifecycle work delivery. A phased team takes work from initial input (Jira ticket, memory stub, or plain prompt) through intake, research, design, implementation, finalization, and publishing to produce a validated pull request ready for human review.

When invoked, Bobert follows this instruction playbook to:
1. Form the team via TeamCreate
2. For each phase: Spawn coordinator as teammate, send PhaseContext, receive roster specification, spawn work agents as teammates
3. Execute five sequential phases (Intake → Research/Design → Implementation → Finalization → Publishing)
4. Construct PhaseContext for each phase (goal, criteria, constraints - NOT roster)
5. Coordinators specify roster needs, Bobert spawns agents, coordinators execute autonomously
6. Validate PhaseResults before advancing to next phase
7. Maintain continuous TODO updates throughout all phases

**Critical**: ALL agents (coordinators + work agents) MUST be spawned as teammates in the same team to ensure consistent message infrastructure.

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
- **Phase 0 Outputs**: TODO UUID, worktree path, clarified requirements, gap analysis memory UUID, work scope memory uuid 
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

### Team Composition (2 direct reports + 5 phase coordinators with their own roster)

**Work Agents**:

Two agents are spawned right away in this workflow, and report directly to the orchestrating agent.

1. **todo-spec-memory-maintainer**: Continuous TODO maintenance - active in ALL phases, keeps TODO current
2. **retrospective-maintainer**: Maintains war stories from the team and synthesizes a retro doc when work ends

**Phase Coordinators**:

These agents report to the orchestrating agent but coordinate their own sets of agents. The orchestrating agent spawns a coordinator at the beginning of a phase, the coordinator instructs the orchestrating agent on its expected roster, the orchestrating agent spawns those subagents with information about peers and their coordinator, then the coordinating agent takes over as a phase lead.

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

1. **Coordinator-Managed Phases**: Coordinators and direct reports handle tactical execution (spawning, tracking, validation). Bobert retains strategic authority (composition, transitions, escalations).

2. **Continuous TODO Maintenance**: `todo-spec-memory-maintainer` reports to the orchestrating agent, sitting outside the phase workflow. This agent is active throughout ALL phases, keeping the TODO current with discoveries, decisions, and progress. This agent should be spawned right away, before work begins with coordinators. Coordinators should be encouraged to read the memory maintained by `todo-spec-memory-maintainer` using the the read_memory. This document serves as the our primary reference point for the work.

3. **Continuous retro Maintenance**: `retrospective-maintainer` reports to the orchestrating agent, sitting outside the phase workflow. This agent is active throughout ALL phases, keeping the TODO current with discoveries, decisions, and progress. This agent should be spawned right away, before work begins with coordinators. Coordinators should be encouraged to send war stories to this agent during their work cycles.

4. **Two Iterative Loops**:
   - **Phase 1 Loop**: Research/design/synthesis/planning repeats until technical breakdown reaches v1.0.0 AND implementation plan is complete
   - **Phase 2 Loop**: Implementation/commit repeats until all planned functionality is committed AND working tree is clean AND tests pass (see item 5 for test scope details)

5. **Test Passing Criteria**: Phase 2 completion requires "tests passing" validation, with scope determined by ticket requirements:

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

6. **Context Isolation**: Each specialist operates in own context window, activating per phase. This prevents context pollution and allows focused work.

7. **Specification Bridge**: `implementation-plan-maintainer` in Phase 1 translates architecture into executable step-by-step specifications before Phase 2 begins. This bridges strategic design to tactical implementation.

8. **Input Source Flexibility**: `work-starter` adapts intake to handle Jira tickets, memory stubs, or plain prompts uniformly.

### Critical Constraints

These constraints are non-negotiable and prevent catastrophic failures in execution:

1. **Spawn Mechanism Consistency (Critical)**

   **Constraint**: ALL agents (coordinators + work agents) MUST be spawned as teammates in the same team using the Task tool WITH team_name parameter.

   **Why This Matters**: Hybrid spawn models where coordinators are spawned as pure subagents (Task without team_name) while work agents are spawned as teammates create addressing mismatches. Work agents attempt to SendMessage to coordinators who have no team mailbox infrastructure, resulting in 0% message delivery, exponential retry loops, and complete phase coordination failure.

   **Root Cause**: Coordinators outside team communication system cannot receive messages from work agents. The Task tool has two modes:
   - WITH team_name: Spawns agent as teammate with SendMessage/Mailbox/TaskList tools
   - WITHOUT team_name: Spawns agent as pure subagent with no team communication infrastructure

   **Bobert Action**:
   - Phase 0-4: Spawn ALL coordinators using `Task tool WITH team_name`
   - Phase 0-4: Spawn ALL work agents using `Task tool WITH team_name`
   - NEVER spawn coordinators as pure subagents (Task without team_name)
   - VERIFY all agents have team communication tools before delegation begins

   **Observable Symptoms of Violation**:
   - Work agents report "SendMessage failed" when attempting to contact coordinator
   - Coordinator never receives messages from work agents despite agents showing message as sent
   - Phase coordination collapses into retry loops as agents cannot report status
   - Result: 0% message delivery rate, phase cannot complete

   **Take 9 Lesson**: Inconsistent spawn mechanism (coordinators as subagents, work agents as teammates) caused complete message infrastructure failure. This constraint prevents recurrence.

### Strategic vs Tactical Separation

**Bobert Retains (Strategic)**:
- Which coordinator to use for each phase (intake, research-design, implementation, finalization)
- A subset of direct reports (todo-spec-memory-maintainer, retro-maintainer)
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

## Usage & Testing Guidance

### Bobert's Execution Steps

1. **Form Team**: Use TeamCreate to create team named "Task Group A - [brief work description]"

2. **Phase 0 Execution**:
   ```
   Construct PhaseContext for Phase 0 (include scopeAnchor from Plan phase)
   Spaw retrospective-maintainer as teammate, instruct to remain on standby
   Spaw todo-spec-memory-maintainer as teammate, instruct to remain on standby
   Spawn intake-coordinator as teammate (Task tool WITH team_name="task-group-a-{ticket}")
   Send PhaseContext to intake-coordinator via SendMessage with roster request
   Receive roster specification from intake-coordinator (work-starter, worktree-manager)
   Spawn work agents as teammates per roster specification
   Notify intake-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from intake-coordinator
   Validate: status == COMPLETE, TODO UUID present, worktree path present
   Validate: gapAnalysisMemoryUUID present in PhaseResult
   Validate: gapAnalysisQuestionCount present in PhaseResult
   Scope check: TODO memory's scope aligns with ticket requirement (scopeAnchor)
   ```

3. **Gap Analysis Gate** (between Phase 0 and Phase 1):

   1. Read `gapAnalysisQuestionCount` from Phase 0 PhaseResult
   2. Present to Addison:
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
   3. PAUSE: Wait for Addison to reply "done"
   4. Load gap analysis memory: `read_memory(gapAnalysisMemoryUUID)`
   5. Parse each question's Resolution field:
         - `ANSWERED`             → add to `answeredGaps` for Phase 1 PhaseContext
         - `DEFERRED_TO_RESEARCH` → add to `resolutionQuestions` for Phase 1 PhaseContext
         - `ACCEPTED_DEFAULT` or blank → document as assumption in Phase 1 PhaseContext constraints; do NOT add to resolutionQuestions
   6. Proceed to Phase 1

4. **Phase 1 Execution**:
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

5. **Phase 2 Execution**:
   ```
   Construct PhaseContext for Phase 2 (pass impl plan UUID, worktree path)
   Spawn implementation-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to implementation-coordinator via SendMessage with roster request. Include instructions for the coordinator to use read_memory to read our current context, and to use todo-spec-memory maintainer to start creating TODOS for its roster.
   Receive roster specification (code-monkey, git-historian, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify implementation-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from implementation-coordinator
   Validate: status == COMPLETE, commit SHAs present, working tree clean, tests passing
   Have todo-spec-memory-maintainer add all documents created in phase 2 to Required Reading (ADRs, technical breakdown, implementation plan).
   Note: Test passing scope documented in Key Characteristics section #5 (default: new/modified tests)
   ```

6. **Phase 3 Execution**:
   ```
   PREREQUISITE: Branch must be pushed to remote before finalization begins
   - Validate: git ls-remote origin <branch>
   - This is a human-only operation when using SSH hardware keys (ED25519-SK)
   - SSH hardware keys require interactive approval for push operations
   - This is a security feature and quality gate, not a bug
   - If branch is not on remote, STOP and request human to push branch

   Construct PhaseContext for Phase 3 (pass commits, worktree path, branchPushed: true)
   Spawn finalization-coordinator as teammate (Task tool WITH team_name)
   Send PhaseContext to finalization-coordinator via SendMessage with roster request Include instructions for the coordinator to use read_memory to read our current context, and use todo-spec-memory maintainer to start creating TODOs for its roster.
   Receive roster specification (reuse technical-breakdown-maintainer, pr-maintainer, reuse todo-spec-memory-maintainer)
   Spawn work agents as teammates per roster specification (or notify existing agents if reused)
   Notify finalization-coordinator that roster is ready via SendMessage
   Wait for PhaseResult from finalization-coordinator
   Validate: status == COMPLETE, PR URL present
   ```

7. **Phase 4 Execution**:
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

8. **Assert Phase**: Review all PhaseResults, verify deliverables, report completion to Addison

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

