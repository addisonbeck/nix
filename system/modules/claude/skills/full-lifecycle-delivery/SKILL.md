---
name: full-lifecycle-delivery
description: |
  Orchestration playbook for taking work from input (Jira ticket, memory stub, or prompt) through intake, research/design, implementation, and finalization to draft PR. Defines team composition (11 work agents + 4 phase coordinators), phase-by-phase workflow, PhaseContext/PhaseResult protocol, and completion criteria for full-lifecycle delivery. Use when asked to research, design, and publish a draft PR from a Jira ticket, or when prompted to use Task Group A.
---

# full-lifecycle-delivery Skill

This skill provides orchestration guidance for **Task Group A**, Bobert's canonical team composition for full-lifecycle work delivery. Task Group A takes work from initial input (Jira ticket, memory stub, or plain prompt) through intake, research, design, implementation, and finalization to produce a draft pull request.

When invoked, Bobert follows this instruction playbook to:
1. Form the 11-agent work team plus 4 phase coordinators
2. Execute four sequential phases (Intake → Research/Design → Implementation → Finalization)
3. Construct PhaseContext for each phase with agent roster and completion criteria
4. Delegate tactical phase management to specialized coordinators
5. Validate PhaseResults before advancing to next phase
6. Maintain continuous TODO updates throughout all phases

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
- **Phase 0 Outputs**: TODO UUID, worktree path, clarified requirements
- **Phase 1 Outputs**: Technical breakdown UUID (v1.0.0), implementation plan UUID
- **Phase 2 Outputs**: Commit SHAs, clean working tree confirmation, passing tests
- **Phase 3 Outputs**: Draft PR URL, completed TODO, finalized technical breakdown

### Success Criteria
All four phases complete with status: COMPLETE and required outputs present.

### Error Conditions
- PhaseResult with status: FAILED triggers Bobert to decide: retry, adjust, or abort
- Coordinator escalations (scope change, scope misalignment, goal conflict) require Bobert's strategic decision
- Scope validation failure (scopeValidation fields false) blocks phase advancement until Bobert investigates
- "Implementation complete + open ticket" contradiction requires Addison consultation
- Missing required outputs block phase advancement

## Implementation Architecture

This is an **instruction-only skill** - no bash script implementation. Bobert loads this skill and follows the guidance.

### Team Composition (11 Work Agents + 4 Phase Coordinators)

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
|  Bobert provides: PhaseContext with input + roster      |
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
|  Bobert provides: PhaseContext with TODO UUID + roster   |
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
|  Coordinator returns: PhaseResult with PR URL           |
|                                                          |
|  COMPLETION: Draft PR created                           |
+---------------------------------------------------------+
             |
             v
        DRAFT PR CREATED
```

### PhaseContext Structure

Bobert constructs PhaseContext for each phase containing:

```json
{
  "phaseId": "phase-N-name",
  "phaseGoal": "Clear statement of what this phase must accomplish",
  "agentRoster": [
    {"name": "agent-name", "role": "What this agent does in this phase"}
  ],
  "completionCriteria": {
    "requiredOutputs": ["Output 1", "Output 2"],
    "validationCommands": ["command to verify output 1", "command to verify output 2"]
  },
  "constraints": {
    "scopeBoundaries": ["What is IN scope", "What is OUT of scope"],
    "timeBox": "Expected duration"
  },
  "prerequisites": {
    "input": "Data or context from prior phases or initial input"
  },
  "scopeAnchor": {
    "ticketRequirement": "What the ticket specifically asks to be BUILT/CHANGED/ADDED",
    "deliverableScope": "The NEW thing that must be produced -- not the existing thing",
    "prerequisiteContext": "What already exists and provides context, but is NOT the deliverable",
    "fulfillmentTest": "How we verify deliverables address the ticket, not just describe the codebase"
  }
}
```

**Scope Anchor**: Bobert populates `scopeAnchor` during Plan phase and carries it forward through all PhaseContexts. This ensures every coordinator can validate that their phase's deliverables address the ticket's actual requirement rather than documenting existing code. The scope anchor is derived from the ticket requirement analysis performed during Plan phase.

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

4. **Context Isolation**: Each specialist operates in own context window, activating per phase. This prevents context pollution and allows focused work.

5. **Specification Bridge**: `implementation-plan-maintainer` in Phase 1 translates architecture into executable step-by-step specifications before Phase 2 begins. This bridges strategic design to tactical implementation.

6. **Input Source Flexibility**: `work-starter` adapts intake to handle Jira tickets, memory stubs, or plain prompts uniformly.

### Strategic vs Tactical Separation

**Bobert Retains (Strategic)**:
- Which agents go on which roster (team composition)
- Whether to advance to next phase (phase transition authorization)
- How to handle coordinator escalations (scope changes, goal conflicts)
- Whether to abort, retry, or adjust after FAILED PhaseResults
- Cross-phase state tracking and quality gates

**Coordinators Handle (Tactical)**:
- Agent spawning from provided roster (no autonomous agent selection)
- Task distribution via TaskList
- Progress monitoring and status updates
- Completion validation against explicit criteria
- Structured PhaseResult reporting

## Environment Dependencies

- **ORG_ROAM_DIR**: Required for TODO and memory creation/maintenance
- **Required agents**: All 11 work agents + 4 phase coordinators must exist in `~/.claude/agents/`
- **Required skills**: `create_memory`, `read_memory`, `todo-writer` must exist for TODO maintenance
- **Git environment**: Worktree operations require git configuration
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
   Delegate to intake-coordinator via Task tool
   Wait for PhaseResult
   Validate: status == COMPLETE, TODO UUID present, worktree path present
   Scope check: TODO memory's scope aligns with ticket requirement (scopeAnchor)
   ```

3. **Phase 1 Execution**:
   ```
   Construct PhaseContext for Phase 1 (pass TODO UUID from Phase 0 + scopeAnchor)
   Delegate to research-design-coordinator via Task tool
   Wait for PhaseResult
   Validate: status == COMPLETE, breakdown UUID present (v1.0.0), impl plan UUID present
   Scope check: scopeValidation.deliverableAddressed == true
   Scope check: Technical breakdown describes NEW work, not existing implementation
   If scopeValidation fails: Do NOT advance to Phase 2 -- investigate scope misalignment
   ```

4. **Phase 2 Execution**:
   ```
   Construct PhaseContext for Phase 2 (pass impl plan UUID, worktree path)
   Delegate to implementation-coordinator via Task tool
   Wait for PhaseResult
   Validate: status == COMPLETE, commit SHAs present, working tree clean
   ```

5. **Phase 3 Execution**:
   ```
   Construct PhaseContext for Phase 3 (pass commits, worktree path)
   Delegate to finalization-coordinator via Task tool
   Wait for PhaseResult
   Validate: status == COMPLETE, PR URL present
   ```

6. **Assert Phase**: Review all PhaseResults, verify deliverables, report completion to Addison

### When to Consult Addison

Bobert should consult Addison (not proceed autonomously) when:
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
