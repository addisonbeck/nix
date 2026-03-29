---
name: phased-coordination
description: |
  Guides Bobert through coordinating multi-phase sequential work where phases must
  complete in order with clear handoffs (e.g., research → design → implement,
  analyze → plan → execute). Provides phase structure templates, agent selection
  criteria, handoff protocols, and completion validation guidance. Use when a more 
  specialized team does not already exist.
---

# phased-coordination Skill

Guides Bobert through coordinating multi-phase sequential work with clear phase boundaries, agent selection criteria, and handoff protocols.

When invoked, this skill loads behavioral guidance into Bobert's context for structuring work into sequential phases where each phase must complete before the next begins.

## Purpose & When to Use

Use **phased-coordination** when:

- Work naturally decomposes into **sequential phases** with clear dependencies (Phase B cannot start until Phase A completes)
- Each phase requires **different specialist agents** or capabilities
- Phases have **distinct completion criteria** and handoff requirements
- Work benefits from **strategic phase transitions** where Bobert validates outputs before proceeding

**Examples of Phased Work**:
- Research → Design → Implementation (knowledge gathering, architecture decisions, code writing)
- Analyze → Plan → Execute (problem diagnosis, solution design, changes)
- Explore → Specify → Validate (discovery, requirements definition, verification)
- Intake → Research → Synthesis (requirements gathering, investigation, documentation)

**Do NOT use phased-coordination when**:
- Work is naturally **parallel** with independent dimensions → Use team-coordination instead
- Work is a **single-phase operation** → Delegate to individual agent
- Phases are predefined with fixed 11-agent roster → Use full-lifecycle-delivery skill instead
- Tactical execution needs management → Delegate to phase coordinator agents

**Key Distinction**: phased-coordination is for **generic multi-phase patterns** where Bobert selects agents per phase. full-lifecycle-delivery is a **specific 11-agent recipe** with predefined roster and coordinator delegation.

## Input Contract

This is an instruction-only skill. No JSON input required.

**Bobert receives** (from context):
- Work description from Addison
- Phase structure inferred from work requirements
- Agent roster available for delegation

## Output Contract

This skill produces **guidance behavior**, not data output.

**Bobert's behavior after loading this skill**:
- Structures work into sequential phases with clear boundaries
- Selects appropriate agents for each phase based on capabilities
- Validates phase completion before proceeding to next phase
- Maintains strategic oversight across all phases
- Consults Addison when agent selection is unclear

## Implementation Architecture

**Implementation Type**: Instruction-only skill (no bash script)

This skill loads behavioral instructions into Bobert's context, guiding coordination patterns for multi-phase sequential work.

### Core Instruction Components

1. **Phase Structure Templates**: Generic patterns for common phase sequences
2. **Agent Selection Criteria**: How to match phase requirements to agent capabilities
3. **User Consultation Protocol**: When and how to prompt Addison for clarification
4. **Handoff Requirements**: What must be complete before phase transitions
5. **Completion Validation**: How to verify each phase achieved its goals

### Behavioral Framework

When this skill is active, Bobert follows this coordination pattern:

```
1. Analyze work → Identify natural phase boundaries
2. For each phase:
   a. Define phase goal and completion criteria
   b. Select appropriate agent(s) for phase capabilities
   c. If agent selection unclear → Consult Addison
   d. Delegate phase to selected agent(s)
   e. Wait for phase completion
   f. Validate phase outputs against completion criteria
   g. If validation fails → Retry or escalate
   h. If validation succeeds → Proceed to next phase
3. After all phases complete → Proceed to Assert
```

## Environment Dependencies

- **Task tool**: Required for agent delegation
- **SendMessage tool**: Required for coordination communication
- **TaskList tool**: Required for tracking phase completion status
- **No bash dependencies**: Instruction-only skill

## Usage & Testing Guidance

### Invocation Pattern

Bobert activates this skill when work fits the phased coordination pattern:

```
Addison: "Research best practices for X, design architecture, then implement"

Bobert: [Loads phased-coordination skill]

## Plan (with phased-coordination guidance)

**Goal**: Deliver X implementation through research → design → implementation phases

**Phase Structure Analysis**:
- Phase 1 (Research): Investigate best practices for X
  - Agent: deep-researcher (domain knowledge gathering)
  - Completion: Learning packet with best practices documented
- Phase 2 (Design): Create architecture based on research findings
  - Agent: adr-maintainer (architecture decisions)
  - Completion: ADR documenting architecture with justification
- Phase 3 (Implementation): Write code following architecture
  - Agent: code-monkey (implementation)
  - Completion: Code committed, tests passing

**Handoff Protocol**:
- Research findings feed into design decisions
- Architecture constraints guide implementation approach
- Each phase validates completion before next begins

## Execute
[Bobert delegates Phase 1, waits for completion, validates]
[Bobert delegates Phase 2 with Phase 1 outputs, waits, validates]
[Bobert delegates Phase 3 with Phase 2 outputs, waits, validates]

## Assert
- [x] All phases complete with validated outputs
- [x] Sequential handoffs provided necessary context
- [x] Final implementation aligns with research and design
```

### When Agent Selection is Unclear

If Bobert cannot determine appropriate agents for a phase, this skill guides consultation:

```
Bobert: "Phase 2 requires API contract validation, but no specialized validator
agent exists. Addison, should Bobert:
A) Use Explore agent as gap-filler with explicit validation constraints?
B) Create temporary validation checklist and delegate to code-review agent?
C) You recommend a different approach?

Please advise before Bobert proceeds with Phase 2."
```

### Phase Structure Templates

**Research → Design → Implement**:
- Phase 1: deep-researcher (investigation, best practices)
- Phase 2: adr-maintainer (architecture decisions)
- Phase 3: code-monkey + git-historian (implementation + commit)

**Analyze → Plan → Execute**:
- Phase 1: Explore agent (codebase investigation, problem diagnosis)
- Phase 2: Plan agent (solution design, step-by-step approach)
- Phase 3: Appropriate implementation agent (changes + verification)

**Intake → Research → Synthesis**:
- Phase 1: work-starter (requirements gathering, gap identification)
- Phase 2: deep-researcher (domain investigation)
- Phase 3: technical-breakdown-maintainer (documentation synthesis)

**Custom Phases**:
- Bobert infers phase structure from work requirements
- Selects agents based on phase capabilities needed
- Consults Addison when structure or selection is ambiguous

### Validation Checklist

After loading this skill, verify Bobert exhibits these behaviors:

- [ ] **Clear Phase Boundaries**: Bobert articulates distinct phases with specific goals
- [ ] **Agent Selection Reasoning**: Bobert explains why each agent matches phase needs
- [ ] **User Consultation**: Bobert prompts Addison when agent selection is unclear
- [ ] **Sequential Execution**: Bobert waits for phase completion before proceeding
- [ ] **Completion Validation**: Bobert verifies each phase achieved its goal
- [ ] **Handoff Context**: Bobert passes phase outputs to subsequent phases
- [ ] **Strategic Oversight**: Bobert maintains authority over phase transitions

### Installation

```bash
# Build and install skill
nix develop .#building --command rebuild <hostname>

# Verify skill appears
ls ~/.claude/skills/phased-coordination/

# Skill is loaded automatically when work matches phased pattern
```

## Core Competencies (Detailed Guidance for Bobert)

When phased-coordination skill is active, Bobert gains these specialized competencies:

### 1. Phase Boundary Identification

**Skill**: Recognize when work naturally decomposes into sequential phases

**Indicators of Phase Boundaries**:
- Knowledge dependency: "We need to understand X before we can design Y"
- Output type changes: Research produces knowledge, design produces decisions, implementation produces code
- Different agent capabilities required: Investigation vs architecture vs coding
- Clear handoff points: "Once we know the requirements, we can plan the approach"

**Anti-Pattern**: Forcing work into phases when it's naturally parallel or single-step

### 2. Agent Selection Per Phase

**Skill**: Match phase requirements to agent capabilities

**Selection Criteria**:

| Phase Type | Typical Agents | Capabilities Needed |
|------------|----------------|---------------------|
| **Research/Investigation** | deep-researcher, Explore | Domain knowledge gathering, codebase exploration |
| **Design/Architecture** | adr-maintainer, Plan | Decision making, solution design |
| **Implementation** | code-monkey, implementation agents | Code writing, testing |
| **Documentation** | technical-breakdown-maintainer | Synthesis, structured writing |
| **Validation** | code-review, test agents | Quality assurance, verification |

**When Selection is Unclear**:
- No specialized agent exists for phase needs → Consult Addison about gap-filler or creating new agent
- Multiple agents could fit → Explain options to Addison with tradeoffs
- Phase needs are ambiguous → Ask clarifying questions before selecting agent

### 3. User Consultation Protocol

**When to Consult Addison**:

1. **Ambiguous Phase Structure**: "This work could be 2 phases or 4 phases depending on depth. What's your preference?"
2. **Missing Specialist Agent**: "Phase requires X capability but no specialized agent exists. Options: A) gap-filler, B) create agent, C) restructure phases?"
3. **Conflicting Requirements**: "Research phase suggests approach A, but design constraints favor approach B. Which takes priority?"
4. **Scope Uncertainty**: "Should this phase include X or defer it to later? Clarify scope before proceeding?"

**How to Consult**:
- State the decision point clearly
- Provide 2-3 concrete options with tradeoffs
- Explain why you cannot proceed without guidance
- Wait for explicit direction before continuing

**Example Consultation**:
```
Bobert encounters decision point at Phase 2: Agent selection unclear

Bobert: "Phase 2 requires API contract validation. No dedicated validator agent exists.

Options:
A) Use Explore agent as gap-filler with explicit validation constraints
   - Pro: Can proceed immediately
   - Con: Explore not specialized for validation
B) Create temporary validation checklist and use code-review agent
   - Pro: Leverages existing review capabilities
   - Con: Code-review designed for post-implementation review
C) Skip validation, proceed to implementation, validate later
   - Pro: Maintains forward momentum
   - Con: May require rework if contracts incorrect

Addison, which approach should Bobert take? Or do you recommend different approach?"

[Bobert WAITS for Addison's guidance before Phase 2]
```

### 4. Phase Handoff Protocol

**Handoff Requirements** (what must be complete before phase transition):

1. **Phase Goal Achieved**: Core objective of phase demonstrably complete
2. **Outputs Validated**: Deliverables meet quality criteria
3. **Handoff Artifacts Ready**: Next phase has necessary inputs from this phase
4. **No Blocking Issues**: Errors or failures resolved before proceeding

**Handoff Checklist**:
- [ ] Agent reports phase completion
- [ ] Bobert validates outputs against phase goal
- [ ] Artifacts exist and are accessible to next phase
- [ ] No unresolved errors or failures in agent output
- [ ] Next phase prerequisites satisfied

**Handoff Context to Provide**:
- What the previous phase produced (paths, UUIDs, key findings)
- Relevant constraints or decisions made
- What the next phase should accomplish
- How outputs integrate with prior work

**Example Handoff**:
```
Phase 1 (Research) Complete:
- Learning packet UUID: ABC123
- Key finding: X pattern preferred over Y pattern for Z reasons
- Constraint identified: Must maintain backward compatibility

Handoff to Phase 2 (Design):
- Design architecture incorporating X pattern
- Respect backward compatibility constraint from research
- Reference learning packet ABC123 for detailed justification
```

### 5. Completion Validation

**Per-Phase Validation**: Before proceeding to next phase, verify:

1. **Goal Achievement**: Did this phase accomplish what it set out to do?
2. **Output Quality**: Are deliverables complete and correct?
3. **Integration Readiness**: Can next phase proceed with these outputs?

**Validation Methods**:
- Read files to confirm artifacts exist
- Check agent output for error messages
- Verify outputs match expected format/schema
- Confirm outputs address phase goal

**If Validation Fails**:
- Diagnose what's missing or incorrect
- Retry phase with clarified instructions OR
- Escalate to Addison if structural issue OR
- Adjust phase approach and re-delegate

**Example Validation**:
```
## Assert (Phase 1 - Research)

Validating deep-researcher output:
- [ ] Learning packet created? [Read to verify]
- [ ] Key findings documented? [Check packet content]
- [ ] Best practices identified? [Review recommendations]
- [ ] Sources cited? [Validate learning packet structure]

Result: ✅ All criteria met. Proceed to Phase 2.
```

### 6. Strategic Phase Transition Authority

**Bobert Retains Strategic Authority**:
- Whether to advance to next phase (validation-gated)
- Whether to retry failed phase or escalate
- Whether to adjust phase structure if work evolves
- Whether to consult Addison on unexpected developments

**Bobert Delegates Tactical Execution**:
- How agent accomplishes phase goal (agent autonomy)
- Tool selection within phase (agent decides)
- Internal agent workflow (agent manages)

**Phase Transition Decision**:
```
After Phase 2 completes:

Bobert evaluates:
1. Did phase achieve its goal? [Yes/No + reasoning]
2. Are outputs complete and correct? [Validation results]
3. Can Phase 3 proceed with these outputs? [Readiness check]
4. Any blocking issues? [Error scan]

IF all checks pass:
  -> Proceed to Phase 3
ELSE IF retryable issue:
  -> Retry Phase 2 with adjusted approach
ELSE IF structural problem:
  -> Escalate to Addison with diagnosis
```

## Phase Structure Templates (Reference)

### Template 1: Research → Design → Implement

**Use When**: Building new functionality requiring domain knowledge and architectural decisions

**Phase 1 - Research**:
- Agent: deep-researcher
- Goal: Investigate domain, best practices, constraints
- Completion Criteria: Learning packet with findings and recommendations
- Handoff: Key patterns, constraints, recommendations to design phase

**Phase 2 - Design**:
- Agent: adr-maintainer
- Goal: Create architecture based on research findings
- Completion Criteria: ADR documenting decisions with justification
- Handoff: Architecture specification to implementation phase

**Phase 3 - Implement**:
- Agent: code-monkey + git-historian
- Goal: Write code following architecture, commit changes
- Completion Criteria: Implementation complete, tests passing, changes committed
- Handoff: Complete (work delivered)

### Template 2: Analyze → Plan → Execute

**Use When**: Addressing bugs, technical debt, or refactoring

**Phase 1 - Analyze**:
- Agent: Explore agent
- Goal: Diagnose problem, understand codebase context
- Completion Criteria: Problem diagnosis with root cause identified
- Handoff: Diagnosis and relevant code paths to planning phase

**Phase 2 - Plan**:
- Agent: Plan agent
- Goal: Design solution approach with step-by-step plan
- Completion Criteria: Detailed implementation plan with risks noted
- Handoff: Executable plan to execution phase

**Phase 3 - Execute**:
- Agent: Appropriate implementation agent
- Goal: Apply solution following plan, verify fix
- Completion Criteria: Changes implemented, verified working, committed
- Handoff: Complete (work delivered)

### Template 3: Intake → Research → Synthesis

**Use When**: Converting vague requirements into structured documentation

**Phase 1 - Intake**:
- Agent: work-starter
- Goal: Clarify requirements, identify knowledge gaps
- Completion Criteria: Structured TODO with clear goals and questions
- Handoff: Requirements and research questions to research phase

**Phase 2 - Research**:
- Agent: deep-researcher
- Goal: Fill knowledge gaps identified in intake
- Completion Criteria: Learning packet answering research questions
- Handoff: Research findings to synthesis phase

**Phase 3 - Synthesis**:
- Agent: technical-breakdown-maintainer
- Goal: Combine intake + research into implementation-ready documentation
- Completion Criteria: Technical breakdown ready for implementation
- Handoff: Complete (work specified)

### Template 4: Custom Phase Structure

**Use When**: Work doesn't match standard templates

**Bobert's Approach**:
1. Analyze work to identify natural phase boundaries
2. For each phase, determine:
   - What must be accomplished (phase goal)
   - What agent capabilities are needed (selection criteria)
   - What outputs feed into next phase (handoff requirements)
   - What validates completion (success criteria)
3. If structure is unclear, consult Addison before proceeding
4. Execute phases sequentially with validation-gated transitions

**Example Custom Structure**:
```
Work: "Migrate database schema and update all dependent code"

Bobert identifies phases:
- Phase 1: Inventory (Explore - find all schema dependencies)
- Phase 2: Design Migration (Plan - design migration strategy)
- Phase 3: Schema Changes (Implementation - alter database)
- Phase 4: Code Updates (Implementation - update dependent code)
- Phase 5: Validation (Testing - verify migration success)

Rationale: Each phase builds on previous, cannot parallelize due to dependencies
```

## Anti-Patterns

**DON'T use phased-coordination when**:

1. **Work is naturally parallel**: Use team-coordination instead
   - Bad: Research + Implementation as sequential phases when they could run concurrently
   - Good: Recognize independent work streams and parallelize

2. **Single-dimensional work**: Delegate to individual agent
   - Bad: Creating phases for simple tasks just to use phased-coordination
   - Good: Reserve phasing for genuinely sequential dependencies

3. **Predefined roster with fixed phases**: Use full-lifecycle-delivery
   - Bad: Manually replicating Task Group A's 11-agent pattern with phased-coordination
   - Good: Recognize full-lifecycle pattern and use dedicated skill

4. **Tactical execution management**: Delegate to coordinator agents
   - Bad: Bobert managing individual agent spawning order within a phase
   - Good: Bobert validates phase completion, coordinators handle agent orchestration

## Integration with Claude Code Ecosystem

### Relationship to Other Coordination Patterns

**phased-coordination vs full-lifecycle-delivery**:
- full-lifecycle-delivery: Specific 11-agent recipe with predefined phases and coordinator delegation
- phased-coordination: Generic N-phase pattern where Bobert selects agents per phase

**phased-coordination vs team-coordination**:
- team-coordination: Parallel work with independent dimensions
- phased-coordination: Sequential work with phase dependencies

**phased-coordination vs coordinator agents**:
- Coordinator agents: Tactical execution (agent spawning, task lists, mailbox)
- phased-coordination: Strategic oversight (phase transitions, validation, escalation)

**Can Combine**:
- phased-coordination at strategic level, coordinators managing tactical phase execution
- phased-coordination for sequential dependencies, team-coordination within phases for parallel sub-work

### When to Escalate

**Escalate to Addison when**:
- Phase structure is ambiguous (could be 2 phases or 5 phases)
- Agent selection unclear (no obvious match for phase capabilities)
- Phase outputs reveal scope change (work bigger/different than expected)
- Blocking issue cannot be resolved by retry (structural problem)
- Conflicting constraints discovered (research findings vs requirements)

**Continue Without Consultation when**:
- Phase structure is clear from work description
- Agent capabilities obviously match phase needs
- Phases complete successfully with expected outputs
- Standard handoff protocol applies

---

This skill equips Bobert with specialized competencies for coordinating multi-phase sequential work, maintaining strategic oversight while delegating phase execution to appropriate specialist agents. It emphasizes clear phase boundaries, validation-gated transitions, and user consultation when decisions require human judgment.
