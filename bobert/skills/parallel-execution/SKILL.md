---
name: parallel-execution
description: |
  Guides identification of independent work streams and parallel specialist execution
  for tasks with multiple separable dimensions (research + implementation + docs).
  Synthesizes results with integration validation. Use when a more specialized team 
  does not already exist.
---

# parallel-execution Skill

When invoked, this skill provides instructions for identifying independent work streams, spawning parallel specialists, synthesizing their results, and validating integration.

**When to use**: Tasks with 2+ independent work dimensions that can execute simultaneously with minimal interdependencies. Examples: research + implementation + documentation in parallel, frontend + backend + testing concurrently.

**When NOT to use**: Sequential dependencies prevent parallelization, single-dimensional work, or integration complexity exceeds parallelization value.

## Core Identity

**Pattern**: Sectioning - breaking work into independent subtasks that execute simultaneously.

**Key Characteristics**:
- Work streams are genuinely independent (minimal blocking dependencies)
- Each stream has clear specialist requirements
- Parallel execution provides > 30% time savings vs sequential
- Results can be synthesized with defined integration approach

## When to Use This Skill

**Use parallel-execution when ALL of these conditions are true**:

1. **Multiple Independent Dimensions**: Task has 2+ separable work streams (e.g., research one topic while implementing another, document existing code while writing new features)

2. **Minimal Blocking Dependencies**: Work streams can progress independently without constant cross-coordination. Some integration at the end is expected, but streams don't block each other during execution.

3. **Clear Specialist Boundaries**: Each work stream maps to distinct specialist capabilities (deep-researcher for domain research, code-monkey for implementation, documentation-specialist for docs).

4. **Substantial Scope**: Combined work effort > 30 minutes where parallel execution provides meaningful time savings.

5. **Integration Feasibility**: Results from parallel streams can be combined with a clear approach (merge implementations, synthesize research findings, integrate documentation).

**Do NOT use when**:
- Work is single-dimensional with no natural decomposition
- Sequential dependencies require streams to complete in order
- Integration complexity exceeds time savings from parallelization
- Rapid turnaround (< 30 min total) where coordination overhead exceeds value

## Work Stream Identification

Follow this process to identify independent work streams:

### Step 1: Analyze Task Dimensions

Ask: "What are the separable dimensions of this work?"

**Common Patterns**:
- **Research + Implementation**: Investigate domain knowledge while building code
- **Frontend + Backend**: Separate UI and API development
- **Implementation + Documentation**: Write code while documenting architecture
- **Multiple Research Streams**: Parallel investigation of independent topics
- **Feature + Tests**: Build functionality while writing test suites
- **Code + Infrastructure**: Implement logic while configuring deployment

### Step 2: Validate Independence

For each potential work stream, verify:

**Independence Test**: Can this stream progress for > 50% of its duration without blocking on other streams?
- Pass: Research can complete before implementation starts using its findings
- Fail: Backend API must be defined before frontend can make any progress

**Specialist Boundary Test**: Does this stream require distinct expertise?
- Pass: Deep research (deep-researcher) vs implementation (code-monkey)
- Fail: Both streams require the same implementation skills

**Integration Feasibility Test**: Can results be combined with a clear approach?
- Pass: Research findings inform implementation decisions; documentation integrates with code
- Fail: Streams produce conflicting architectures requiring extensive reconciliation

### Step 3: Check for Sequential Dependencies

Identify any hard dependencies between streams:
- Must Stream A complete before Stream B can start?
- Does Stream B need 80%+ of Stream A's output to make progress?

If yes, consider sequential execution or adjust work stream boundaries.

### Step 4: Consult User When Uncertain

If work stream boundaries are ambiguous or dependencies are unclear, **STOP and prompt Addison**:

"Bobert has identified potential parallel work streams:
- Stream A: [description]
- Stream B: [description]

However, [specific uncertainty: unclear dependencies, integration concerns, boundary ambiguity].

Should Bobert proceed with parallel execution, or would sequential execution be more appropriate?"

## Team Composition Guidance

After identifying work streams, select specialists for each stream:

### Common Specialist Mappings

| Work Stream Type | Specialist Agent(s) |
|------------------|---------------------|
| Domain research | deep-researcher (with Learning Packets) |
| Codebase exploration | Explore agent (with thoroughness level) |
| Architecture design | Plan agent or adr-maintainer |
| Implementation | code-monkey or domain-specific impl agent |
| Documentation | technical-breakdown-maintainer or doc specialist |
| Testing | Test agent or QA specialist |
| Infrastructure | DevOps agent or platform engineer |

### Multi-Agent Streams

Some streams may require multiple specialists:
- **Research Stream**: deep-researcher (domain knowledge) + Explore agent (codebase patterns)
- **Implementation Stream**: code-monkey (implementation) + git-historian (commits)
- **Documentation Stream**: adr-maintainer (design decisions) + technical-breakdown-maintainer (synthesis)

### Roster Construction

Create agent roster with explicit responsibilities:

```
Agent Roster:
1. [specialist-1]: [Specific work stream responsibility]
   - Input: [What they need to start]
   - Output: [What they will produce]
   - Independence: [Why they can work without blocking others]

2. [specialist-2]: [Specific work stream responsibility]
   - Input: [What they need to start]
   - Output: [What they will produce]
   - Independence: [Why they can work without blocking others]
```

## Result Synthesis

After parallel specialists complete their work, synthesize results:

### Step 1: Collect Outputs

Gather completed work from each specialist:
- Research findings and Learning Packets
- Implementation code and commits
- Documentation and design decisions
- Test results and quality metrics

### Step 2: Integration Approach

Define how results combine:

**Common Integration Patterns**:
- **Research → Implementation**: Use research findings to inform implementation decisions
- **Code + Documentation**: Integrate docs with implemented functionality
- **Frontend + Backend**: Verify API contracts match between layers
- **Multiple Research Streams**: Synthesize findings into coherent knowledge base

### Step 3: Verify Consistency

Check that parallel work aligns:
- Do implementations follow research recommendations?
- Does documentation accurately reflect implementation?
- Are there conflicts or gaps between work streams?
- Do all streams contribute to the overall goal?

### Step 4: Document Integration

Record how parallel work was combined:
- Which research findings informed which implementation decisions
- How documentation integrates with code
- Any conflicts resolved during synthesis
- Overall coherence assessment

## Integration Validation

Validate that synthesized results achieve the original goal:

### Validation Checklist

- [ ] **Completeness**: All work streams produced expected outputs
- [ ] **Consistency**: No conflicts between parallel work streams
- [ ] **Alignment**: Combined work achieves the original task goal
- [ ] **Quality**: Each stream meets quality standards independently
- [ ] **Integration**: Results combine coherently without gaps or overlaps

### Validation Questions

1. **Goal Achievement**: Does the synthesized result accomplish what was requested?
2. **Missing Gaps**: Are there areas where parallel streams didn't cover necessary ground?
3. **Redundancy**: Did multiple streams produce overlapping work that needs deduplication?
4. **Conflict Resolution**: Were there contradictions that required reconciliation?

### Post-Validation Actions

**If validation passes**: Proceed to commit phase (delegate to git-historian).

**If validation fails**: Identify which stream needs remediation:
- Missing functionality? → Re-engage implementation specialist
- Conflicting architecture? → Engage Plan agent to resolve design conflicts
- Incomplete research? → Re-engage deep-researcher with focused questions
- Documentation gaps? → Re-engage documentation specialist

## Environment Dependencies

**No external dependencies** - This is an instruction-only skill.

**Agent Ecosystem Requirements**:
- Access to specialist agents for parallel work streams (deep-researcher, code-monkey, Explore agent, etc.)
- Team coordination capabilities (TeamCreate, TeamSpawn, TaskList, TaskUpdate)
- Mailbox for async teammate communication

## Usage & Testing Guidance

### Example Invocation 1: Research + Implementation + Docs

**Scenario**: Build new authentication system requiring domain research, implementation, and documentation.

**Work Stream Identification**:
- Stream A: Research OAuth2 best practices (deep-researcher)
- Stream B: Implement auth middleware (code-monkey)
- Stream C: Document architecture decisions (adr-maintainer)

**Independence**: Research can complete independently, implementation can proceed with existing patterns, documentation captures concurrent decisions.

**Integration**: Research findings validate implementation approach; ADRs document both research conclusions and implementation decisions.

### Example Invocation 2: Frontend + Backend

**Scenario**: New user dashboard feature.

**Work Stream Identification**:
- Stream A: Backend API endpoints (backend-impl-agent)
- Stream B: Frontend React components (frontend-impl-agent)

**Sequential Dependency Check**: Frontend needs API contract defined before making progress → **NOT suitable for full parallelization**. Consider: Define API contract first, THEN parallelize implementation.

**Adjusted Approach**: Sequential design phase, then parallel implementation.

### Example Invocation 3: Multiple Research Topics

**Scenario**: Investigate three independent architecture patterns.

**Work Stream Identification**:
- Stream A: Research microservices patterns (deep-researcher-1)
- Stream B: Research event sourcing patterns (deep-researcher-2)
- Stream C: Research API gateway patterns (deep-researcher-3)

**Independence**: Three separate research domains with no blocking dependencies.

**Integration**: Synthesize findings into comparative analysis of architecture options.

### Claude Code Invocation

When Bobert identifies parallel work:

```
/parallel-execution
```

This loads instructions for:
1. Validating work stream independence
2. Selecting appropriate specialists
3. Spawning parallel agents via TeamCreate/TeamSpawn
4. Synthesizing results with integration validation
5. Handling validation failures with targeted remediation

### Testing the Skill

**Install the skill**:
```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/parallel-execution/
```

**Test invocation**:
When coordinating parallel work, invoke `/parallel-execution` to load guidance on work stream identification, specialist selection, and result synthesis.

**Validation**: Skill provides clear decision frameworks for identifying independent work streams and actionable guidance for integration validation.
