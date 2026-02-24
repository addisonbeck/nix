---
name: sequential-pipeline
description: |
  Orchestration playbook for managing fixed sequential delegation chains where each stage consumes the previous stage's output (prompt chaining pattern). Defines pipeline structure, agent selection guidance, output passing protocol, and validation criteria for 2-5 stage sequential workflows.
allowed-tools: Task, SendMessage
---

# sequential-pipeline Skill

This skill provides orchestration guidance for **sequential delegation pipelines**, a pattern where work flows through a fixed chain of stages with each stage consuming and transforming the output of the previous stage. This is the prompt chaining pattern: Stage 1 → Stage 2 → Stage 3, where each arrow represents explicit output handoff.

When invoked, Bobert follows this instruction playbook to:
1. Identify the stages in the sequential chain
2. Select appropriate agents for each stage based on capabilities
3. Execute stages sequentially with proper output passing
4. Validate each stage completion before proceeding
5. Aggregate final results and report to Addison

## Purpose & When to Use

Use sequential-pipeline when:
- **Fixed Stage Sequence**: Work follows a clear linear path (research → document, analyze → summarize → report)
- **Output Transformation**: Each stage consumes previous output and produces new artifact (data flows through chain)
- **Stage Dependencies**: Later stages strictly depend on earlier stages completing first (cannot parallelize)
- **Clear Handoff Points**: Explicit contracts exist between stages (JSON, file paths, structured data)
- **2-5 Stage Workflows**: Pipeline has well-defined beginning and end with intermediate transformations

Do NOT use sequential-pipeline when:
- **Parallel Work Possible**: Stages can run independently without strict ordering (use agent teams instead)
- **Single Stage**: Work is accomplished by one agent (direct delegation more appropriate)
- **Dynamic Branching**: Pipeline needs conditional logic or branching paths (requires custom orchestration)
- **Complex Phase Management**: Work needs iterative loops or multi-phase coordination (use full-lifecycle-delivery skill)
- **More than 5 stages**: Pipeline becomes unwieldy (consider decomposition or full team approach)

This pattern is ideal for linear transformation workflows where data or artifacts flow through a clear chain of processing steps.

## Input Contract

sequential-pipeline expects Bobert to identify:
- **pipeline** (array of stage objects, required): Ordered list of 2-5 stages
  - Each stage object contains:
    - **stageId** (string): Unique identifier (e.g., "stage-1-research")
    - **stageGoal** (string): Clear statement of what this stage accomplishes
    - **agent** (string): Which agent performs this stage's work
    - **inputFrom** (string): Where input comes from ("initial" for stage 1, previous stageId for later stages)
    - **outputFormat** (string): Description of what this stage produces (JSON schema, file path, structured summary)
    - **validationCriteria** (array of strings): Explicit checks to confirm stage completion
- **initialInput** (string|object): Starting data for the pipeline (passed to stage 1)
- **contextRequirements** (optional, array of strings): Additional context all agents should receive (e.g., memory UUIDs, file paths, constraints)

### Validation Rules
- Pipeline must have 2-5 stages (no single-stage or overly complex pipelines)
- Each stage (except first) must reference a previous stage in `inputFrom`
- Each stage must have at least one validation criterion
- Agent selection must align with stage goal capabilities

### Example Input
```json
{
  "pipeline": [
    {
      "stageId": "stage-1-research",
      "stageGoal": "Investigate best practices for error handling in React applications",
      "agent": "deep-researcher",
      "inputFrom": "initial",
      "outputFormat": "Learning Packet with sources and patterns",
      "validationCriteria": [
        "At least 5 sources cited",
        "Patterns are actionable and specific"
      ]
    },
    {
      "stageId": "stage-2-document",
      "stageGoal": "Create documentation guide from research findings",
      "agent": "technical-breakdown-maintainer",
      "inputFrom": "stage-1-research",
      "outputFormat": "Markdown documentation file with examples",
      "validationCriteria": [
        "All patterns from research are documented",
        "Code examples are included"
      ]
    }
  ],
  "initialInput": "We need comprehensive error handling standards for our React codebase",
  "contextRequirements": ["src/components/", "existing-error-patterns.md"]
}
```

Note: In practice, Bobert constructs this structure based on Addison's request rather than receiving explicit JSON. The schema above guides Bobert's pipeline definition process.

## Output Contract

sequential-pipeline produces:
- **Stage Outputs**: Each stage produces its defined output format (Learning Packet, documentation file, summary, etc.)
- **Final Output**: The output of the last stage in the pipeline
- **Validation Evidence**: For each stage, evidence that validation criteria were met
- **Pipeline Metrics**: Total stages completed, any failures, total duration

### Success Response
All stages complete with validation criteria satisfied. Final stage output is delivered to Addison.

### Error Conditions
- **Stage Failure**: A stage cannot complete (missing dependencies, insufficient information, agent error)
- **Validation Failure**: Stage output does not meet validation criteria
- **Handoff Failure**: Output format from one stage is incompatible with next stage's input expectations

When errors occur, Bobert decides:
- **Retry**: Re-execute failed stage with clarified prompt
- **Adjust**: Modify validation criteria or agent selection and retry
- **Escalate**: Consult Addison if repeated failures or fundamental blocker

## Implementation Architecture

This is an **instruction-only skill** - no bash script implementation. Bobert loads this skill and follows the guidance.

### Pipeline Execution Pattern

```
                  SEQUENTIAL PIPELINE PATTERN

Initial Input
    |
    v
+------------------------------------------+
| STAGE 1                                  |
|  Agent: [agent-1]                        |
|  Input: Initial input                    |
|  Goal: [stage 1 goal]                    |
|  Output: [stage 1 output format]         |
|  Validation: [criteria 1, criteria 2]    |
+------------------------------------------+
    |
    | Stage 1 Output (validated)
    v
+------------------------------------------+
| STAGE 2                                  |
|  Agent: [agent-2]                        |
|  Input: Stage 1 output                   |
|  Goal: [stage 2 goal]                    |
|  Output: [stage 2 output format]         |
|  Validation: [criteria 1, criteria 2]    |
+------------------------------------------+
    |
    | Stage 2 Output (validated)
    v
+------------------------------------------+
| STAGE N (final)                          |
|  Agent: [agent-N]                        |
|  Input: Stage N-1 output                 |
|  Goal: [stage N goal]                    |
|  Output: [final output format]           |
|  Validation: [criteria 1, criteria 2]    |
+------------------------------------------+
    |
    v
Final Output → Deliver to Addison
```

### Agent Selection Guidance

When defining the pipeline, Bobert selects agents based on stage goals:

**Research Stages**:
- `deep-researcher`: External research requiring Learning Packets and source analysis
- `Explore`: Codebase investigation and pattern discovery

**Analysis Stages**:
- `Plan`: Structured analysis and decision-making
- `code-review`: Code quality assessment and recommendations

**Documentation Stages**:
- `technical-breakdown-maintainer`: Context synthesis and technical documentation
- `adr-maintainer`: Architecture decision recording

**Implementation Stages**:
- `code-monkey`: Fast code modifications from clear specifications
- `worktree-manager`: Worktree operations and branch management

**Synthesis Stages**:
- `implementation-plan-maintainer`: Translating architecture into executable specifications
- `technical-breakdown-maintainer`: Combining multiple sources into coherent documentation

### Output Passing Protocol

Between stages, Bobert must:

1. **Capture Output**: Store the previous stage's output explicitly (file path, JSON data, summary text)
2. **Validate Output**: Check that output meets validation criteria before proceeding
3. **Construct Prompt**: Build the next stage's Task prompt including:
   - Stage goal
   - Input from previous stage (explicitly passed)
   - Output format expected
   - Context requirements
   - Validation criteria
4. **Delegate**: Use Task tool to spawn next agent with complete context
5. **Wait for Completion**: Do not proceed until current stage completes
6. **Validate Result**: Check validation criteria before advancing

**Example Prompt for Stage 2**:
```
Stage 2: Document research findings

Input from Stage 1 (deep-researcher Learning Packet):
[Full text of research output]

Your goal: Create documentation guide from research findings.

Output format: Markdown documentation file with examples.

Validation criteria:
- All patterns from research are documented
- Code examples are included

Context: src/components/, existing-error-patterns.md
```

### Validation Criteria Framework

Each stage must have explicit validation criteria. Common patterns:

**Quantitative Criteria**:
- "At least N sources cited"
- "Covers X specific topics"
- "Includes Y code examples"

**Qualitative Criteria**:
- "Patterns are actionable and specific"
- "Examples match project conventions"
- "Documentation is complete and clear"

**Artifact Criteria**:
- "File created at specified path"
- "JSON output matches schema"
- "Tests pass"

Bobert verifies these criteria using read-only tools (Read, Grep, Bash with ls/cat) before proceeding to the next stage.

## Environment Dependencies

- **Agent availability**: All agents referenced in pipeline stages must exist in `~/.claude/agents/`
- **Context access**: Files or memory nodes specified in contextRequirements must be accessible
- **Tool access**: Bobert must have Task tool access for agent delegation

No external tools required (instruction-only skill).

## Usage & Testing Guidance

### Invocation Pattern

**From Addison to Bobert (implicit pipeline)**:
```
"Research error handling patterns in React, then create documentation from your findings."
```

Bobert recognizes sequential pattern and constructs pipeline:
- Stage 1: deep-researcher investigates React error handling
- Stage 2: technical-breakdown-maintainer creates documentation

**From Addison (explicit pipeline)**:
```
"Use sequential pipeline: Explore agent finds all TODO patterns in the codebase, then Plan agent proposes standardization approach, then technical-breakdown-maintainer documents the standard."
```

Bobert constructs three-stage pipeline:
- Stage 1: Explore → TODO patterns in codebase
- Stage 2: Plan → Standardization approach
- Stage 3: technical-breakdown-maintainer → Documentation

### Bobert's Execution Steps

1. **Identify Pipeline Structure**:
   ```
   Analyze Addison's request for sequential stages
   Identify stage goals, agents, and dependencies
   Define output formats and validation criteria
   ```

2. **Validate Pipeline Definition**:
   ```
   Check stage count (2-5 stages)
   Verify agent availability
   Confirm output passing is feasible
   Ensure validation criteria are explicit
   ```

3. **Execute Stage 1**:
   ```
   Delegate to stage 1 agent via Task tool
   Pass initialInput and contextRequirements
   Wait for completion
   Validate output against criteria
   ```

4. **Execute Stage 2**:
   ```
   Construct prompt with Stage 1 output
   Delegate to stage 2 agent via Task tool
   Wait for completion
   Validate output against criteria
   ```

5. **Repeat for Remaining Stages**: Continue sequential execution with validation between each stage

6. **Report Final Output**: Deliver last stage's output to Addison with pipeline summary

### When to Consult Addison

Bobert should consult Addison (not proceed autonomously) when:
- **Unclear Pipeline Structure**: Cannot identify clear stages from Addison's request
- **Agent Selection Ambiguity**: Multiple agents could perform a stage, choice impacts quality
- **Validation Failure**: Stage output fails validation and Bobert cannot determine how to fix
- **Repeated Stage Failures**: Stage fails 2+ times even after retry/adjustment
- **Output Format Incompatibility**: Stage produces output that next stage cannot consume

Bobert should proceed autonomously when:
- **Pipeline Structure is Clear**: Stages, agents, and dependencies are evident from request
- **Agents are Obvious**: Stage goals clearly match agent capabilities
- **First Failure**: Retry with adjusted prompt before escalating
- **Minor Validation Misses**: Small adjustments can bring output into compliance

### Common Pipeline Patterns

**Research → Document**:
- Stage 1: deep-researcher produces Learning Packet
- Stage 2: technical-breakdown-maintainer creates documentation
- Use case: Documenting external best practices

**Explore → Analyze → Plan**:
- Stage 1: Explore agent discovers codebase patterns
- Stage 2: Plan agent analyzes findings and proposes approach
- Stage 3: implementation-plan-maintainer creates executable specification
- Use case: Understanding before designing

**Research → Design → Document**:
- Stage 1: deep-researcher gathers external knowledge
- Stage 2: adr-maintainer records architecture decisions
- Stage 3: technical-breakdown-maintainer synthesizes into guide
- Use case: Informed architecture documentation

**Implement → Review → Document**:
- Stage 1: code-monkey implements feature from specification
- Stage 2: code-review evaluates implementation quality
- Stage 3: technical-breakdown-maintainer documents how it works
- Use case: Quality-gated implementation

### Testing the Workflow

Since this is an orchestration playbook (not executable code), testing involves:

1. **Two-Stage Test**: Simple linear transformation
   ```
   "Research React hooks best practices, then document them."
   ```
   Expected: deep-researcher → Learning Packet, technical-breakdown-maintainer → Documentation

2. **Three-Stage Test**: Analysis in the middle
   ```
   "Explore our auth code, analyze what needs improvement, then create a plan."
   ```
   Expected: Explore → patterns, Plan → analysis, implementation-plan-maintainer → spec

3. **Validation Failure Test**: Stage produces incomplete output
   Expected: Bobert detects validation failure, retries with clarified prompt

4. **Handoff Test**: Verify output from one stage properly reaches next stage
   Expected: Stage 2 prompt explicitly includes Stage 1 output

### Installation

This skill is installed as part of the Claude Code configuration:

```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/sequential-pipeline/
```

After rebuild, Bobert automatically has access to this skill via the skills frontmatter in `bobert.md`.

## Related Skills and Agents

**Skills**:
- `full-lifecycle-delivery`: Use when work requires multi-phase coordination with iterative loops
- Agent teams: Use when stages can run in parallel rather than sequentially

**Agents**:
- `deep-researcher`: Stage 1 research for external knowledge
- `Explore`: Stage 1 codebase investigation
- `Plan`: Middle-stage analysis and decision-making
- `code-monkey`: Implementation stages with clear specifications
- `technical-breakdown-maintainer`: Documentation and synthesis stages
- `adr-maintainer`: Architecture decision documentation stages
- `implementation-plan-maintainer`: Specification creation stages

**Memory Nodes**:
- Prompt Chaining pattern from Anthropic: Sequential steps where each agent processes previous output
- Task Group A: Related but more complex (multi-phase with loops vs linear sequential)

## Pattern Consistency Notes

This skill follows the instruction-only pattern like `full-lifecycle-delivery` and `todo-writer` - it's loaded into Bobert's context to provide guidance rather than executed as a script. The 6-section structure ensures consistency with all other skills in the ecosystem.

Key patterns maintained:
- YAML frontmatter with name, description, allowed-tools
- Six standard sections (Purpose, Input, Output, Architecture, Dependencies, Usage)
- Clear scope boundaries and when-to-use guidance
- Explicit contracts even for instruction-only skills
- Examples and testing guidance for validation
- Comparison to related patterns (when to use this vs alternatives)
