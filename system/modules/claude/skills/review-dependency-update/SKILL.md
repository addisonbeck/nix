---
name: review-dependency-update
description: |
  Orchestration playbook for reviewing dependency update PRs (Renovate, Dependabot, manual). Guides Bobert through PR retrieval, usage analysis, version research, and synthesis to produce actionable recommendations (APPROVE/REQUEST_CHANGES/COMMENT). Use for any dependency PR requiring thorough review.
---

# review-dependency-update Skill

This skill provides orchestration guidance for **dependency update PR reviews**, a pattern where Bobert systematically analyzes proposed dependency changes by retrieving PR details, analyzing codebase usage patterns, researching version changes with deep-researcher, and synthesizing findings into actionable recommendations.

When invoked, Bobert follows this instruction playbook to:
1. Retrieve PR metadata and diff to identify package changes
2. Delegate usage analysis to Explore (built-in) for pattern discovery
3. Delegate version research to deep-researcher for Learning Packet creation
4. Synthesize findings into risk assessment and recommendation
5. Provide testing guidance and confidence-qualified decision

## Purpose & When to Use

Use review-dependency-update when:
- **Dependency PR Review**: Renovate, Dependabot, or manual dependency update PRs require thorough analysis before approval
- **Version Change Assessment**: Need to understand breaking changes, new features, security implications between versions
- **Usage Impact Analysis**: Must identify how package changes affect the codebase (API usage, integration points, configuration)
- **Informed Decision Making**: Requires evidence-based recommendation (APPROVE/REQUEST_CHANGES/COMMENT) with confidence level
- **Knowledge Archival**: Major version updates or significant changes benefit from Learning Packet preservation

Do NOT use review-dependency-update when:
- **Trivial Patches**: Patch version updates (1.2.3 → 1.2.4) with no known breaking changes may not justify deep analysis
- **Non-Dependency PRs**: PR does not modify dependency versions (use standard PR review workflow)
- **Quick Approval Needed**: Time constraints prevent multi-hour research (accept reduced confidence or delegate to human reviewer)
- **Rollback PRs**: Reverting a dependency change (use git history analysis instead)

This pattern is ideal for major/minor version updates where understanding compatibility impact and new capabilities requires systematic investigation.

## Input Contract

review-dependency-update expects Bobert to identify from Addison's request:

- **pr_number** (integer, required): GitHub PR number to review
- **repository_context** (string, optional): Path to repository [default: current working directory from git context]
- **create_learning_packet** (string, optional): When to create Learning Packet
  - `"always"`: Create for every review (maximum archival) (default)
  - `"major"`: Only for major version updates (X.0.0 changes)
  - `"significant"`: When breaking changes or security issues are found
  - `"never"`: Skip Learning Packet creation (usage summary and recommendation only)
- **exploration_thoroughness** (string, optional): Depth of codebase usage analysis
  - `"basic"`: Import statements and direct usage only
  - `"thorough"`: Include configuration, initialization patterns, integration points (default)
  - `"very thorough"`: Exhaustive analysis including transitive usage and edge cases
- **package_manager** (string, optional): Infer from PR diff or specify explicitly
  - `"npm"`: JavaScript/TypeScript (package.json, package-lock.json)
  - `"pip"`: Python (requirements.txt, poetry.lock, Pipfile.lock)
  - `"cargo"`: Rust (Cargo.toml, Cargo.lock)
  - `"maven"`: Java (pom.xml)
  - `"auto"`: Detect from PR diff (default)

### Validation Rules
- PR must exist and be accessible via `gh pr view <pr_number>`
- PR diff must contain dependency file changes (package.json, Cargo.toml, etc.)
- At least one version change must be identifiable from diff
- Repository context must be a valid git repository with GitHub remote

### Example Invocation (from Addison to Bobert)
```
"Review Renovate PR #16054 - looks like a major version update for @microsoft/signalr"
```

Bobert constructs the skill parameters:
```
{
  "pr_number": 16054,
  "repository_context": "." (inferred from cwd),
  "create_learning_packet": "significant",
  "exploration_thoroughness": "thorough",
  "package_manager": "auto"
}
```

## Output Contract

review-dependency-update produces a structured review recommendation delivered to Addison:

### Success Response Components

**1. PR Metadata Summary**
- PR number, title, author
- Package names and version changes (from → to)
- Release notes excerpt from PR body (if available)

**2. Usage Analysis**
- Import patterns and locations (files, line numbers)
- API surface usage (which methods/classes are called)
- Configuration and initialization patterns
- Integration points with other packages
- Transitive dependencies affected

**3. Risk Assessment**
- Breaking changes identified (API changes, behavior changes, removed features)
- Compatibility concerns (TypeScript definitions, platform support, protocol changes)
- Migration complexity (what code needs updating)
- Confidence level: High/Moderate/Low/Very Low with rationale

**4. Security Implications**
- CVEs addressed by the update
- New vulnerabilities introduced (if any)
- Security features added/removed
- Dependency vulnerability chain changes

**5. New Features & Opportunities**
- New capabilities available (opt-in features, performance improvements)
- Deprecation warnings for current usage
- Recommended migration paths to leverage new APIs

**6. Recommendation**
- **Decision**: APPROVE | REQUEST_CHANGES | COMMENT
- **Confidence Level**: High/Moderate/Low/Very Low
- **Rationale**: Evidence-based justification for decision
- **Conditions**: Prerequisites for approval (tests pass, migration completed, etc.)

**7. Testing Approach**
- Specific test areas to validate (functionality using updated package)
- Edge cases to verify (error handling, configuration, integration points)
- Regression test guidance (existing features that might break)
- Performance validation (if relevant)

**8. Learning Packet Reference** (if created)
- UUID of Learning Packet memory node
- Key findings summary
- Research confidence overall

### Error Conditions

**PR Not Found**:
```
Error: PR #<pr_number> not found. Verify PR number and repository context.
```

**Not a Dependency PR**:
```
Error: PR #<pr_number> does not contain dependency updates. Diff shows no package file changes.
```

**Insufficient Information**:
```
Warning: Could not determine version changes from PR diff. Manual review required.
Recommendation: COMMENT with request for clarification.
```

**Research Gaps Prevent Recommendation**:
```
Warning: deep-researcher reported Low confidence due to [research gaps].
Recommendation: REQUEST_CHANGES with note about information needs.
```

When errors occur, Bobert decides:
- **Abort**: Not a dependency PR or fundamentally incompatible request
- **Adapt**: Partial information available, proceed with caveats
- **Escalate**: Consult Addison for guidance on how to proceed

## Implementation Architecture

This is an **instruction-only skill** - no bash script implementation. Bobert loads this skill into context and follows the orchestration guidance.

### Four-Stage Workflow Pattern

```
                DEPENDENCY UPDATE REVIEW WORKFLOW

                    +------------------+
                    | Stage 1:         |
                    | PR Retrieval     |
                    | (Bobert: gh CLI) |
                    +------------------+
                            |
                            v
                  [PR metadata extracted]
                  [Version changes identified]
                  [Release notes captured]
                            |
                            v
                    +------------------+
                    | Stage 2:         |
                    | Usage Analysis   |
                    | (Explore tool)   |
                    +------------------+
                            |
                            v
                  [Import patterns documented]
                  [API surface mapped]
                  [Integration points found]
                            |
       +--------------------+--------------------+
       |                                         |
       v                                         v
+------------------+                  +------------------+
| Stage 3:         |                  | Stage 3:         |
| Version Research |    (Parallel)    | Codebase Context |
| (deep-researcher)|                  | (For researcher) |
+------------------+                  +------------------+
       |                                         |
       v                                         v
[Learning Packet created]            [Usage patterns provided]
[Breaking changes documented]        [API surface shared]
[Confidence assessed]                [Integration context given]
       |                                         |
       +--------------------+--------------------+
                            |
                            v
                    +------------------+
                    | Stage 4:         |
                    | Synthesis        |
                    | (Bobert)         |
                    +------------------+
                            |
                            v
                  [Risk assessment]
                  [Recommendation: APPROVE/CHANGES/COMMENT]
                  [Testing approach]
                  [Confidence level]
                            |
                            v
                 [Report to Addison]
```

### Stage 1: PR Retrieval (Bobert)

**Objective**: Extract PR metadata, version changes, and release notes.

**Commands**:
```bash
# Get PR metadata
gh pr view <pr_number> --json number,title,body,author,url

# Get PR diff
gh pr diff <pr_number>
```

**Parsing Strategy**:
- Identify package files changed (package.json, package-lock.json, Cargo.toml, etc.)
- Extract version changes using pattern matching:
  - npm: `"@package/name": "1.0.0"` → `"@package/name": "2.0.0"`
  - cargo: `package = "1.0"` → `package = "2.0"`
  - pip: `package==1.0.0` → `package==2.0.0`
- Capture release notes from PR body (Renovate/Dependabot include excerpts)
- Determine semantic version change type (major, minor, patch)

**Validation**:
- At least one version change identified
- Package manager inferred or specified
- PR body contains release notes or changelog links

**Output**: Structured summary with package names, from/to versions, and release notes excerpt.

### Stage 2: Usage Analysis (Explore Tool)

**Objective**: Discover how packages are used in the codebase.

**Delegation Prompt Template**:
```
Find all usage patterns for [package-name] in this codebase.

Focus areas:
1. Import statements and locations (which files import this package)
2. API surface usage (which methods, classes, types are used)
3. Configuration and initialization patterns
4. Integration points with other packages or services
5. Dependency on specific package APIs or features

Thoroughness level: [exploration_thoroughness]

Provide code snippets showing actual usage patterns.
```

**Explore Tool Usage**:
- Bobert uses the built-in Explore tool (not an agent) for codebase investigation
- Explore returns structured findings with file locations and code snippets
- Bobert captures the Explore output for synthesis and researcher context

**Validation**:
- Import patterns found (or explicit note if package is unused)
- Usage locations documented with file paths
- API surface mapped (methods/classes called)

**Output**: Comprehensive map of where/how packages are used with code examples.

### Stage 3: Version Research (deep-researcher)

**Objective**: Investigate version changes producing a Learning Packet.

**Delegation Prompt Template**:
```
Research version change impact for [package-name] from v[old] to v[new].

Context from PR:
[Release notes excerpt from Renovate/Dependabot PR body]

Context from codebase usage analysis:
[Usage patterns from Stage 2]

Investigation areas (per GRADE framework):
1. Breaking Changes: API/behavior/pattern changes requiring code updates
2. New Features: Improvements or new capabilities available (opt-in opportunities)
3. Security Implications: CVEs addressed, vulnerabilities introduced, security features
4. Dependency Compatibility: Interactions with other packages, known incompatibilities
5. Migration Path: What's required to upgrade? Interim version considerations?
6. Type/Platform Support: TypeScript definitions, browser/Node.js compatibility changes
7. Protocol/Integration Changes: For libraries with protocols (like SignalR's MessagePack)

Produce a Learning Packet with GRADE confidence assessments for each finding.

Package manager: [package_manager]
Version jump type: [major|minor|patch]
```

**deep-researcher Execution**:
- Conducts systematic research using WebSearch and WebFetch
- Consults official documentation, changelogs, release notes, issue trackers
- Cross-references multiple sources for confidence assessment
- Creates Learning Packet memory node via create_memory skill
- Returns structured JSON metadata with UUID, findings, confidence levels

**Validation**:
- Learning Packet created (UUID returned)
- Breaking changes section complete (or noted as none found)
- Confidence levels assigned per GRADE framework
- Research gaps documented

**Output**: Learning Packet UUID and structured metadata with key findings.

### Stage 4: Synthesis (Bobert)

**Objective**: Integrate all findings into actionable recommendation.

**Synthesis Process**:
1. **Load Context**: Review usage analysis (Stage 2) and Learning Packet (Stage 3)
2. **Assess Risk**: Map breaking changes against actual codebase usage
   - High Risk: Breaking changes affect APIs we use extensively
   - Moderate Risk: Breaking changes in areas we use lightly or have workarounds
   - Low Risk: Breaking changes in unused features, or no breaking changes
3. **Evaluate Security**: Prioritize CVE fixes and security improvements
4. **Identify Opportunities**: Note new features relevant to current usage patterns
5. **Formulate Recommendation**:
   - APPROVE: No breaking changes affecting our usage, or all changes are improvements
     - Confidence: High (well-researched, clear compatibility)
   - REQUEST_CHANGES: Breaking changes require migration, insufficient information, or security concerns
     - Confidence: Moderate/High (evidence-based concerns)
   - COMMENT: Partial information, questions for PR author, or optional improvements noted
     - Confidence: Low/Moderate (research gaps or ambiguity)
6. **Define Testing**: Specific validation steps based on usage patterns and changes

**Output Template**:
```markdown
## Dependency Update Review: PR #<pr_number>

### Summary
- **Package**: [package-name]
- **Version Change**: v[old] → v[new] ([major|minor|patch] update)
- **PR Author**: [author]
- **PR URL**: [url]

### Usage in Codebase
[Summary from Stage 2: where used, how used, API surface]

### Version Change Analysis
[Summary from Stage 3 Learning Packet: breaking changes, new features, security]

### Risk Assessment
- **Breaking Changes**: [description and impact on our usage]
- **Compatibility Concerns**: [any platform/dependency issues]
- **Migration Complexity**: [what needs updating]
- **Confidence Level**: [High|Moderate|Low|Very Low]
- **Rationale**: [why this confidence level]

### Security Implications
- **CVEs Addressed**: [list or "None identified"]
- **New Vulnerabilities**: [list or "None identified"]
- **Security Features**: [improvements or changes]

### New Features & Opportunities
- [New capabilities worth considering]
- [Opt-in features available]
- [Recommended migration paths]

### Recommendation
**Decision**: [APPROVE|REQUEST_CHANGES|COMMENT]
**Confidence**: [High|Moderate|Low|Very Low]

**Rationale**: [Evidence-based justification]

**Conditions** (if applicable):
- [ ] Tests pass (especially [specific test areas])
- [ ] Migration completed for [breaking change areas]
- [ ] Performance validated for [specific scenarios]

### Testing Approach
- **Functionality**: [specific features using updated package]
- **Edge Cases**: [error handling, configuration, integration points]
- **Regression**: [existing features that might break]
- **Performance**: [if relevant based on changes]

### References
- Learning Packet: [[id:<uuid>][<title>]]
- PR: <pr_url>
- Release Notes: [links from research]
```

**Validation**:
- All synthesis sections complete
- Recommendation has clear rationale
- Testing approach is specific and actionable
- Confidence level is justified

**Output**: Complete review report delivered to Addison.

## Environment Dependencies

- **gh CLI**: GitHub CLI with authentication configured
  - Commands: `gh pr view`, `gh pr diff`
  - Auth: `gh auth status` must show authenticated state
- **Git repository context**: Must be run from within a git repository with GitHub remote
- **Agent availability**:
  - `deep-researcher`: For version research and Learning Packet creation
  - Built-in Explore tool: For codebase usage analysis
- **Skill access**:
  - `read_memory`: For loading Learning Packet after creation
  - `create_memory`: Used by deep-researcher for Learning Packet persistence
- **Bobert tools**: Bash (read-only), Task (delegation), Skill (memory access)

No external dependencies for the skill itself (instruction-only).

## Usage & Testing Guidance

### Invocation Pattern

**From Addison (simple)**:
```
"Review PR #16054"
```

**From Addison (explicit)**:
```
"Review dependency update PR #16054 with very thorough exploration"
```

**From Addison (with parameters)**:
```
"Review PR #16054, create Learning Packet always, use basic exploration"
```

### Bobert's Execution Steps

#### 1. Load Skill and Parse Request
```
Bobert analyzes Addison's request:
- PR number: 16054
- Repository: current working directory (bitwarden-clients)
- Learning Packet: "significant" (default)
- Exploration: "thorough" (default)
- Package manager: "auto" (infer from diff)

Bobert loads review-dependency-update skill into context.
```

#### 2. Execute Stage 1: PR Retrieval
```bash
# Bobert runs (read-only Bash allowed)
gh pr view 16054 --json number,title,body,author,url
gh pr diff 16054
```

**Parse output**:
```
PR #16054: Update @microsoft/signalr to v10.0.0
Author: renovate[bot]
Package: @microsoft/signalr
Version change: 8.0.17 → 10.0.0 (major update)
Release notes: [excerpt from PR body]
```

**Validation**: ✅ Version change identified, major update detected

#### 3. Execute Stage 2: Usage Analysis
```
Bobert delegates to Explore (built-in):

"Find all usage patterns for @microsoft/signalr in this codebase.

Focus areas:
1. Import statements and locations
2. API surface usage (methods, classes, types)
3. Configuration and initialization patterns
4. Integration points with other packages
5. Dependency on specific SignalR APIs or features

Thoroughness level: thorough

Provide code snippets showing actual usage patterns."
```

**Explore returns**:
```
Import patterns found in:
- apps/web/src/services/hub-connection.ts
- libs/common/src/platform/services/signalr.service.ts

API usage:
- HubConnectionBuilder (initialization)
- connection.start(), connection.stop() (lifecycle)
- connection.on('method', handler) (event handling)
- withAutomaticReconnect() (configuration)

Configuration:
- Uses MessagePack protocol via @microsoft/signalr-protocol-msgpack
- Automatic reconnection enabled
- Access token provider for authentication

[Code snippets provided]
```

**Validation**: ✅ Import patterns found, API surface mapped, integration points documented

#### 4. Execute Stage 3: Version Research
```
Bobert delegates to deep-researcher:

"Research version change impact for @microsoft/signalr from v8.0.17 to v10.0.0.

Context from PR:
[Release notes excerpt showing MessagePack protocol changes, reconnect improvements]

Context from codebase usage analysis:
We use:
- HubConnectionBuilder for initialization
- connection.start/stop for lifecycle
- connection.on for event handling
- withAutomaticReconnect for resilience
- MessagePack protocol via @microsoft/signalr-protocol-msgpack

Investigation areas (per GRADE framework):
1. Breaking Changes: API/behavior/pattern changes requiring code updates
2. New Features: Improvements or new capabilities
3. Security Implications: CVEs, vulnerabilities, security features
4. Dependency Compatibility: @microsoft/signalr-protocol-msgpack compatibility
5. Migration Path: Steps required for v8 → v10
6. Type Support: TypeScript definitions changes
7. Protocol Changes: MessagePack protocol evolution

Produce a Learning Packet with GRADE confidence assessments.

Package manager: npm
Version jump type: major"
```

**deep-researcher execution**:
- Searches for "@microsoft/signalr v10 breaking changes", "signalr migration guide v8 to v10"
- Consults official Microsoft documentation, GitHub release notes, changelog
- Investigates MessagePack protocol compatibility
- Assesses confidence using GRADE framework
- Creates Learning Packet: `[[id:ABC123][SignalR v8→v10 Migration Analysis]]`
- Returns metadata:
  ```json
  {
    "uuid": "ABC123",
    "title": "SignalR v8→v10 Migration Analysis",
    "confidence_overall": "High",
    "breaking_changes": [
      "MessagePack protocol requires @microsoft/signalr-protocol-msgpack v9+"
    ],
    "new_features": [
      "Improved automatic reconnection with exponential backoff",
      "Better TypeScript type definitions"
    ],
    "security_implications": "No CVEs in v8, v10 includes security hardening",
    "research_gaps": ["Long-term connection stability in production environments"]
  }
  ```

**Validation**: ✅ Learning Packet created, breaking changes documented, confidence assessed

#### 5. Execute Stage 4: Synthesis
```
Bobert integrates findings:

Usage Analysis: We use HubConnectionBuilder, lifecycle methods, event handling, MessagePack
Version Research: Breaking change requires upgrading @microsoft/signalr-protocol-msgpack
Security: No critical issues, v10 adds security hardening
New Features: Better reconnection, improved TypeScript types

Risk Assessment:
- Breaking Change: MessagePack dependency must be upgraded simultaneously
- Impact: Moderate (single dependency upgrade required)
- Migration: Check package.json for msgpack version, update if needed
- Confidence: High (clear migration path, well-documented changes)

Recommendation:
- Decision: REQUEST_CHANGES
- Rationale: PR updates @microsoft/signalr but may not update @microsoft/signalr-protocol-msgpack to compatible version. Need to verify msgpack upgrade is included.
- Condition: Confirm @microsoft/signalr-protocol-msgpack ≥ v9.0.0 in package.json
```

**Bobert reports to Addison**:
```markdown
## Dependency Update Review: PR #16054

[Complete synthesis report as per Output Contract template]

**Recommendation**: REQUEST_CHANGES
**Confidence**: High

**Rationale**: The PR updates @microsoft/signalr from v8.0.17 to v10.0.0 (major version), which requires @microsoft/signalr-protocol-msgpack v9+ for compatibility. We use MessagePack protocol in our SignalR connections. Need to verify the msgpack package is upgraded in this PR or add it.

**Conditions**:
- [ ] Confirm @microsoft/signalr-protocol-msgpack ≥ v9.0.0 in package.json
- [ ] Tests pass for SignalR connectivity (hub-connection.ts, signalr.service.ts)
- [ ] Validate automatic reconnection still works as expected

**Testing Approach**:
- Functionality: SignalR connection establishment, message sending/receiving
- Edge Cases: Network interruptions (reconnection behavior), authentication token refresh
- Regression: Existing SignalR integrations in web and common libraries
```

**Validation**: ✅ All sections complete, recommendation justified, testing specific

### When to Consult Addison

Bobert should consult Addison (not proceed autonomously) when:
- **Ambiguous PR Identification**: PR number unclear or multiple PRs mentioned
- **Non-Dependency PR**: Diff shows no package changes (abort vs force analysis?)
- **Multiple Unrelated Updates**: PR contains 10+ package updates spanning different domains (review all vs focus?)
- **Research Confidence Very Low**: deep-researcher cannot assess impact due to missing documentation
- **Security Critical**: CVEs are present and decision has safety implications

Bobert should proceed autonomously when:
- **Clear PR Number**: Single PR specified with dependency changes
- **Standard Workflow**: Stages complete successfully with moderate+ confidence
- **Straightforward Recommendation**: Evidence clearly supports APPROVE or REQUEST_CHANGES
- **Minor Gaps**: Research has small gaps but overall picture is clear

### Common Review Patterns

**Pattern 1: Clean Patch Update**
- PR: React 18.2.0 → 18.2.1
- Usage: Extensive throughout codebase
- Research: Bug fixes only, no breaking changes
- Recommendation: APPROVE (High confidence)
- Learning Packet: Skip (patch update, no significant changes)

**Pattern 2: Major Update with Breaking Changes**
- PR: @microsoft/signalr 8.0.17 → 10.0.0
- Usage: MessagePack protocol, reconnection
- Research: Breaking change in protocol compatibility
- Recommendation: REQUEST_CHANGES (High confidence - need msgpack upgrade)
- Learning Packet: Create (major update with migration requirements)

**Pattern 3: Security Update with New Features**
- PR: axios 0.21.1 → 1.6.0
- Usage: HTTP client throughout
- Research: CVE-2021-3749 fixed, new API for request cancellation
- Recommendation: APPROVE with note about new features (High confidence)
- Learning Packet: Create (security implications + new capabilities)

**Pattern 4: Unused Transitive Dependency**
- PR: lodash 4.17.19 → 4.17.21
- Usage: Not directly imported (transitive dependency)
- Research: Security fixes in lodash
- Recommendation: APPROVE (Moderate confidence - indirect impact)
- Learning Packet: Skip (no direct usage, routine security patch)

**Pattern 5: Insufficient Information**
- PR: obscure-package 1.0.0 → 2.0.0
- Usage: Single file, limited API surface
- Research: No documentation, no changelog, source code only
- Recommendation: COMMENT requesting PR author provide migration notes (Low confidence)
- Learning Packet: Create with research gaps documented

### Testing the Workflow

Since this is an orchestration playbook (not executable code), testing involves:

**1. Simple Patch Update Test**
```
"Review PR #12345" (where PR is a patch version update)
```
Expected:
- Stage 1: PR retrieved, patch update identified
- Stage 2: Usage analysis finds import patterns
- Stage 3: deep-researcher finds no breaking changes (Learning Packet skipped)
- Stage 4: APPROVE recommendation with High confidence

**2. Major Version Update Test**
```
"Review PR #16054 with very thorough exploration"
```
Expected:
- Stage 1: Major update identified (v8 → v10)
- Stage 2: Comprehensive usage map with code snippets
- Stage 3: Learning Packet created with breaking changes and GRADE confidence
- Stage 4: REQUEST_CHANGES or APPROVE based on findings

**3. Security Update Test**
```
"Review PR #23456" (where PR fixes CVEs)
```
Expected:
- Stage 1: Version change identified
- Stage 2: Usage analysis finds vulnerable code paths
- Stage 3: Learning Packet documents CVEs addressed
- Stage 4: APPROVE with priority note for security fixes

**4. Non-Dependency PR Test**
```
"Review PR #99999" (where PR is a feature, not dependency)
```
Expected:
- Stage 1: No package file changes detected
- Bobert aborts: "Error: PR #99999 does not contain dependency updates"

**5. Parallel Execution Validation**
```
Monitor deep-researcher receives usage context from Stage 2
```
Expected:
- Stage 2 completes: Usage patterns documented
- Stage 3 prompt includes: "Context from codebase usage analysis: [Stage 2 output]"
- Learning Packet references our specific API usage in analysis

### Installation

This skill is installed as part of the Claude Code configuration:

```bash
nix develop .#building --command rebuild <hostname>
ls ~/.claude/skills/review-dependency-update/
```

After rebuild, Bobert automatically has access to this skill.

### Activating the Skill

**From Addison**:
```
"Review dependency PR #16054"
```

**Bobert's Plan phase**:
```
Bobert will load the review-dependency-update skill to guide systematic PR review.
This skill orchestrates:
1. PR metadata retrieval via gh CLI
2. Codebase usage analysis via Explore tool
3. Version research via deep-researcher (Learning Packet creation)
4. Synthesis into recommendation with confidence assessment
```

**Bobert's Execute phase**:
Bobert follows the four-stage workflow defined in this skill, delegating to appropriate tools and agents, validating each stage, and synthesizing findings.

## Related Skills and Agents

**Skills**:
- `sequential-pipeline`: General-purpose sequential delegation pattern (this skill is a specialized instance)
- `full-lifecycle-delivery`: Complete implementation workflow (different domain)
- `create_memory`: Used by deep-researcher for Learning Packet persistence
- `read_memory`: Used by Bobert to access Learning Packet findings

**Agents**:
- `deep-researcher`: Stage 3 version research and Learning Packet creation
- Built-in Explore tool: Stage 2 codebase usage analysis
- `pr-maintainer`: Creates PRs (opposite direction - this reviews them)

**Memory Nodes**:
- Learning Packets created by deep-researcher (semantic memory for version migration knowledge)
- GRADE framework: Confidence assessment methodology
- Prompt Chaining: Sequential delegation pattern

## Pattern Consistency Notes

This skill follows the instruction-only orchestration pattern established by `sequential-pipeline` and `full-lifecycle-delivery`. It's a specialized workflow coordinator that Bobert loads and executes through delegation to specialized agents.

Key patterns maintained:
- YAML frontmatter with name, description
- Six standard sections (Purpose, Input, Output, Architecture, Dependencies, Usage)
- Clear scope boundaries and when-to-use guidance
- Explicit contracts for inputs and outputs
- Validation criteria at each stage
- Examples and testing guidance
- Comparison to related patterns
- Progressive disclosure (simple invocation, detailed guidance for edge cases)

**Differences from sequential-pipeline**:
- Specialized for dependency PR review (not general-purpose)
- Fixed four-stage workflow (not configurable pipeline)
- Parallel execution opportunity (Stage 2 and Stage 3 can run concurrently)
- Built-in decision logic (Learning Packet creation conditions)
- Domain-specific validation (package manager detection, version parsing)

This skill represents a reusable pattern that can be adapted for other specialized review workflows (security audit reviews, architecture change reviews, etc.).
