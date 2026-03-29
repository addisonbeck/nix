#!/usr/bin/env bash
# UserPromptSubmit hook that reminds Claude to prioritize specialized agents
# Outputs blocking message encouraging agent-first workflow

set -euo pipefail

# Build the reminder message
message=""
message+="===========================================\n"
message+="📋 AGENT-FIRST WORKFLOW REMINDER\n"
message+="===========================================\n"
message+="\n"
message+="Before proceeding with this request, consider:\n"
message+="\n"
message+="1. **Is there an existing specialized agent for this work?**\n"
message+="   - If yes, delegate to that agent using the Task tool\n"
message+="   - Agents provide optimized, reliable, specialized capabilities\n"
message+="\n"
message+="2. **If no obvious agent exists, should we create one?**\n"
message+="   - Tasks that are specialized, recurring, or complex benefit from dedicated agents\n"
message+="   - Agents should be atomic - focused on single task types\n"
message+="   - Creating agents improves future workflow efficiency\n"
message+="\n"
message+="3. **When to suggest agent creation:**\n"
message+="   - The work requires specialized domain knowledge\n"
message+="   - Similar tasks will likely recur\n"
message+="   - The task has clear boundaries and scope\n"
message+="   - Specialized tool access would benefit the work\n"
message+="\n"
message+="**If uncertain whether an appropriate agent exists or should be created:**\n"
message+="- Briefly acknowledge this reminder in your response\n"
message+="- Share your assessment: existing agent, new agent needed, or general approach\n"
message+="- For new agents: suggest using bootstrap-agent to create one\n"
message+="- Await Addison's confirmation before proceeding\n"
message+="\n"
message+="**If an obvious agent or approach exists:**\n"
message+="- Proceed directly without additional discussion\n"
message+="- This reminder is automatic and can be safely ignored when appropriate\n"
message+="\n"
message+="===========================================\n"

# Output the message directly to stdout (no JSON needed)
# UserPromptSubmit hooks add stdout content to Claude's context
echo -e "$message"
