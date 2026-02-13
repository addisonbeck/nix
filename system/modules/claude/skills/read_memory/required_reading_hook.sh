#!/usr/bin/env bash
# PostToolUse hook for Required Reading automation
# Triggers when read_memory skill completes, parsing Required Reading section
# and outputting forceful instructions for Claude to load dependencies

set -euo pipefail

ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Parse hook input JSON from stdin
# Expected format: {"tool_name": "Bash", "tool_input": {...}, "tool_response": {...}}
hook_input=$(cat)

# Check if this is a read_memory.sh invocation
command=$(echo "$hook_input" | jq -r '.tool_input.command // empty')
if [[ ! "$command" =~ read_memory\.sh ]]; then
  # Not a read_memory invocation, exit silently
  exit 0
fi

# Extract the JSON output from the Bash tool response
# The Bash tool returns stdout as a string, which contains our JSON
bash_output=$(echo "$hook_input" | jq -r '.tool_response.stdout // empty')

# Parse the JSON from bash output to get the actual content
content=$(echo "$bash_output" | jq -r '.content // empty')

# If no content or content is empty, pass through silently
if [ -z "$content" ]; then
  exit 0
fi

# Extract Required Reading section from org content
# Finds text between "* Required Reading" and next top-level heading
extract_required_reading() {
  local content="$1"
  
  echo "$content" | awk '
    BEGIN { in_section = 0 }
    /^\* Required Reading/ { in_section = 1; next }
    /^\* [^*]/ && in_section { exit }
    in_section { print }
  '
}

# Parse org-mode links from section: [[type:reference][description]]
parse_org_links() {
  local section="$1"
  
  # Use perl for more reliable regex parsing of org-mode links
  echo "$section" | perl -ne '
    while (/\[\[([^\]]+)\](?:\[([^\]]*)\])?\]/g) {
      my $type_ref = $1;
      my $desc = $2 // "";
      
      # Split type:reference
      my ($link_type, $reference);
      if ($type_ref =~ /^([^:]+):(.+)$/) {
        $link_type = $1;
        $reference = $2;
      } else {
        $link_type = "file";
        $reference = $type_ref;
      }
      
      # Escape for JSON (simple approach for common cases)
      $reference =~ s/"/\\"/g;
      $desc =~ s/"/\\"/g;
      
      print "{\"type\":\"$link_type\",\"reference\":\"$reference\",\"description\":\"$desc\"}\n";
    }
  '
}

# Categorize links by type
categorize_links() {
  local links_json="$1"
  
  declare -a org_roam_ids=()
  declare -a file_paths=()
  declare -a jira_tickets=()
  
  # Process each link JSON object
  while IFS= read -r link; do
    # Skip empty lines
    [ -z "$link" ] && continue
    
    link_type=$(echo "$link" | jq -r '.type')
    reference=$(echo "$link" | jq -r '.reference')
    
    # Skip empty references
    [ -z "$reference" ] && continue
    
    case "$link_type" in
      "id")
        org_roam_ids+=("$reference")
        ;;
      "file")
        # Expand ~ to $HOME in file paths
        expanded_path="${reference/#\~/$HOME}"
        file_paths+=("$expanded_path")
        ;;
      "jira")
        jira_tickets+=("$reference")
        ;;
      *)
        # Unknown link type - log warning to stderr and skip
        echo "Warning: Unknown link type '$link_type' with reference '$reference'" >&2
        ;;
    esac
  done <<< "$links_json"
  
  # Return categorized arrays as JSON
  jq -n \
    --argjson ids "$(printf '%s\n' "${org_roam_ids[@]}" | jq -R . | jq -s .)" \
    --argjson files "$(printf '%s\n' "${file_paths[@]}" | jq -R . | jq -s .)" \
    --argjson jiras "$(printf '%s\n' "${jira_tickets[@]}" | jq -R . | jq -s .)" \
    '{org_roam_ids: $ids, file_paths: $files, jira_tickets: $jiras}'
}

# Build forceful instruction text for Claude
build_instruction() {
  local categorized_json="$1"
  
  # Extract arrays from JSON
  readarray -t org_roam_ids < <(echo "$categorized_json" | jq -r '.org_roam_ids[]')
  readarray -t file_paths < <(echo "$categorized_json" | jq -r '.file_paths[]')
  readarray -t jira_tickets < <(echo "$categorized_json" | jq -r '.jira_tickets[]')
  
  # Count total items
  total_items=$((${#org_roam_ids[@]} + ${#file_paths[@]} + ${#jira_tickets[@]}))
  
  # If no Required Reading items found, exit silently
  if [ "$total_items" -eq 0 ]; then
    exit 0
  fi
  
  # Output forceful instruction header
  cat <<'EOF'
===========================================
⚠️  CRITICAL: REQUIRED READING DETECTED ⚠️
===========================================

The memory node you just read contains REQUIRED READING that MUST be loaded
before proceeding with any task or response.

MANDATORY NEXT STEPS (complete IN ORDER before responding):

EOF
  
  # Step 1: org-roam memory nodes
  if [ ${#org_roam_ids[@]} -gt 0 ]; then
    echo "STEP 1 - Load org-roam memory nodes (REQUIRED, NOT OPTIONAL):"
    for uuid in "${org_roam_ids[@]}"; do
      echo "  → /read_memory $uuid"
    done
    echo ""
  fi
  
  # Step 2: file links
  if [ ${#file_paths[@]} -gt 0 ]; then
    echo "STEP 2 - Read referenced files (REQUIRED, NOT OPTIONAL):"
    for path in "${file_paths[@]}"; do
      echo "  → Read file $path"
    done
    echo ""
  fi
  
  # Step 3: Jira tickets via MCP
  if [ ${#jira_tickets[@]} -gt 0 ]; then
    echo "STEP 3 - Load Jira tickets via MCP (REQUIRED, NOT OPTIONAL):"
    for ticket in "${jira_tickets[@]}"; do
      echo "  → Call getJiraIssue MCP tool with cloudId='bitwarden.atlassian.net' and issueIdOrKey='$ticket'"
    done
    echo ""
  fi
  
  # Output forceful footer with commitment mechanism
  cat <<'EOF'
⚠️  DO NOT proceed with the task until ALL Required Reading is loaded.
⚠️  Each /read_memory invocation is MANDATORY.
⚠️  Before invoking /read_memory, check if this UUID was already loaded in this session.
⚠️  If a UUID appears in earlier context, skip it to prevent cycles.
⚠️  Skipping Required Reading will result in incomplete context and task failure.

AFTER loading all Required Reading, you may proceed with the task.
===========================================
EOF
}

# Main processing pipeline
required_reading_section=$(extract_required_reading "$content")

# If no Required Reading section found, exit silently
if [ -z "$required_reading_section" ]; then
  exit 0
fi

# Parse links from section
links_json=$(parse_org_links "$required_reading_section")

# If no links found, exit silently
if [ -z "$links_json" ]; then
  exit 0
fi

# Categorize links by type
categorized=$(categorize_links "$links_json")

# Build and output forceful instruction
build_instruction "$categorized"
