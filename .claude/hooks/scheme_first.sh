#!/bin/sh
# A Claude Code PreToolUse hook: when an edit adds a function to JavaScript
# under src/, remind the agent what may be JavaScript in this project.
#
# The reminder is the "What may be JavaScript" item of the rules
# (.agent/rules/rules.md, which CLAUDE.md and AGENTS.md link to), read from
# there so that the two cannot drift apart. It adds context and never blocks:
# much of the JavaScript under src/ is rightly JavaScript, and a block would
# only make the edit be retried.
#
# Shell rather than Scheme, because it runs before every edit and has to work
# while the Scheme implementation itself is half-changed: a hook that ran on
# the system being edited would fail exactly when its runtime is being worked
# on. It needs jq, and says so rather than failing if jq is missing.

root=$(cd "$(dirname "$0")/../.." && pwd)
rules="$root/.agent/rules/rules.md"

command -v jq >/dev/null 2>&1 || {
  echo '{"systemMessage": "scheme_first hook: jq was not found, so the Scheme-first reminder is off"}'
  exit 0
}

input=$(cat)
path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty') || exit 0

case "$path" in
  */node_modules/*) exit 0 ;;
  */src/*.js | */src/*.mjs) ;;
  *) exit 0 ;;
esac

# A generated file is written by its build script, not edited.
case $(head -n 1 "$path" 2>/dev/null) in
  *Auto-generated*) exit 0 ;;
esac

# The text before the edit and after it.
case $(printf '%s' "$input" | jq -r '.tool_name') in
  Write)
    before=$(cat "$path" 2>/dev/null)
    after=$(printf '%s' "$input" | jq -r '.tool_input.content // empty') ;;
  Edit)
    before=$(printf '%s' "$input" | jq -r '.tool_input.old_string // empty')
    after=$(printf '%s' "$input" | jq -r '.tool_input.new_string // empty') ;;
  MultiEdit)
    before=$(printf '%s' "$input" | jq -r '[.tool_input.edits[]?.old_string] | join("\n")')
    after=$(printf '%s' "$input" | jq -r '[.tool_input.edits[]?.new_string] | join("\n")') ;;
  *) exit 0 ;;
esac

# How many lines of some JavaScript define a function. Four shapes: a function
# declaration or named function expression; a name bound to a function or an
# arrow; an object entry whose value is one, which is how the host procedures
# of src/compiler/host.js are written; and a method -- a name, its parameters
# and a brace ending the line -- once control statements and comments are
# ruled out. A definition whose parameters span lines is missed, which only
# makes the reminder quieter.
definitions() {
  printf '%s\n' "$1" | grep -E \
    -e '(^|[^A-Za-z0-9_$.])function([[:space:]]*[*][[:space:]]*|[[:space:]]+)[A-Za-z_$][A-Za-z0-9_$]*[[:space:]]*[(]' \
    -e '(const|let|var)[[:space:]]+[A-Za-z_$][A-Za-z0-9_$]*[[:space:]]*=[[:space:]]*(async[[:space:]]+)?(function|[(][^)]*[)][[:space:]]*=>|[A-Za-z_$][A-Za-z0-9_$]*[[:space:]]*=>)' \
    -e '^[[:space:]]*[^[:space:]:(){},=]+[[:space:]]*:[[:space:]]*(async[[:space:]]+)?(function|[(][^)]*[)][[:space:]]*=>|[A-Za-z_$][A-Za-z0-9_$]*[[:space:]]*=>)' \
    -e '^[[:space:]]*((static|async|get|set)[[:space:]]+)*[A-Za-z_$][A-Za-z0-9_$]*[[:space:]]*[(][^)]*[)][[:space:]]*[{][[:space:]]*$' \
  | grep -cvE '^[[:space:]]*(//|/?[*]|(if|for|while|switch|catch|with|return)[[:space:]]*[(])'
}

added=$(( $(definitions "$after") - $(definitions "$before") ))
[ "$added" -gt 0 ] || exit 0

# The rules' item, up to the next item.
allowed=$(awk '/^- \*\*What may be JavaScript/ { on = 1; print; next }
               on && /^- / { exit }
               on { print }' "$rules" 2>/dev/null)
[ -n "$allowed" ] || allowed="(The \"What may be JavaScript\" item could not be read from $rules.)"

relative=${path#"$root"/}
context="This edit adds $added JavaScript function definition(s) to $relative. From the project rules, under *Scheme first*:

$allowed

In your reply, name the item above that requires this JavaScript. If none does, write it in Scheme instead."

jq -n --arg context "$context" --arg message "Scheme-first reminder: a JavaScript function is being added to $relative" \
  '{systemMessage: $message, hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $context}}'
