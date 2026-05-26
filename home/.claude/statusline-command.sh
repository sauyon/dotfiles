#!/bin/sh
input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model=$(echo "$input" | jq -r '.model.display_name')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Shorten home directory to ~ (POSIX-compatible)
home_prefix="$HOME/"
case "$cwd" in
    "$HOME") short_cwd="~" ;;
    "$HOME/"*) short_cwd="~/${cwd#$home_prefix}" ;;
    *) short_cwd="$cwd" ;;
esac

ESC=$(printf '\033')
# Palette (256-color)
C_SEP="${ESC}[38;5;240m"    # darker gray
C_MODEL="${ESC}[38;5;114m"  # soft green
C_CTX="${ESC}[38;5;222m"    # amber
C_COST="${ESC}[38;5;179m"   # gold
C_DIR="${ESC}[1;38;5;75m"   # bold bright blue
C_GIT="${ESC}[38;5;176m"    # pastel magenta
C_K8S="${ESC}[38;5;141m"    # soft purple
C_RATE="${ESC}[38;5;244m"   # dim gray
C_ACCT="${ESC}[38;5;109m"   # soft cyan
R="${ESC}[0m"
SEP=" ${C_SEP}|${R} "

# Git branch
git_branch=$(git --git-dir="$cwd/.git" symbolic-ref --short HEAD 2>/dev/null)
git_part=""
[ -n "$git_branch" ] && git_part=" ${C_GIT}(${git_branch})${R}"

k8s_ctx=$(kubectl config current-context 2>/dev/null | awk '{print $1}')
k8s_part=""
[ -n "$k8s_ctx" ] && k8s_part="${C_K8S}⎈ ${k8s_ctx}${R}"

# Context usage
ctx_part=""
[ -n "$used" ] && ctx_part="${C_CTX}ctx:${used}%${R}"

# Session cost
cost_usd=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')
cost_part=""
[ -n "$cost_usd" ] && cost_part="${C_COST}$(printf '$%.2f' "$cost_usd")${R}"

# Rate limits
five_h=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
seven_d=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
rate_part=""
if [ -n "$five_h" ] || [ -n "$seven_d" ]; then
    rate_part="${C_RATE}"
    [ -n "$five_h" ] && rate_part="${rate_part}5h:$(printf '%.0f' "$five_h")% "
    [ -n "$seven_d" ] && rate_part="${rate_part}7d:$(printf '%.0f' "$seven_d")%"
    rate_part="${rate_part}${R}"
fi

# Claude account email (from .claude.json, respecting CLAUDE_CONFIG_DIR)
acct_part=""
claude_json="${CLAUDE_CONFIG_DIR:-$HOME}/.claude.json"
[ -f "$claude_json" ] || claude_json="$HOME/.claude.json"
if [ -f "$claude_json" ]; then
    email=$(jq -r '.oauthAccount.emailAddress // empty' "$claude_json" 2>/dev/null)
    [ -n "$email" ] && acct_part="${C_ACCT}${email}${R}"
fi

user_host="${C_SEP}[${R}$(whoami)@$(hostname -s) ${C_DIR}${short_cwd}${R}${C_SEP}]${R}"

# Build output, joining non-empty parts with separator
output=""
for _val in "$ctx_part" "$cost_part" "$k8s_part" "$acct_part"; do
    [ -z "$_val" ] && continue
    [ -n "$output" ] && output="${output}${SEP}"
    output="${output}${_val}"
done

[ -n "$output" ] && output="${output}${SEP}"
output="${output}${user_host}${git_part}${SEP}${C_MODEL}${model}${R}"
[ -n "$rate_part" ] && output="${output}${SEP}${rate_part}"

printf "%s" "$output"
