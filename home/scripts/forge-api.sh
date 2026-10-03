# forge.ko.ag REST helpers, shared by the scripts in home.nix that wait on a CI
# run (`hmeval`, and `hms` once it is moved onto this file).
#
# Sourced, not executed. The caller sets these first — they are deliberately
# plain variables rather than arguments, so each script keeps its own test seam
# (HMS_CURL, HMEVAL_CURL, ...) without this file knowing about any of them:
#
#   forge       base URL, e.g. https://forge.ko.ag
#   slug        owner/repo
#   curl        path to curl
#   jq          path to jq
#   grep        path to GNU grep
#   awk         path to gawk
#   token_cmd   a git-credential helper that answers `get` for forge.ko.ag
#   tmp         a private 0700 scratch dir the caller created and traps on
#
# Everything below is lifted from `hms`, where the comments were earned the hard
# way. Do not simplify one without re-reading the reason attached to it.

# The token lands in a 0600 curl config, never in argv or the environment —
# /proc/<pid>/cmdline and environ are world-readable.
write_curlrc() {
  local tok
  # The first password line is the token, and only the first: the curlrc is
  # line-oriented, so a second one would not lengthen the header but end it and
  # leave a stray directive behind. Read to EOF rather than exiting on the
  # match — under `set -o pipefail` an early exit SIGPIPEs the helper and fails
  # the pipeline.
  tok=$(printf 'host=forge.ko.ag\n\n' | "$token_cmd" get \
    | "$awk" '/^password=/ && !seen { sub(/^password=/, ""); print; seen = 1 }')
  [ -n "$tok" ] || return 1
  ( umask 077
    printf 'header = "Authorization: token %s"\nsilent\nconnect-timeout = 10\nmax-time = 120\n' \
      "$tok" > "$tmp/curlrc" )
}

# An HTTP error must not read as success — otherwise a dead token gives jq an
# error body to find nothing in, and the caller concludes "nothing to wait for".
# --fail alone only yields curl's exit 22 for everything >=400, which cannot
# tell an expired token from a forge having a bad day, so the status code is
# carried out explicitly. The code goes to a file, not a variable: call sites
# are `x=$(api ...)`, which runs api in a subshell, so an assignment inside it
# would never reach api_why. 0 means no HTTP response at all.
#
# The header is re-minted before every request rather than once per run.
# forge.ko.ag is an OAuth grant whose access token expires roughly an hour out,
# so a snapshot taken before a long wait is routinely dead by the end of it.
# git-credential-fj owns the expiry check and the `fj whoami` poke that mints a
# new one; asking it per request is what picks that up. Best-effort, exactly
# like the helper itself: a refresh that fails leaves the last good curlrc in
# place, so one hiccup cannot throw away a wait that is already minutes deep.
api() {
  local p="$1"; shift
  local out code
  write_curlrc || true
  if ! out=$($curl -K "$tmp/curlrc" -w '\n%{http_code}' "$forge$p" "$@"); then
    echo 0 > "$tmp/code"; return 1
  fi
  code=${out##*$'\n'}
  echo "$code" > "$tmp/code"
  printf '%s' "${out%$'\n'*}"
  [ "$code" -lt 400 ]
}

# Why the request failed, in the terms that decide what you do about it.
api_why() {
  local code; code=$(cat "$tmp/code" 2>/dev/null || echo 0)
  case "$code" in
    0)       echo "could not reach $forge" ;;
    # Every request re-mints the header through git-credential-fj, which
    # refreshes an expired grant on its own. So a 401 that survives that is not
    # staleness — it is a login that can no longer be refreshed. Emphatically
    # *not* `fj auth add-token`: an application token has no expires_at, so it
    # skips the refresh poke entirely and "fixes" this by abandoning the OAuth
    # login instead.
    401|403) echo "$forge rejected our token (HTTP $code) — try 'fj auth login'" ;;
    5??)     echo "$forge returned HTTP $code — server-side, retry later" ;;
    *)       echo "$forge returned HTTP $code" ;;
  esac
}

# The job-id lookup is the exception: it *wants* the 404 body, which is the only
# place Forgejo 13 names the job id. Failing on it would throw that away.
api_raw() { local p="$1"; shift; write_curlrc || true; $curl -K "$tmp/curlrc" "$forge$p" "$@"; }

# Fetch a finished run's job log to $1. Forgejo 13 exposes no run->job API, but
# the web job endpoint names the job id in its error body, and
# /actions/jobs/<id>/logs then serves the log.
#
# Returns 1 and leaves $1 absent when the log could not be fetched, so the
# caller can say so rather than print an empty "--- CI error ---", which reads
# as "the run failed silently" — a different and much more alarming bug.
fetch_job_log() {
  local run="$1" dest="$2" job
  job=$(api_raw "/$slug/actions/runs/$run/jobs/0" -X POST \
          -H 'Content-Type: application/json' -d '{"logCursors":[]}' \
        | "$grep" -o 'job_id [0-9]*' | tr -dc '0-9' || true)
  [ -n "$job" ] || return 1
  api "/api/v1/repos/$slug/actions/jobs/$job/logs" > "$dest" || return 1
}

# Wait for a run whose head_sha is $1 and whose workflow is $2 to appear, for up
# to $3 seconds. Echoes the run_number, or nothing if none showed up.
#
# Tolerates a failed request rather than assigning from a failing pipeline:
# under `set -o pipefail` that would abort a wait minutes deep over one blip.
# The caller reads $tmp/poll_ok to tell "the forge just told us there is no run"
# from "we could not ask" — a success ten tries ago does not answer that.
await_run() {
  local sha="$1" wf="$2" budget="$3" run="" tasks give_up=$((SECONDS + $3))
  while [ -z "$run" ]; do
    if tasks=$(api "/api/v1/repos/$slug/actions/tasks?limit=20"); then
      echo 1 > "$tmp/poll_ok"
      run=$(printf '%s' "$tasks" | $jq -r --arg s "$sha" --arg w "$wf" 'first(.workflow_runs[]
          | select(.head_sha == $s and .workflow_id == $w)
          | .run_number) // empty' 2>/dev/null || true)
    else
      echo "" > "$tmp/poll_ok"
    fi
    [ -z "$run" ] || break
    [ "$SECONDS" -lt "$give_up" ] || return 1
    printf '.' >&2; sleep 5
  done
  printf '%s' "$run"
}

# Poll run number $1 to a terminal status, bounded by $2 seconds. Echoes the
# final status, or nothing on timeout. A blip leaves the status alone and the
# loop simply asks again; bounded by SECONDS rather than an iteration count,
# since a request can burn max-time before returning.
await_status() {
  local run="$1" status="" degraded="" tasks deadline=$((SECONDS + $2))
  while :; do
    if tasks=$(api "/api/v1/repos/$slug/actions/tasks?limit=20"); then
      [ -z "$degraded" ] || { echo "forge back" >&2; degraded=""; }
      status=$(printf '%s' "$tasks" | $jq -r --arg n "$run" 'first(.workflow_runs[]
          | select(.run_number == ($n | tonumber)) | .status) // empty' 2>/dev/null || true)
    else
      # Say it once per outage rather than every poll, and once on recovery: a
      # long silence is indistinguishable from patient waiting.
      [ -n "$degraded" ] || { echo "$(api_why); still waiting" >&2; degraded=1; }
    fi
    case "$status" in
      success|failure|cancelled|skipped) printf '%s' "$status"; return 0 ;;
    esac
    [ "$SECONDS" -lt "$deadline" ] || return 1
    printf '.' >&2; sleep 5
  done
}
