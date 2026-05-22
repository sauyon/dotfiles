## Public Communication Discipline

ALWAYS ask for explicit confirmation when posting public communications, even when explicitly asked, unless 

NEVER create a pull request without explicitly asking the user for confirmation first.

NEVER post comments, reviews, or replies on a PR (including `gh pr comment`, `gh pr review`, `gh api .../comments`, or any other write to a PR's discussion) without explicitly asking the user for confirmation first. This includes pinging reviewers, leaving "ready for re-review" notes, replying to review threads, and resolving/marking review conversations. Read-only operations (`gh pr view`, `gh pr checks`, fetching diffs/comments) are fine.

## MCloud / Modular Cloud Access

The `mcloud` CLI is the standing way to hit the Modular Cloud (Yatai/mcloud) admin API. It stores its config in `~/.config/mcloud/config.toml` (api_url per context) and `~/.config/mcloud/credentials.toml` (api_token per context). Available contexts include `prod` (`https://modular.console.modular.com`) and `staging`.

The OpenAPI spec lives at `${api_url}/swagger/doc.json` (e.g. `https://modular.console.modular.com/swagger/doc.json`). You can use the token in `~/.config/mcloud/credentials.toml`.

## Linear

When you file Linear tickets on my behalf (or assigned to me), default the team to `Customer Engineering` (`CENG-`). Only use a different team when the work is genuinely for that team (e.g., asking IT to action something).

## Slack Message Style

When drafting Slack messages for me, match this style:

- **Lower-case sentence starts** — "should be live. should we update..." not "Should be live. Should we update..."
- "I" stays capitalized (it's the only exception)
- Casual contractions: "it's", "don't", "we're", "shouldn't"
- Hedge often: "I think", "probably", "should", "maybe", "not sure", "my understanding is", "for what it's worth"
- 1-2 sentences per message, usually short. If a thought has multiple beats, prefer sending as separate sequential messages rather than one long block (Slack-native rhythm).
- No greetings ("hey"), no sign-offs ("thanks"), no "let me know" — just the substance
- No bullet lists, no headers, no bold/italic formatting unless it really helps
- Soft proposals end in "?" — "should we update X first?"
- Sparing emoji, mostly `:sweat_smile:` style, never decorative
- Technical jargon used freely — assume the reader knows the system

When in doubt, lean shorter and less formal. If I provide a draft and ask for edits, preserve any phrasings I already wrote; only change what I called out.

## Secret Handling

NEVER cat or echo secrets, or otherwise run bash tool commands that will log them in the conversation. Get them directly in each tool call, eg with env vars `MY_SECRET=$(cat secretfile) command`.
