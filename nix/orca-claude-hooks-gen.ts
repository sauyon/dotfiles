// Regenerates nix/orca-claude-hooks.json: the hooks block Orca's own
// ClaudeHookService.install() writes into ~/.claude/settings.json, computed by
// Orca's source rather than copied by hand, so the entries match the form Orca
// counts as installed. home.nix merges them into claudeBaseSettings because
// that settings.json is a read-only store symlink Orca cannot write.
//
// Regenerate when Orca changes its hook form or Claude gains an event Orca
// gates on a version (an Orca bump that touches src/main/claude/):
//
//   git -C ~/devel/orca checkout v<orca version>   # a clone of stablyai/orca
//   mkdir -p ~/devel/orca/node_modules/@orca
//   ln -sfn ../../src/packages/process-host ~/devel/orca/node_modules/@orca/process-host
//   nix shell nixpkgs#bun -c bun --conditions=orca-source run \
//     nix/orca-claude-hooks-gen.ts ~/devel/orca "$(claude --version | cut -d' ' -f1)" \
//     > nix/orca-claude-hooks.json
//
// The statusLine Orca would also install is left out: claudeBaseSettings sets
// its own.
const [root, claudeVersion] = process.argv.slice(2)
if (!root || !claudeVersion) {
  console.error('usage: orca-claude-hooks-gen.ts <orca checkout> <claude version>')
  process.exit(2)
}
const { getClaudeManagedHookPlan } = await import(`${root}/src/main/claude/claude-managed-hook-events.ts`)
const { getManagedEventHook, getManagedScriptPath } = await import(`${root}/src/main/claude/hook-settings.ts`)
const orcaVersion = (await Bun.file(`${root}/package.json`).json()).version

const scriptPath = getManagedScriptPath()
const hooks: Record<string, unknown[]> = {}
for (const event of getClaudeManagedHookPlan(claudeVersion).install) {
  // The definition's `hooks` is a placeholder; install() fills in the command.
  const { hooks: _placeholder, ...rest } = event.definition
  hooks[event.eventName] = [{ ...rest, hooks: [getManagedEventHook(event.eventName, scriptPath)] }]
}
console.log(JSON.stringify({ orcaVersion, claudeVersion, hooks }, null, 2))
