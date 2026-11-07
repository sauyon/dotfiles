// Regenerates the hook entries Orca's own installers write for Claude Code
// (~/.claude/settings.json) and cursor-agent (~/.cursor/hooks.json), computed
// by Orca's source rather than copied by hand: Orca recognises its entries by
// their command string, which changes with its wrapper. home.nix declares them
// because both files are read-only store symlinks Orca cannot write.
//
// Regenerate both on every Orca bump, and the Claude file on every claude-code
// version change too -- a manifest bump or a flake update that moves nixpkgs'
// claude-code past it -- since Orca gates events on Claude's version.
// tests/orca-agent-hooks.sh holds the files to both versions and, on a
// mismatch, names the claude-code version the config installs; pass that one
// (claude on PATH is the one installed, which before a switch may differ):
//
//   git -C ~/devel/orca checkout v<orca version>   # a clone of stablyai/orca
//   mkdir -p ~/devel/orca/node_modules/@orca
//   ln -sfn ../../src/packages/process-host ~/devel/orca/node_modules/@orca/process-host
//   gen() { nix shell nixpkgs#bun -c bun --conditions=orca-source run nix/orca-agent-hooks-gen.ts ~/devel/orca "$@"; }
//   gen claude "$(claude --version | cut -d' ' -f1)" > nix/orca-claude-hooks.json
//   gen cursor > nix/orca-cursor-hooks.json
//
// Claude's commands find the script through ${HOME-} at run time. Cursor's
// embed its absolute path, which is written out as @HOME@ and replaced with
// homeDirectory in home.nix. The statusLine Orca would
// also install for Claude is left out: claudeBaseSettings sets its own.
const [rootArg, agent, claudeVersion] = process.argv.slice(2)
// Absolute: import() resolves a relative path against this file, not the cwd.
const root = rootArg && (await import('node:path')).resolve(rootArg)
if (!rootArg || !(agent === 'cursor' || (agent === 'claude' && claudeVersion))) {
  console.error('usage: orca-agent-hooks-gen.ts <orca checkout> claude <claude version>')
  console.error('       orca-agent-hooks-gen.ts <orca checkout> cursor')
  process.exit(2)
}
const { homedir } = await import('node:os')
// The generating machine's home, as Orca's quoting embeds it, becomes @HOME@.
// Orca single-quotes every path, so the substitution is exact for any home
// without a quote in it.
const emit = (value: unknown) =>
  console.log(JSON.stringify(value, null, 2).replaceAll(`${homedir()}/.orca/`, '@HOME@/.orca/'))
const orcaVersion = (await Bun.file(`${root}/package.json`).json()).version
const { getSharedManagedScriptPath, buildManagedCommandDefinition } = await import(
  `${root}/src/main/agent-hooks/installer-utils.ts`
)
const hooks: Record<string, unknown[]> = {}

if (agent === 'claude') {
  const { getClaudeManagedHookPlan } = await import(`${root}/src/main/claude/claude-managed-hook-events.ts`)
  const { getManagedEventHook, getManagedScriptPath } = await import(`${root}/src/main/claude/hook-settings.ts`)
  const scriptPath = getManagedScriptPath()
  for (const event of getClaudeManagedHookPlan(claudeVersion).install) {
    // The definition's `hooks` is a placeholder; install() fills in the command.
    const { hooks: _placeholder, ...rest } = event.definition
    hooks[event.eventName] = [{ ...rest, hooks: [getManagedEventHook(event.eventName, scriptPath)] }]
  }
  emit({ orcaVersion, claudeVersion, hooks })
} else {
  const { CURSOR_EVENTS } = await import(`${root}/src/main/cursor/hook-events.ts`)
  const { getManagedCommand } = await import(`${root}/src/main/cursor/hook-script.ts`)
  // hook-service.ts's private getManagedScriptPath(), for Linux.
  const scriptPath = getSharedManagedScriptPath('cursor-hook.sh')
  for (const eventName of CURSOR_EVENTS) {
    hooks[eventName] = [buildManagedCommandDefinition(getManagedCommand(scriptPath, eventName))]
  }
  emit({ orcaVersion, hooks })
}
