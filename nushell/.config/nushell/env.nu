# Nu package manager.
$env.NUPM_HOME = $env.XDG_DATA_HOME | path join "nupm"
$env.NU_LIB_DIRS = [
    ($env.NUPM_HOME | path join "modules")
]
$env.PATH = (
    $env.PATH
        | split row (char esep)
        | prepend ($env.NUPM_HOME | path join "scripts")
        | uniq
)

# External initialization scripts.
def generate-init [tool: string, command: closure] {
  if not ($"/tmp/($tool).nu" | path exists) {
    if (which $tool | is-not-empty ) {
      do $command
    } else {
      $"# ($tool) is not installed"
    } | save --force $"/tmp/($tool).nu"
  }
}

generate-init carapace { carapace _carapace nushell }
generate-init mise { mise activate nu }
generate-init starship { starship init nu }
