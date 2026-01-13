# Nu modules.
$env.NU_LIB_DIRS = ls ($nu.default-config-dir | path join "modules") | each { $in.name }

# External initialization scripts.
let generated_lib_dir = if "XDG_CACHE_HOME" in $env {
  $env.XDG_CACHE_HOME
} else {
    $env.HOME | path join ".cache"
} | path join "nushell" "generated"
$env.NU_LIB_DIRS = [$generated_lib_dir, ...$env.NU_LIB_DIRS]

def generate-init [tool: string, command: closure] {
  if not ($"($generated_lib_dir)/($tool)/mod.nu" | path exists) {
    mkdir $"($generated_lib_dir)/($tool)"
    if (which $tool | is-not-empty ) {
      do $command
    } else {
      $"# ($tool) is not installed"
    } | save --force $"($generated_lib_dir)/($tool)/mod.nu"
  }
}

generate-init carapace { carapace _carapace nushell }
generate-init mise { mise activate nu }
generate-init starship { starship init nu }
