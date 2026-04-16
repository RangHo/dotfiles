# Nu modules.
$env.NU_LIB_DIRS = ls ($nu.default-config-dir | path join "modules") | each { $in.name }

# Generated init scripts.
def generate-init [tool: string, command: closure] {
  let local_vendor_autoload_dir = $nu.vendor-autoload-dirs | where { str starts-with $env.HOME } | last
  if not ($local_vendor_autoload_dir | path exists) {
    mkdir $local_vendor_autoload_dir
  }
  if (which $tool | is-not-empty) {
    do $command
  } else {
    "# $tool is not installed."
  } | save --force ($local_vendor_autoload_dir | path join ($tool + ".nu"))
}

generate-init carapace { carapace _carapace nushell }
generate-init mise { mise activate nu }
generate-init starship { starship init nu }
