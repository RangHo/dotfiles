# Set up mise
use /tmp/mise.nu

# Set up starship prompt
use /tmp/starship.nu


# If carapace is available, use it for completions
let carapace_available = (which carapace | length) > 0
let carapace_completer = { |spans|
    carapace $spans.0 nushell ...$spans | from json
}

$env.config = {
    completions: {
        external: {
            enable: $carapace_available
            completer: $carapace_completer
        }
    }
}
