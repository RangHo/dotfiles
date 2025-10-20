# Nu Package Manager.
overlay use nupm/nupm --prefix

# Completion and prompting.
source "~/.cache/nushell/generated/carapace/mod.nu"
source "~/.cache/nushell/generated/starship/mod.nu"

# Package and version management.
overlay use mise
overlay use opam
