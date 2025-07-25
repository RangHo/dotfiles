# Nu Package Manager.
overlay use nupm/nupm --prefix

# OCaml package manager integration.
overlay use opam

# Generated integrations.
source "/tmp/carapace.nu"
overlay use "/tmp/mise.nu"
overlay use "/tmp/starship.nu"
