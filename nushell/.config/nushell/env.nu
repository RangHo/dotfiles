# starship prompt
if not ("/tmp/starship.nu" | path exists) {
  if (which starship | length) > 0 {
    starship init nu | save /tmp/starship.nu
  } else {
    "# starship is not installed" | save /tmp/starship.nu
  }
}

if not ("/tmp/mise.nu" | path exists) {
  if (which mise | length) > 0 {
    mise activate nu | save /tmp/mise.nu
  } else {
    "# mise is not installed" | save /tmp/mise.nu
  }
}
