# starship prompt
if not ("/tmp/starship.nu" | path exists) {
    if (which starship | length) > 0 {
        starship init nu | save /tmp/starship.nu
    } else {
        "# starship is not installed" | save /tmp/starship.nu
    }
}
