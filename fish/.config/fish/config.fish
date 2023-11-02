#!/bin/env fish

# Bootstrap Fisher package manager if not installed already
if not type -q fisher
    # Download fisher
    echo "Fisher package manager is not installed."
    echo "Downloading Fisher package manager..."
    curl -sL https://git.io/fisher | source
    echo "Finished downloading Fisher!"

    # Change XDG_CONFIG_HOME temporarily to prevent nonexistent commmands
    # from being executed
    set tmpdir (mktemp --directory)
    set xdg_config_home_backup $XDG_CONFIG_HOME
    set -x XDG_CONFIG_HOME $tmpdir

    # Install other plugins
    fisher update

    # Delete the temporary directory and restore XDG_CONFIG_HOME
    rm -rf $tmpdir
    set -gx XDG_CONFIG_HOME $xdg_config_home_backup
end

# Try updating the plugins
if not set --query __dotfiles_fish_update
    set -x __dotfiles_fish_update 1

    diff -qw (cat $__fish_config_dir/fish_plugins | sort | psub) (fisher list | sort | psub) >/dev/null
    or fisher update
end

# Fix curses-based GPG pinentry screen
set -gx GPG_TTY (tty)

# If there is ~/.profile, then source the file
test -e ~/.profile
and fenv "source ~/.profile"

# If asdf version manager is installed, use that
test -e ~/.asdf/asdf.fish
and source ~/.asdf/asdf.fish

# If rtx version manager is installed, use that
type -q rtx
and rtx activate fish | source

# If thefuck command correction engine is installed, use that
type -q thefuck
and thefuck --alias | source
