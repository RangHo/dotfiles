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

    # Install fisher properly
    #fisher install jorgebucaran/fisher

    # Install other plugins
    fisher update

    # Delete the temporary directory and restore XDG_CONFIG_HOME
    rm -rf $tmpdir
    set -gx XDG_CONFIG_HOME $xdg_config_home_backup
end

# If fish_plugins file is different, run fisher update
diff -q -w (cat $__fish_config_dir/fish_plugins | sort | psub) (fisher list | sort | psub); \
    or fisher update

# Fix curses-based GPG pinentry screen
set -gx GPG_TTY (tty)

# If on WSL, then set VcXsrv as main display
if uname -r | grep -q -e [Mm]icrosoft
    set -gx DISPLAY (grep nameserver /etc/resolv.conf | cut -d ' ' -f 2):0
    set -gx LIBGL_ALWAYS_INDIRECT 1
end

# Import wal colorscheme, if exists
test -e ~/.cache/wal/sequences; \
    and /bin/cat ~/.cache/wal/sequences

# If there is ~/.profile, then source the file
if test -e ~/.profile
    fenv "source ~/.profile"
end

# If asdf version manager is installed, use that
if test -d ~/.asdf
    source ~/.asdf/asdf.fish
end
