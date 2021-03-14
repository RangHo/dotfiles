#!/bin/env fish

# Bootstrap Fisher package manager if not installed already
if ! type -q fisher; and test "$__fish_fisher_installed" != yes
    # Download fisher
    echo "Fisher package manager is not installed."
    echo "Downloading Fisher package manager..."
    curl -sL https://git.io/fisher | source
    echo "Finished downloading Fisher!"

    # Set universal variable lest fish recurse infinitely
    set --universal __fish_fisher_installed yes

    # Install fisher properly
    fisher install jorgebucaran/fisher

    # Install other plugins
    fisher update
end

# Import wal colorscheme, if exists
if test -e ~/.cache/wal/sequences
    /bin/cat ~/.cache/wal/sequences
end

# If there is ~/.profile, then source the file
if test -e ~/.profile
    fenv "source ~/.profile"
end

# If asdf version manager is installed, use that
if test -d ~/.asdf
    source ~/.asdf/asdf.fish
end
