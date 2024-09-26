# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# History settings
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

# Check window size after every command execution
shopt -s checkwinsize

# Use '**' pattern to match all files and directories recursively
shopt -s globstar

# Make less more friendly for non-text input files
if [ -x /usr/bin/lesspipe ]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

# Source external aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# If mise version manager is installed, use that
if command -v mise >/dev/null; then
    eval "$(mise activate bash)"
fi

# If starship prompt is installed, use that
if command -v starship >/dev/null; then
    eval "$(starship init bash)"
fi
