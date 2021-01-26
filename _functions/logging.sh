# shellcheck shell=ash
# This script assumes at least `local` extension is enabled.

# Color constants
BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)

# Text style constants
BOLD=$(tput bold)
UNDERLINE=$(tput smul)
BLINK=$(tput blink)

# Reset test to normal style
NORMAL=$(tput sgr0)

debug() {
    [ "$DOTFILE_DEBUG" -eq 0 ] && [ "$DOTFILE_LOG_LEVEL" -lt 3 ] && return
    printf "[${BOLD}${WHITE}DEBUG${NORMAL}] ${WHITE}%s${NORMAL}\n" "$@" >&2
}

info() {
    [ "$DOTFILE_LOG_LEVEL" -lt 2 ] && return
    printf "[${BOLD}${BLUE}INFO${NORMAL}] ${WHITE}%s${NORMAL}\n" "$@" >&2
}

warn() {
    [ "$DOTFILE_LOG_LEVEL" -lt 1 ] && return
    printf "[${BOLD}${YELLOW}WARNING${NORMAL}] ${WHITE}%s${NORMAL}\n" "$@" >&2
}

error() {
    printf "[${BOLD}${RED}ERROR${NORMAL}] ${WHITE}%s${NORMAL}\n" "$@" >&2
}

