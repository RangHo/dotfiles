# shellcheck shell=ash
# This script assumes at least `local` extension is enabled.

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

