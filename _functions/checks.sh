# shellcheck shell=ash
# This script assumes at least `local` extension is enabled.

## Ensure a command succeeds
ensure() {
    if ! "$@"; then
        error "Command failed: $*"
        error "This command is required. Cannot proceed..."
        exit 1
    fi
}

## Maybe a command exists
maybe() {
    is_command $1 && "$@"
}

## Check if specified command exists
is_command() {
    command -v "$1" > /dev/null
}

## Check if specified file exists
is_file() {
    [ -e "$1" ]
}

## Check if specified directory exists
is_directory() {
    [ -d "$1" ]
}

## Check if specified symbolic link exists
is_symlink() {
    [ -L "$1" ]
}

