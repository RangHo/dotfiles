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
    is $1 a command?
    yes && "$@" || debug "No such command: $1"
}

## is-checks
is() {
    # Syntax: is <subject> a/an <complement>?

    local subject article complement
    subject="$1"
    article="$2"
    complement=$(echo "$3" | sed -e 's/\?$//')

    if [ "$#" -ne 3 ] || [ "$article" != "a" ] && [ "$article" != "an" ]; then
        error "Usage: is <subject> a(n) <complement>?"
        return 1
    fi

    case "$complement" in
        command) command -v "$subject" > /dev/null ;;
        file) [ -e "$subject" ] ;;
        directory) [ -d "$subject" ] ;;
        symlink) [ -L "$subject" ] ;;

        *)
            error "<complement> can be either command, file, directory, or symlink."
            false
            ;;
    esac && checks_last_result="yes" || checks_last_result="no"
}

## does-checks
does() {
    # Syntax: does <subject> <verb> <object>?

    local subject verb object
    subject="$1"
    verb="$2"
    object=$(echo "$3" | sed -e 's/\?$//')

    if [ "$#" -ne 3 ]; then
        error "Usage: does <subject> <verb> <object>?"
        return 1
    fi

    case "$verb" in
        contain) does_contain "$subject" "$object" ;;

        *)
            error "<verb> must be contain."
            false
            ;;
    esac && checks_last_result="yes" || checks_last_result="no"

}

## Check if the "array" contains the item
does_contain() {
    local array item occurrence
    array=$1
    item=$2
    occurrence=$(printf "%s\n" $(eval "echo \"\$$array\"") | grep -c "^$item$")

    [ "$occurrence" -ne 0 ]
}

## Yes check
yes() {
    [ "$checks_last_result" = "yes" ]
}

## No check
no() {
    [ "$checks_last_result" != "yes" ]
}

