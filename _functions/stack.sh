# shellcheck shell=ash
# This script assumes at least `local` extension is enabled.

## Push a new item to stack
stack_push() {
    # Syntax: stack_push <stack_id> <item>

    if [ "$#" -ne 2 ]; then
        error "Usage: stack_push <stack_id> <item>"
        return 1
    fi

    local stack_id item
    stack_id="$1"
    item="$2"

    eval "stack_$stack_id=\"\${stack_$stack_id} $item\""
}

## Pop an item from stack to stdout
stack_pop() {
    # Syntax: stack_pop <stack_id>

    if [ "$#" -ne 1 ]; then
        error "Usage: stack_pop <stack_id>"
        return 1
    fi

    local stack_id result
    stack_id="$1"
    result=$(eval "echo \${stack_$stack_id}" | awk -F ' ' '{ print $NF }')

    # Check if the result is empty
    if [ -z "$result" ]; then
        warn "Stack is already empty."
        return
    fi

    # Remove the item from the end
    eval "stack_$stack_id=\$(echo \"\${stack_$stack_id}\" | sed -e 's/ $result$//')"

    # Print the last element to stdout
    echo $result
}

## Print the content of the stack
stack_print() {
    # Syntax: stack_print <stack_id>

    if [ "$#" -ne 1 ]; then
        error "Usage: stack_print <stack_id>"
        return 1
    fi
    
    local stack_id
    stack_id="$1"

    eval "echo \${stack_$stack_id}"
}

## Print the name of the stack
stack_name() {
    # Syntax: stack_name <stack_id>

    if [ "$#" -ne 1 ]; then
        error "Usage: stack_name <stack_id>"
        return 1
    fi

    local stack_id
    stack_id="$1"

    echo "stack_$stack_id"
}

