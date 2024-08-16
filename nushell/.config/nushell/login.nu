# Run a POSIX shell script in the current shell, keeping the environment.
def --env source-posix [filename] {
    /bin/sh -c $". ($filename) && env"
        | lines
        | parse "{key}={value}"
        | filter { |x| ($x.key not-in $env) or ($env | get $x.key) != $x.value }
        | where key not-in ["_" "LAST_EXIT_CODE" "DIRS_POSITION"]
        | transpose --header-row
        | into record
        | load-env
}

for profile in ["/etc/profile" "~/.profile"] {
    try { source-posix $profile }
}
