if [ -e "$XDG_CACHE_HOME/wal/sequences" ]; then
    [ "$TERM" = "xterm-256color" ] && cat "$XDG_CACHE_HOME/wal/sequences"
fi
