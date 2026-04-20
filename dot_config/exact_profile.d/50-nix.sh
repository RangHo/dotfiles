if [ -r "$XDG_STATE_HOME/nix/profile" ]; then
    export PATH="$XDG_STATE_HOME/nix/profile/bin:$PATH"
fi
