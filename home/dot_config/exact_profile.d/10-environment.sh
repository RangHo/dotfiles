for env_file in $HOME/.config/environment.d/*; do
    if [ -r "$env_file" ]; then
        vars=$(grep -E '^[A-Za-z_][A-Za-z0-9_]*=' "$env_file")
        . "$env_file"
        for var in $vars; do
            var_name=$(echo "$var" | cut -d= -f1)
            export "$var_name"
        done
    fi
done
