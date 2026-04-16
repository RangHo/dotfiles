case ":$PATH:" in
    *:"$HOME/.juliaup/bin":*)
        # Do nothing
        ;;
    *)
        export PATH="$HOME/.juliaup/bin:$PATH"
        ;;
esac
