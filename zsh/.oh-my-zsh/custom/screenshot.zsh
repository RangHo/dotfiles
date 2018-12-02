#===============================================================================
# Take screenshots easy!
#===============================================================================

# This script requires maim to be installed.
# If it is not, it will fallback to scrot.

main() {
    if ! [ -d ~/Pictures/Screenshots ]; then
        mkdir -p ~/Pictures/Screenshots
    fi

    maim \
        | tee ~/Pictures/Screenshots/$(date +%Y%m%d%H%M%S).png \
        | xclip -selection clipboard -t image/png

    notify-send --expire-time=2500 --icon=view-fullscreen,'~/Picture/Resources/hibiki-smile.png' 'Smile!' 'Screenshot is ready to go!'
}

alias screenshot=main
