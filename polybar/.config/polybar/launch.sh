#!/bin/bash

# Kill polybar if running
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do
    sleep 1
done

# Launch all bars in bars directory
for bar in $HOME/.config/polybar/bars/*; do
    polybar $(basename $bar) &
done
