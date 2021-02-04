#!/bin/bash

# Find all network interfaces
NETWORK_INTERFACES=$(ip link show | egrep '^[0-9]+:' | cut -d':' -f2)
export WIRELESS_DEVICE=$(for dev in $NETWORK_INTERFACES; do
                             echo $dev | egrep '^[wW]' && break
                         done)
export ETHERNET_DEVICE=$(for dev in $NETWORK_INTERFACES; do
                             echo $dev | egrep '^[eE]' && break
                         done)

# Kill polybar if running
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do
    sleep 1
done

# Launch all bars in bars directory
for bar in $HOME/.config/polybar/bars/*; do
    polybar $(basename $bar) &
    sleep 0.1
done
