#!/bin/bash

# Global xinitrc #######################################################
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/*.sh ; do
        . "$f"
    done
    unset f
fi


# System profile #######################################################
[ -r "/etc/profile" ] && . "/etc/profile"
[ -r "$HOME/.profile" ] && . "$HOME/.profile"


# System daemons #######################################################

# D-Bus session bus
eval "$(dbus-launch --sh-syntax --exit-with-session)"

# SSH agent
eval "$(ssh-agent -s)"

# Xfce polkit agent (requires D-Bus)
/usr/libexec/xfce-polkit &

# Xfce settings daemon (requires D-Bus)
xfsettingsd --replace &

# PipeWire multimedia service (requires D-Bus)
pipewire &


# Input methods ########################################################
fcitx5 -rd
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus
export XMODIFIERS=@im=fcitx


# Environment variables ################################################
export QT_STYLE_OVERRIDE=kvantum


# X server configuration ###############################################
xrdb -merge ~/.Xresources

xinput set-prop "PIXA3854:00 093A:0274 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "PIXA3854:00 093A:0274 Touchpad" "libinput Natural Scrolling Enabled" 1

xsetroot -cursor_name left_ptr


# X session ############################################################
exec "$HOME/.cache/xmonad/xmonad-x86_64-linux"
