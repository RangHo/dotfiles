#!/bin/bash

# Global xinitrc #######################################################
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi


# System daemons #######################################################
eval "$(dbus-launch --sh-syntax --exit-with-session)"
eval "$(ssh-agent -s)"


# Input methods ########################################################
fcitx5 -rd
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export SDL_IM_MODULE=fcitx
export GLFW_IM_MODULE=ibus
export XMODIFIERS=@im=fcitx


# Environment variables ################################################
export GTK2_RC_FILES=~/.gtkrc-2.0
export QT_QPA_PLATFORMTHEME=gtk2

if [ -f "$HOME/.local/bin/mise" ]; then
    eval "$($HOME/.local/bin/mise env -s bash)"
fi


# X server configuration ###############################################
xrdb -merge ~/.Xresources

xinput set-prop "PIXA3854:00 093A:0274 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "PIXA3854:00 093A:0274 Touchpad" "libinput Natural Scrolling Enabled" 1

xsetroot -cursor_name left_ptr


# X session ############################################################
exec $HOME/.cache/xmonad/xmonad-x86_64-linux
