#===============================================================================
#  _____    _              
# |__  /___| |__  _ __ ___ 
#   / // __| '_ \| '__/ __|
#  / /_\__ \ | | | | | (__ 
# /____|___/_| |_|_|  \___|
#                          
# Simple zsh resources with zplugin, created by RangHo.
#===============================================================================

# ZPLUGIN #
#---------#

source ~/.zplugin/bin/zplugin.zsh

zplugin light romkatv/powerlevel10k

zplugin light zsh-users/zsh-completions
zplugin light zsh-users/zsh-autosuggestions
zplugin light zdharma/fast-syntax-highlighting

zplugin light gretzky/auto-color-ls

zplugin ice pick"init.zsh" blockf
zplugin light laggardkernel/git-ignore

# KEYBINDINGS #
#-------------#

bindkey -e

# Zsh does not like inputrc for some reason
# So let's shove keybindings into its mouth
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[ShiftTab]="${terminfo[kcbt]}"

[[ -n "${key[Home]}" ]] \
    && bindkey -- "${key[Home]}"        beginning-of-line
[[ -n "${key[End]}" ]] \
    && bindkey -- "${key[End]}"         end-of-line
[[ -n "${key[Insert]}" ]] \
    && bindkey -- "${key[Insert]}"      overwrite-mode
[[ -n "${key[Backspace]}" ]] \
    && bindkey -- "${key[Backspace]}"   backward-delete-char
[[ -n "${key[Delete]}" ]] \
    && bindkey -- "${key[Delete]}"      delete-char
[[ -n "${key[Up]}" ]] \
    && bindkey -- "${key[Up]}"          up-line-or-history
[[ -n "${key[Down]}" ]] \
    && bindkey -- "${key[Down]}"        down-line-or-history
[[ -n "${key[Left]}" ]] \
    && bindkey -- "${key[Left]}"        backward-char
[[ -n "${key[Right]}" ]] \
    && bindkey -- "${key[Right]}"       forward-char
[[ -n "${key[PageUp]}" ]] \
    && bindkey -- "${key[PageUp]}"      beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] \
    && bindkey -- "${key[PageDown]}"    end-of-buffer-or-history
[[ -n "${key[ShiftTab]}"  ]] \
    && bindkey -- "${key[ShiftTab]}"    reverse-menu-complete

# Finally, make sure the terminal is in application mode,
# when zle is active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
    autoload -Uz add-zle-hook-widget
    function zle_application_mode_start {
        echoti smkx
    }
    function zle_application_mode_stop {
        echoti rmkx
    }
    add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
    add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi


# USER CONFIGURATION #
#--------------------#

# Preferred terminal emulator
export TERMINAL=alacritty

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='vim'
fi

# Add rubygems directory to $PATH
if which ruby >/dev/null && which gem >/dev/null; then
    export PATH="$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH)"
fi

# Source Awesome Terminal Fonts scripts
if [ -d /usr/share/fonts/awesome-terminal-fonts ]; then
    for script in /usr/share/fonts/awesome-terminal-fonts/*.sh; do
        source $script
    done
fi

# Source Nerd Fonts scripts
if [ -d /usr/lib/nerd-fonts-complete ]; then
    source /usr/lib/nerd-fonts-complete/i_all.sh
fi

# Some useful aliases
alias vim=nvim
alias cat=bat
alias ls=colorls

# Case-insensitive Completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# POWERLEVEL9K THEME #
#--------------------#

# Tell Powerlevel9k to use fontconfig
POWERLEVEL9K_MODE="nerdfont-complete"

# Start prompt on newline
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

# DIR settings
POWERLEVEL9K_HOME_ICON="$i_fa_home "
POWERLEVEL9K_HOME_SUB_ICON="$i_fa_folder_open "
POWERLEVEL9K_FOLDER_ICON="$i_fa_folder "
POWERLEVEL9K_ETC_ICON="$i_fa_puzzle_piece "
POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_SHORTEN_DELIMITER=""
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

# VCS settings
POWERLEVEL9K_VCS_GIT_ICON="$i_fa_git  "
POWERLEVEL9K_VCS_GIT_GITHUB_ICON="$i_fa_github  "
POWERLEVEL9K_VCS_GIT_BITBUCKET_ICON="$i_fa_bitbucket  "
POWERLEVEL9K_VCS_GIT_GITLAB_ICON="$i_fa_gitlab  "

# TIME settings
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M}"

# Something else
POWERLEVEL9K_CUSTOM_TERMINAL_ICON="echo $i_fa_terminal"

# Elements
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_terminal_icon dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status rbenv pyenv time)
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%{%B%F{yellow}%K{blue}%} $%{%b%f%k%F{blue}%} %{%f%}"

