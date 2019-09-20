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

if [ ! -e ~/.zplugin/bin/zplugin.zsh ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

source ~/.zplugin/bin/zplugin.zsh

zplugin light romkatv/powerlevel10k

zplugin light zsh-users/zsh-completions
zplugin light zsh-users/zsh-autosuggestions
zplugin light zdharma/fast-syntax-highlighting

zplugin light desyncr/auto-ls
AUTO_LS_COMMANDS=('/bin/lsd')

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
alias ls=lsd

# Some useful utilities
colormap() {
    for i in {0..255}; do
        print -Pn "%${i}F${(l:3::0:)i}%f " ${${(M)$((i % 8)):#7}:+$'\n'}
    done
}

reshell() {
    clear
    exec $SHELL
}

# Case-insensitive Completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# POWERLEVEL9K THEME #
#--------------------#

# Tell Powerlevel9k to use fontconfig
POWERLEVEL9K_MODE="nerdfont-complete"

# Basic color settings
#POWERLEVEL9K_BACKGROUND=
#POWERLEVEL9K_FOREGROUND=

# Segment settings
POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR="%246F$i_pl_left_soft_divider"
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR="%246F$i_pl_right_soft_divider"
POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR="$i_pl_left_hard_divider"
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR="$i_pl_right_hard_divider"
POWERLEVEL9K_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL=
POWERLEVEL9K_RIGHT_PROMPT_FIRST_SEGMENT_START_SYMBOL="$i_pl_right_hard_divider"
POWERLEVEL9K_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL="$i_pl_left_hard_divider"
POWERLEVEL0K_RIGHT_PROMPT_LAST_SEGMENT_END_SYMBOL=

# Leave a line between two prompts
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
    # Line 1
    context
    dir
    vcs

    # Line 2
    newline
    prompt_char
)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
    # Line 1

    # Line 2
    newline
    status
    nvm
    rvm
    pyenv
    rust_version
)

POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX=""

# --- CONTEXT settings ----
POWERLEVEL9K_CONTEXT_FOREGROUND=180
POWERLEVEL9K_CONTEXT_TEMPLATE="%6F%n%248F at %5F%m"

# ---- DIR settings ----
POWERLEVEL9K_HOME_ICON="$i_fa_home"
POWERLEVEL9K_HOME_SUB_ICON="$i_fa_folder_open"
POWERLEVEL9K_FOLDER_ICON="$i_fa_folder"
POWERLEVEL9K_ETC_ICON="$i_fa_puzzle_piece"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_SHORTEN_DELIMITER=""
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_to_unique"
POWERLEVEL9K_SHORTEN_FOLDER_MARKER="(.bzr|.citc|.git|.hg|.node-version|.python-version|.ruby-version|.shorten_folder_marker|.svn|.terraform|CVS|Cargo.toml|composer.json|go.mod|package.json|)"
POWERLEVEL9K_DIRSHORTENED_FOREGROUND=18
POWERLEVEL9K_DIR_ANCHOR_FOREGROUND=55
POWERLEVEL9k_DIR_ANCHOR_BOLD=true
POWERLEVEL9K_DIR_PREFIX="in "

# ---- VCS settings ----
POWERLEVEL9K_VCS_GIT_ICON="$i_fa_git "
POWERLEVEL9K_VCS_GIT_GITHUB_ICON="$i_fa_github "
POWERLEVEL9K_VCS_GIT_BITBUCKET_ICON="$i_fa_bitbucket "
POWERLEVEL9K_VCS_GIT_GITLAB_ICON="$i_fa_gitlab "
POWERLEVEL9K_VCS_BRANCH_ICON="$i_oct_git_branch"
POWERLEVEL9K_VCS_TAG_ICON="$i_oct_tag"
POWERLEVEL9K_VCS_UNTRACKED_ICON="$i_oct_question"

# ---- PROMPT_CHAR settings ----
POWERLEVEL9K_PROMPT_CHAR_OK_VIINS_FOREGROUND=119
POWERLEVEL9K_PROMPT_CHAR_OK_VICMD_FOREGROUND=119
POWERLEVEL9K_PROMPT_CHAR_OK_VIVIS_FOREGROUND=119
POWERLEVEL9K_PROMPT_CHAR_ERROR_VIINS_FOREGROUND=204
POWERLEVEL9K_PROMPT_CHAR_ERROR_VICMD_FOREGROUND=204
POWERLEVEL9K_PROMPT_CHAR_ERROR_VIVIS_FOREGROUND=204
POWERLEVEL9K_PROMPT_CHAR_OK_VIINS_CONTENT_EXPANSION="(✿ •⌄•) 〜$i_mdi_heart "
POWERLEVEL9K_PROMPT_CHAR_ERROR_VIINS_CONTENT_EXPANSION="(✿ ˃⌓˂) 〜$i_mdi_heart_broken "
POWERLEVEL9K_PROMPT_CHAR_OK_VICMD_CONTENT_EXPANSION="(✿ •⌄•)✎ "
POWERLEVEL9K_PROMPT_CHAR_ERROR_VICMD_CONTENT_EXPANSION="(✿ •o•)✎ "
POWERLEVEL9K_PROMPT_CHAR_OK_VIVIS_CONTENT_EXPANSION="(✿ •~•).oO"
POWERLEVEL9K_PROMPT_CHAR_ERROR_VIVIS_CONTENT_EXPANSION="(✿ •⌓•).oO"

# ---- STATUS settings ----
POWERLEVEL9K_STATUS_EXTENDED_STATES=true
POWERLEVEL9K_STATUS_OK=false
POWERLEVEL9K_STATUS_OK_FOREGROUND=0
POWERLEVEL9K_STATUS_OK_BACKGROUND=119
POWERLEVEL9K_STATUS_OK_VISUAL_IDENTIFIER_EXPANSION="$i_fa_thumbs_up "
POWERLEVEL9K_STATUS_OK_PIPE=true
POWERLEVEL9K_STATUS_OK_PIPE_FOREGROUND=0
POWERLEVEL9K_STATUS_OK_PIPE_BACKGROUND=119
POWERLEVEL9K_STATUS_OK_PIPE_VISUAL_IDENTIFIER_EXPANSION="$i_fa_thumbs_up "
POWERLEVEL9K_STATUS_ERROR=false
POWERLEVEL9K_STATUS_ERROR_FOREGROUND=0
POWERLEVEL9K_STATUS_ERROR_BACKGROUND=204
POWERLEVEL9K_STATUS_ERROR_VISUAL_IDENTIFIER_EXPANSION="$i_fa_thumbs_down "
POWERLEVEL9K_STATUS_ERROR_SIGNAL=true
POWERLEVEL9K_STATUS_ERROR_SIGNAL_FOREGROUND=0
POWERLEVEL9K_STATUS_ERROR_SIGNAL_BACKGROUND=204
POWERLEVEL9K_STATUS_ERROR_SIGNAL_VISUAL_IDENTIFIER_EXPANSION="$i_fa_thumbs_down "
POWERLEVEL9K_STATUS_ERROR_PIPE=true
POWERLEVEL9K_STATUS_ERROR_PIPE_FOREGROUND=0
POWERLEVEL9K_STATUS_ERROR_PIPE_BACKGROUND=204
POWERLEVEL9K_STATUS_ERROR_PIPE_VISUAL_IDENTIFIER_EXPANSION="$i_fa_thumbs_down "



