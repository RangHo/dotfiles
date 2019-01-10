#===============================================================================
#  _____    _              
# |__  /___| |__  _ __ ___ 
#   / // __| '_ \| '__/ __|
#  / /_\__ \ | | | | | (__ 
# /____|___/_| |_|_|  \___|
#                          
# Generated using Oh-My-Zsh, modified by RangHo.
# 
# For a complete reference, visit: 
# https://github.com/robbyrussell/oh-my-zsh/wiki
#===============================================================================

# ZSH SYSTEM CONFIGURATION #
#--------------------------#

# Path to Oh-My-Zsh configuration
export ZSH="/home/zu0107/.oh-my-zsh"

# Theme to decorate the terminal.
ZSH_THEME="powerlevel9k/powerlevel9k"

# Make the shell more tolerant about fucking up hyphens and underscores.
HYPHEN_INSENSITIVE="true"

# Disable bi-weekly auto-update checks.
#DISABLE_AUTO_UPDATE="true"

# Change automatic update-checking interval.
#export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable auto-setting terminal title.
#DISABLE_AUTO_TITLE="true"

# Enable command auto-correction.
#ENABLE_CORRECTION="true"

# Display red dots whilst waiting for completion.
#COMPLETION_WAITING_DOTS="true"

# Disable marking untracked files as dirty in VCS's.
#DISABLE_UNTRACKED_FILES_DIRTY="true"

# Change the command execution timestamp shown in the history command output.
#HIST_STAMPS="mm/dd/yyyy"

# Specify plugins to load.
# Install custom plugins in $ZSH/custom/plugins.
plugins=(
    git
    thefuck
    vscode
    web-search
    zsh_reload
    fast-syntax-highlighting
)

# Now fire up Oh My Zsh!
source $ZSH/oh-my-zsh.sh

#==============================================================================#

# USER CONFIGURATION #
#--------------------#

# Preferred terminal emulator
export TERMINAL=xfce4-terminal

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='mvim'
fi

# Source Awesome Terminal Fonts scripts
if [ -d /usr/share/fonts/awesome-terminal-fonts ]; then
    for script in /usr/share/fonts/awesome-terminal-fonts/*.sh; do
        source $script
    done
fi

# POWERLEVEL9K THEME #
#--------------------#

code2icon() {
    echo "\\u$1 "
}

# Tell Powerlevel9k to use fontconfig
POWERLEVEL9K_MODE="awesome-fontconfig"

# Start prompt on newline
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

# DIR settings
POWERLEVEL9K_HOME_ICON=$(code2icon $CODEPOINT_OF_AWESOME_HOME)
POWERLEVEL9K_HOME_SUB_ICON=$(code2icon $CODEPOINT_OF_AWESOME_FOLDER_OPEN)
POWERLEVEL9K_FOLDER_ICON=$(code2icon $CODEPOINT_OF_AWESOME_FOLDER)
POWERLEVEL9K_ETC_ICON=$(code2icon $CODEPOINT_OF_AWESOME_PUZZLE_PIECE)
POWERLEVEL9K_DIR_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_to_first_and_last"

# VCS settings
POWERLEVEL9K_VCS_GIT_ICON=$(code2icon $CODEPOINT_OF_AWESOME_GIT)
POWERLEVEL9K_VCS_GIT_GITHUB_ICON=$(code2icon $CODEPOINT_OF_AWESOME_GITHUB)
POWERLEVEL9K_VCS_GIT_BITBUCKET_ICON=$(code2icon $CODEPOINT_OF_AWESOME_BITBUCKET)
POWERLEVEL9K_VCS_GIT_GITLAB_ICON=$(code2icon $CODEPOINT_OF_AWESOME_GITLAB)

# TIME settings
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M $(code2icon $CODEPOINT_OF_AWESOME_CALENDAR) %y/%m/%d}"

# Something else
POWERLEVEL9K_CUSTOM_TERMINAL_ICON="echo $(code2icon $CODEPOINT_OF_AWESOME_TERMINAL)"

# Elements
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_terminal_icon dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(virtualenv time)
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%{%B%F{yellow}%K{blue}%} $%{%b%f%k%F{blue}%} %{%f%}"
