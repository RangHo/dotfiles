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

# Append ~/.local/bin to PATH
export PATH=$PATH:~/.local/bin

# Preferred terminal emulator
export TERMINAL=alacritty

# Preferred editor for local and remote sessions
#if [[ -n $SSH_CONNECTION ]]; then
#  export EDITOR='vim'
#else
#  export EDITOR='mvim'
#fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# SSH key location
# export SSH_KEY_PATH="~/.ssh/rsa_id"
