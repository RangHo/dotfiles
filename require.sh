#!/bin/sh

NO_COLOR='\033[0m'  # Text Reset
BLACK='\033[0;30m'  # Black
RED='\033[0;31m'    # Red
GREEN='\033[0;32m'  # Green
YELLOW='\033[0;33m' # Yellow
BLUE='\033[0;34m'   # Blue
PURPLE='\033[0;35m' # Purple
CYAN='\033[0;36m'   # Cyan
WHITE='\033[0;37m'  # White

install_ARCH() {
    # Installs packages from Arch Official Repository.
    
    echo -e "\n${WHITE}Installing ${CYAN}Arch Official Packages${WHITE}...${NO_COLOR}"
    sudo pacman -Sy $@
}

install_AUR() {
    # Installs packages from Arch User Repository.

    echo -e "\n${WHITE}Installing ${CYAN}Arch User Packages${WHITE}...${NO_COLOR}"
    if ! command -v aur; then
        echo -e "${RED}AURUTILS does not exist! Installing that first...${NO_COLOR}"
        
        aur_directory=$(mktemp -d)
        pushd $aur_directory
        
        git clone https://aur.archlinux.org/aurutils
        
        cd aurutils
        makepkg -si

        popd
        rm -rf $aur_directory

        echo -e "\n[AUR]\nSigLevel = Optional TrustAll\nServer = file:///home/$USER/AUR" \
            | sudo tee -a /etc/pacman.conf

        sudo install -d /home/$USER/AUR/ -o $USER
        repo-add /home/$USER/AUR/AUR.db.tar

        sudo pacman -Sy
    fi

    for package in $@; do
        aur sync $package
    done

    sudo pacman -Sy $@

}

install_PIP() {
    # Installs packages from PyPI.

    echo -e "\n${WHITE}Installing ${BLUE}PyPI Packages${WHITE}...${NO_COLOR}"
    if ! command -v pip; then
        echo -e "${RED}Python-PIP does not exist! Installing that first...${NO_COLOR}"
        
        sudo pacman -Sy python python-pip
    fi

    sudo pip install $@
}

install_GEM() {
    # Installs packages from RubyGem.

    echo -e "\n${WHITE}Installing ${RED}RubyGem Packages${WHITE}...${NO_COLOR}"
    if ! command -v gem; then
        echo -e "${RED}RubyGem does not exist! Installing that first...${NO_COLOR}"
        
        sudo pacman -Sy ruby rubygems
    fi

    gem install $@
}

install_DOTFILE() {
    # Installs other dotfiles.

    echo -e "\n${WHITE}Installing ${GREEN}other Dotfiles${WHITE}...${NO_COLOR}"
    for package in $@; do
        make install=$package
    done
}

ensure_exist() {
    # Ensures that directories exist.

    for path in $1; do
        if ! [ -d "$path" ]; then
            echo -e "\n${GREEN}$path${WHITE} directory is required. Creating...${NO_COLOR}"

            mkdir -p $(eval "echo $path")
        fi
    done
}

ensure_nonexist() {
    # Ensures that directories/files do not exist.
    
    for path in $1; do
        if [ "$path" -o -d "$path" ]; then
            echo -e "\n${RED}$path${WHITE} directory must be removed. Deleting...${NO_COLOR}"

            rm -rf $(eval "echo $path")
        fi
    done
}

create_links() {
    # Creates symbolic links

    for link in $@; do
        
        splitted=( $(echo $link | sed 's/->/ /') )
        from=${splitted[0]}
        to=${splitted[1]}

        echo -e "\n${WHITE}Creating symlinks from ${YELLOW}$from${WHITE} to ${GREEN}$to${WHITE}...${NO_COLOR}"

        sudo ln -s $from $to
    done
}

source $1

if [ "$ARCH" ]; then
    install_ARCH ${ARCH[@]}
fi

if [ "$AUR" ]; then
    install_AUR ${AUR[@]}
fi

if [ "$PIP" ]; then
    install_PIP ${PIP[@]}
fi

if [ "$GEM"]; then
    install_GEM ${GEM[@]}
fi

if [ "$DOTFILE" ]; then
    install_DOTFILE ${DOTFILE[@]}
fi

if [ "$ENSURE_EXISTS" ]; then
    ensure_exist ${ENSURE_EXISTS[@]}
fi

if [ "$ENSURE_NONEXISTS" ]; then
    ensure_nonexist ${ENSURE_NONEXISTS[@]}
fi

if [ "$LINK" ]; then
    create_links ${LINK[@]}
fi

