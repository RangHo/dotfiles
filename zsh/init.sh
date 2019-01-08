# Install Oh-My-Zsh first then remove ~/.zshrc
# Or dotfiles repo will be mad about having unwanted stuff because of symlink
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
rm -rf ~/.zshrc

# Update submodules in case I forgot to do it
git submodule update
