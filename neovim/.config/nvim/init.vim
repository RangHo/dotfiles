" Smart Indentation (tab to 4 spaces)
set tabstop=4
set shiftwidth=4
set expandtab

" -------- VIM-PLUG --------
if empty(glob('~/.config/nvim/autoload/plug.vim'))
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')

" vim-airline status/tabline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_powerline_fonts=1

" Language Server Protocol client for NeoVim
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
\ }

" Multy-entry selection UI
Plug 'junegunn/fzf'
Plug 'Shougo/deoplete.nvim', {
    \ 'do': ':UpdateRemotePlugins'
\ }

" NERDTree File Manager
Plug 'scrooloose/nerdtree'

call plug#end()
" --------------------------

