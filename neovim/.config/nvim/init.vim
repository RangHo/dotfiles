" Smart Indentation (tab to 4 spaces)
set tabstop=4
set shiftwidth=4
set expandtab

" Hybrid line number
set number relativenumber

" Toggle hybrid number when necessary
augroup numbering
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu | endif
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave * if &nu | set nornu | endif
augroup END

" -------- VIM-PLUG --------
if empty(glob('~/.config/nvim/autoload/plug.vim'))
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')

" vim-airline status/tabline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'

" Language Server Protocol client for NeoVim
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
\ }

set hidden
let g:LanguageClient_serverCommands = {
    \ 'c': ['/bin/clangd', '-background-index'],
    \ 'cpp': ['/bin/clangd', '-background-index'],
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'python': ['/bin/pyls'],
\ }

autocmd FileType c,cpp setlocal signcolumn=yes

" Multy-entry selection UI
Plug 'junegunn/fzf'
Plug 'Shougo/deoplete.nvim', {
    \ 'do': ':UpdateRemotePlugins'
\ }
" let g:deoplete#enable_at_startup = 1

" NERDTree File Manager
Plug 'scrooloose/nerdtree'

call plug#end()
" --------------------------

