" -----------
" Preparation
" -----------

" Vim/NeoVim specific directories
if has('nvim')
    let s:config_directory   = stdpath('config') . '/'
    let s:data_directory     = stdpath('data') . '/'
    let s:autoload_directory = stdpath('data') . '/site/autoload/'
else
    let s:config_directory   = '~/.vim/'
    let s:data_directory     = '~/.vim/'
    let s:autoload_directory = '~/.vim/autoload/'
endif

" POSIX the hell out of fish
if &shell =~# 'fish$'
    set shell=sh
endif

" --------
" Plug-ins
" --------

" Install vim-plug if not already installed
if empty(glob(s:autoload_directory . 'plug.vim'))
    execute '! curl -fLo ' . s:autoload_directory . 'plug.vim --create-dirs '
        \ . 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(s:data_directory . 'plugged')

" Configuration helpers
Plug 'editorconfig/editorconfig-vim'

" Apperance-related
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'

" Editing helpers
Plug 'mattn/emmet-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'hrsh7th/vim-vsnip'

" File type support
Plug 'dag/vim-fish'
Plug 'neoclide/jsonc.vim'

" Language Server Protocol support
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'hrsh7th/vim-vsnip-integ'

" Other features
Plug 'rhysd/vim-clang-format'
Plug 'tpope/vim-eunuch'
Plug 'scrooloose/nerdtree'

call plug#end()

" ---------------------
" Editor Configurations
" ---------------------

" Enable features depending on file types
filetype plugin on
filetype indent on

" Read new external changes automatically
set autoread
autocmd FocusGained,FileChangedShell,BufEnter * checktime

" Set indentation rules
set tabstop=8
set expandtab
set shiftwidth=4
set shiftround
set softtabstop=4
set smartindent

" Let language server handle folding
set foldmethod=expr
    \ foldexpr=lsp#ui#vim#folding#foldexpr()
    \ foldtext=lsp#ui#vim#folding#foldtext()
let g:lsp_fold_enable = 0

" Show line numbers
set number relativenumber
augroup relativenumber_on_insert
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu | endif
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave * if &nu | set nornu | endif
augroup END

" Use mouse in all modes
set mouse=a

" -----------
" Keybindings
" -----------

" Map <leader>w as Ctrl-W (for use in web terminals)
nnoremap <leader>w <C-w>

" Tab completion (asyncomplete.vim)
imap <C-Space> <Plug>(asyncomplete_force_refresh)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <CR> pumvisible() ? asyncomplete#close_popup() . "\<CR>" : "\<CR>"

