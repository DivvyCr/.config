" IMPORTANT: Symlink ~/.config/nvim/pack to ~/.local/share/nvim/site/pack

" Numbering:
set number
set relativenumber

" Patterns:
set ignorecase
set smartcase

" Indentation:
set expandtab
set shiftwidth=2
set tabstop=2

" Statusline:
set laststatus=2
set noshowmode

" Tabline:
set showtabline=2

" Misc:
set path+=**
set clipboard=unnamedplus
set lazyredraw
set termguicolors
colorscheme dv

"
" Mappings:
"

let mapleader=','
inoremap hh <Esc>
nnoremap k gk
nnoremap j gj
nnoremap Y y$

nnoremap <Leader>n :bnext<CR>
nnoremap <Leader>p :bprev<CR>

nnoremap <C-n> gt
nnoremap <C-p> gT
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tc :tabclose<CR>
nnoremap <Leader>te :call DvTabEditName()<CR>

nnoremap <Leader>o :only<CR>
nnoremap <Leader>\ :tcd %:p:h<CR>:pwd<CR>
nnoremap <silent> <Leader>v :edit $MYVIMRC<CR>
nnoremap <silent> <Leader>r :source $MYVIMRC<CR>:colorscheme dv<CR>
nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <Leader>i :Inspect<CR>

"
" Plugins:
"

" Jump to window with <Leader> followed by [1-9]
" (See: http://stackoverflow.com/a/6404246/151007)
let i = 1
while i <= 9
  exe 'nnoremap <silent> <Leader>'.i.' :'.i.'wincmd w<CR>'
  let i = i + 1
endwhile

" VimTeX (https://github.com/lervag/vimtex)
let g:vimtex_view_method='sioyek'
