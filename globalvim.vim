" .vimrc
" Author: Idzard Hoekstra
" only use <leader><space> in inoremap and no other leader commands!!

" 1. Preamble {{{
filetype off
set nocompatible
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
" Do not remove any of the above!!
" }}}

" 2. Vim Plugins {{{
" Fancy statusbar + themes
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" File tree sidebar
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
" git show additions compared to staged
Plugin 'airblade/vim-gitgutter'

" Ctags sidebar
Plugin 'majutsushi/tagbar'

" closing brackets
Plugin 'cohama/lexima.vim'

" quickrun
Plugin 'thinca/vim-quickrun'

" Git plugin
Plugin 'tpope/vim-fugitive'

" Fuzzy finder
Plugin 'ctrlpvim/ctrlp.vim'

" Syntax checking
Plugin 'scrooloose/syntastic'
" Cpp completion
" Plugin 'Valloric/YouCompleteMe'
source /usr/share/vim/vimfiles/plugin/youcompleteme.vim

" Latex Preview
Plugin 'xuhdev/vim-latex-live-preview'

" Ack integration
Plugin 'mileszs/ack.vim'

" Comment plugin
Plugin 'tpope/vim-commentary'

" fancy icons
Plugin 'ryanoasis/vim-devicons'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
" do not change the above two lines!
" }}}

" 3. Plugin Settings {{{
" Airline settings {{{
" let g:airline_theme='behelit'
let g:airline_theme='luna'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFolderExtensionPatternMatching = 1

set ttimeoutlen=10
set laststatus=2
set noshowmode
" }}}

" NERDTree settings {{{
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 0
let NERDTreeShowLineNumbers=1
autocmd FileType nerdtree setlocal relativenumber
autocmd FileType nerdtree setlocal nolist

" space config
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

let g:WebDevIconsUnicodeDecorateFolderNodes = 0
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['tex'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['pdf'] = ''

" color config
let g:NERDTreeDisablePatternMatchHighlight = 0

let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1

let s:lightGreen = '31B53E'
let s:darkOrange = 'F16529'

let g:NERDTreeExtensionHighlightColor = {} " needed
let g:NERDTreeExtensionHighlightColor['tex'] = s:lightGreen
let g:NERDTreeExtensionHighlightColor['pdf'] = s:darkOrange
" }}}

" GitGutter settings {{{
let g:gitgutter_map_keys = 0
" }}}

" syntastic settings {{{
let g:syntastic_cpp_compiler = 'g++'
let g:syntastic_cpp_compiler_options = '-std=c++17 -Wall -Wextra -Wunused'
" }}}

" youcompleteme {{{
let g:ycm_python_binary_path    = '/usr/bin/python2'
let g:ycm_server_python_interpreter = '/usr/bin/python2'
let g:ycm_global_ycm_extra_conf = '/usr/share/vim/vimfiles/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_extra_conf_globlist = ['~/dev/*','!~/*']
" }}}

"}}}

" 4. Vim Settings {{{
set nowrap
set linebreak
set showcmd
syntax on
set cursorline
set incsearch
set hlsearch
set ignorecase
set smartcase
set lazyredraw
set encoding=utf-8
"set textwidth=80
set list
set listchars=trail:ᚲ,eol:¬,extends:⟩,precedes:⟨,tab:►\ 
set t_Co=256
set splitright
set splitbelow
set shell=/bin/zsh

set foldlevelstart=0
set foldmethod=marker

" ctags optimization
"set autochdir
set tags=tags;

" language
set spelllang=en_gb

" cpp folds
"augroup ft_cpp
"  au!
"  au FileType cpp setlocal foldmethod=syntax
"  au FileType cpp setlocal ts=4 sts=4 sw=4 noexpandtab
"  au FileType cpp setlocal foldlevelstart=3
"augroup END

" c folds
"augroup ft_c
"  au!
"  au FileType c setlocal foldmethod=syntax
"  au FileType c setlocal ts=4 sts=4 sw=4 noexpandtab
"  au FileType c setlocal foldlevelstart=3
"augroup END

" h folds
"augroup ft_h
"  au!
"  au FileType h setlocal foldmethod=syntax
"  au FileType h setlocal ts=4 sts=4 sw=4 noexpandtab
"  au FileType h setlocal foldlevelstart=3
"augroup END

colorscheme elflord
"desert

hi Normal ctermbg=NONE

let g:tex_flavor = 'tex'

set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.hi,*.dyn_hi,*.dyn_o,*.txt,*.zip,*.pdf
" }}}

" 5. Key remappings {{{

" when searching focus in middle of screen
nnoremap n nzzzv
nnoremap N Nzzzv

" when searching focus in middle of screen
nnoremap g; g;zz
nnoremap g, g,zz

" simple reformating of a paragraph (e.g. in latex)
nnoremap Q gqip

" tab = 2 spaces
" set expandtab     "turn tab into spaces

set tabstop=2     "display tab as 2 spaces
set shiftwidth=2  "insert two spaces for tab
set softtabstop=2 "insert two spaces for tab

" disable arrow keys and assign to resizing
nnoremap <up>    5<c-w>+
nnoremap <down>  5<c-w>-
nnoremap <left>  5<c-w><
nnoremap <right> 5<c-w>>

inoremap <up>    5<c-w>+
inoremap <down>  5<c-w>-
inoremap <left>  5<c-w><
inoremap <right> 5<c-w>>
" }}}

" 6. F Key Mappings {{{

" F1 = help
set pastetoggle=<F2> "toggle paste mode with F2 key
nnoremap <F3> :noh<cr>
nnoremap <F4> :%s/\r//g<cr>

nnoremap <F5> :let _s=@/<bar>:%s/\s\+$//e<bar>:let @/=_s<bar><cr>
" F6
" F7
" F8

nnoremap <F9> :copen<cr>
nnoremap <F10> :shell<cr>
" F11
" F12
" }}}

" 7. Ctrl Mappings {{{

"insert mode completion
inoremap <c-f> <c-x><c-f>
" inoremap <c-]> <c-x><c-]> (First need tags file)
inoremap <c-l> <c-x><c-l>
" toggle line numbers relative or absolute
nnoremap <C-n> :call NumberToggle()<cr>
" make new enter appear beneath the current one in edit mode
inoremap <c-cr> <c-o>o

" }}}

" 8. Ex Mappings {{{
" enable sudo write if forgotten
cnoremap w!! w !sudo tee > /dev/null %

"}}}

" 9. Leader Commands {{{
let mapleader="\<space>"

" Single character {{{
" do not use: e s
" surround a word with either single or double quotes
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel
"insert a space from normal mode
nnoremap <leader><space> i<space><esc>
" move across windows without having to press c-w
nnoremap <leader>h <c-w>h
nnoremap <leader>j <c-w>j
nnoremap <leader>k <c-w>k
nnoremap <leader>l <c-w>l
" open file also in a new tab (editing a file in two places) (split)
nnoremap <leader>v <c-w>v
" write a file in two key strokes
nnoremap <leader>w :w<cr>
" execute a mkreport in the folder
nnoremap <leader>b :w <bar> make<cr><cr><cr>
" use 0 to go first non-blank character and <leader>0 to go to start of line
nnoremap 0 ^
nnoremap <leader>0 0
"switch between wrap and nowrap
nnoremap <silent> <leader>W :set wrap!<cr>
"switch folds
nnoremap <leader>a za
nnoremap <leader>z zMzvzz
" highlight interesting words without searching
nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
" toggle invisible characters
nnoremap <silent> <leader>i :set list!<cr>
" toggle NERDTree
nnoremap <silent> <leader>f :NERDTreeToggle<cr>
" find current file in NERDTree
nnoremap <silent> <leader>F :NERDTreeFind<cr>
" open tagbar
nnoremap <silent> <leader>t :TagbarToggle<cr>
" quickrun
nnoremap <silent> <leader>r :QuickRun<cr>
" }}}

" Double character {{{
" do not use: ' " a b fF h i j k l r t v wW z 0 1 2 3 4 5 6
" edit vimrc
nnoremap <silent> <leader>ev :vsplit ~/dotfiles/globalvim.vim<cr>
nnoremap <silent> <leader>sv :source $MYVIMRC<cr>
" edit bashrc
nnoremap <silent> <leader>ez :vsplit ~/.zshrc<cr>
nnoremap <silent> <leader>sz :! source ~/.zshrc<cr><cr>
" TEMP: edit configuration package file
nnoremap <silent> <leader>ep :vsplit ~/Documents/studie/master/afstudeeropdracht/report/include/settings.tex<cr>
" make session
nnoremap <leader>sm :mksession ~/.vim/sessions/
" update session
nnoremap <leader>su :mksession! ~/.vim/sesions/
" load session
nnoremap <leader>sl :source ~/.vim/sessions/
" toggle spell check
nnoremap <leader>ss :setlocal spell!<cr>
" }}}
" }}}

" 10. Line Settings {{{
set number         "show line numbers
set relativenumber "show linenumbers relative to current line

function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

au FocusLost * :set norelativenumber
au FocusGained * :set relativenumber

" lines that exceed length 80 are marked
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn','\%82v',100)

" }}}

" 11. Highlight function {{{
function! HiInterestingWord(n)
  " save the location
  normal! mz
  " yank the current word into the z register
  normal! "zyiw
  " calculate an arbitrary match ID (hope nobody else is using it)
  let mid = 86750 + a:n
  " clear existing matches, but don't worry if they don't exist
  silent! call matchdelete(mid)
  " construct a literal pattern that has to match at boundaries
  let pat = '\V\<' . escape(@z, '\') . '\>'
  " actually match the words
  call matchadd("InterestingWord" . a:n, pat, 1, mid)
  " move back to the original location
  normal! `z
endfunction

" default Highlights
hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

" }}}

" 12. Latex bindings {{{
" find next <++> for custom Latex editing and enable spell check
autocmd FileType tex,plaintex inoremap <leader><space> <esc>/<++><cr>"_c4l
autocmd FileType tex,plaintex setlocal spell
" {{{ Sections
" type ;sec to automatically make a section header
autocmd FileType tex,plaintex inoremap ;sec \section{}<cr><cr><++><esc>2k0f{a
" type ;ssec to automatically make a subsection header
autocmd FileType tex,plaintex inoremap ;ssec \subsection{}<cr><cr><++><esc>2k0f{a
" type ;sussec to automatically make a subsection header
autocmd FileType tex,plaintex inoremap ;susec \subsubsection{}<cr><cr><++><esc>2k0f{a
" }}}
" {{{ Lists
" ;lit -> list with itemize
autocmd FileType tex,plaintex inoremap ;lit \begin{itemize}<cr>\item<space><cr>\end{itemize}<cr><cr><++><esc>3kA
" ;len -> list with enumerate
autocmd FileType tex,plaintex inoremap ;len \begin{enumerate}<cr>\item<space><cr>\end{enumerate}<cr><cr><++><esc>3kA
" ;lt -> add item
autocmd FileType tex,plaintex inoremap ;lt \item<space>
" }}}
" {{{ Math
" type ;eqn to automatically make an equation environment
autocmd FileType tex,plaintex inoremap ;eqn \begin{equation}<cr><cr>\end{equation}<cr><cr><++><esc>3kcc
" type ;eqr to automatically make an equation with a reference
autocmd FileType tex,plaintex inoremap ;eqr \begin{equation}<cr>\label{eq:<++>}<cr>\end{equation}<cr><cr><++><esc>3k0i
" type ;al to automatically make a standard align environment
autocmd FileType tex,plaintex inoremap ;al \begin{align}<cr><cr>\end{align}<cr><++><esc>2kcc
" }}}
" {{{ Figures
" type ;fi to automatically make a standard figure environment
autocmd FileType tex,plaintex inoremap ;fi \begin{figure}[hb]<cr>\centering<cr>\includegraphics[width=\columnwidth]{./img/}<cr>\caption{<++>}<cr>\label{fig:<++>}<cr>\end{figure}<cr><cr><++><esc>5k02f/a
" type ;fsi to automatically make a standard subfigure environment
autocmd FileType tex,plaintex inoremap ;fsi \begin{figure}[hb]<cr>\centering<cr>\subfloat[\label{fig:<++>}]{\includegraphics[width=0.45\columnwidth]{./img/<++>}}<cr>\qquad<cr>\subfloat[<++>\label{fig:<++>}]{\includegraphics[width=0.45\columnwidth]{./img/<++>}}<cr>\label{fig:<++>}<cr>\end{figure}<cr><cr><++><esc>6k0f[a
" }}}
" {{{ References TODO: make them into concatenations of (name)ref
" type ;ref to automatically make a ref to a fig
autocmd FileType tex,plaintex inoremap ;ref figure~\ref{fig:}<++><esc>F:a
" type ;ree to automatically make a ref to an equation
autocmd FileType tex,plaintex inoremap ;ree equation~\eqref{eq:}<++><esc>F:a
" type ;res to automatically make a ref to a section
autocmd FileType tex,plaintex inoremap ;res section~\ref{sec:}<++><esc>F:a
" type ;rec to automatically make a ref to a chapter
autocmd FileType tex,plaintex inoremap ;rec chapter~\ref{sec:}<++><esc>F:a
" type ;la to automatically make a label
autocmd FileType tex,plaintex inoremap ;lab \label{}<++><esc>F{a
" }}}
" {{{ Markup
" type ;bf to automatically make boldface
autocmd FileType tex,plaintex inoremap ;bf \textbf{}<++><esc>F{a
" type ;it to automatically make italic
autocmd FileType tex,plaintex inoremap ;it \textit{}<++><esc>F{a
" type ;em to automatically make emphatic
autocmd FileType tex,plaintex inoremap ;em \emph{}<++><esc>F{a
" type ;un to automatically make underline
autocmd FileType tex,plaintex inoremap ;un \underline{}<++><esc>F{a
" }}}
" {{{ gantt
autocmd FileType tex,plaintex inoremap ;gn \gantt
autocmd FileType tex,plaintex inoremap ;gr group[progress=00,progress label text={}]{}{<++>}{<++>}<++><esc>3F{a
autocmd FileType tex,plaintex inoremap ;ba bar[progress=00,progress label text={}]{}{<++>}{<++>}<++><esc>3F{a
autocmd FileType tex,plaintex inoremap ;mi milestone{}{<++>}<++><esc>2F{a
autocmd FileType tex,plaintex inoremap ;gl link{}{<++>}<cr><++><esc>k$2F{a
" }}}
" {{{ beamer
autocmd FileType tex,plaintex inoremap ;fr \begin{frame}<cr>\frametitle{}<cr>\end{frame}<esc>k0f{a
autocmd FileType tex,plaintex inoremap ;co \begin{column}{.5\textwidth}<cr><cr>\end{column}<esc>k0cc
" }}}
" {{{ TEMP ros
autocmd FileType tex,plaintex inoremap ;ros \ac{ROS}
" }}}
" }}}

" 13. Cpp bindings {{{
" find next <++> for custom cpp editing
autocmd FileType h,cpp inoremap <leader><space> <esc>/<++><cr>"_c4l
" make ;class class<space><cr>{<cr>public:<cr>private:<cr><++><cr>};<esc>5kA
autocmd FileType h,cpp inoremap <silent> ;class <esc>:call MakeClass()<cr>i
" def MakeClass {{{
function! MakeClass()
  call inputsave()
  " ask the user for class name and base class name
  let classname = input('Enter Class Name: ')
  " TODO: give a boolean (y/n) question to check derived/virtual
  " TODO: check if first letter is capital
  call inputrestore()
  call setline('.','class ' . classname)
  execute "normal! o{"
  execute "normal! opublic:"
  execute "normal! o"
  call setline('.', '		' . classname . '();')
  execute "normal! o"
  call setline('.', '		virtual ~' . classname . '();')
  execute "normal! o"
  execute "normal! oprivate:"
  execute "normal! o"
  execute "normal! o};"
  execute "normal! k"
endfunction
" }}}
" make ;std::
autocmd FileType h,cpp inoremap ;s std::
autocmd FileType h,cpp cnoremap ;s std::
" make ;vector
autocmd FileType h,cpp inoremap ;v vector<><space><++><esc>Fvf<a
autocmd FileType h,cpp cnoremap ;v vector<
" make ;array
autocmd FileType h,cpp inoremap ;a array<,<++>><space><++><esc>Faf<a
autocmd FileType h,cpp cnoremap ;a array<
" include standard header
autocmd FileType h,cpp inoremap ;incs #include<space><><++><esc>0f<a
" include own header
autocmd FileType h,cpp inoremap ;inc #include<space>""<++><esc>0f"a
" include iostream
autocmd FileType h,cpp inoremap ;iinc #include<space><iostream><cr>
" make a private data member and automatically make getters and setters
autocmd FileType h,cpp inoremap <silent> ;mem <esc>:call MakeMember()<cr>i
" def MakeMember {{{
function! MakeMember()
  call inputsave()
  " ask the user for input
  let type = input('Enter Type: ')
  let name = input('Enter Name: ')
  " TODO: add question to add it to list initializer
  call inputrestore()
  call setline('.', '		' . type . ' m_' . name . ';')
  execute "normal! /\vprivate:"
  let priv = search('private:', 'b')
  " Create new line for getter and setter
  execute "normal! O"
  " create get function
  call setline('.\', '		' . type . ' get' . name . '() const;')
  " find matching get
  let caseget = search('\v\ get')
  " change case and create next line
  execute "normal! 4l~o"
  " create set function
  call setline('.\', '		void set' . name . '(' . type . ');')
  " find matching set
  let caseset = search('\v\ set')
  " change capitalisation
  execute "normal! 4l~o"
  " set cursor again to the original position
  let findorig = search(name)
  execute "normal! o"
endfunction
" }}}
" make standard header ifndef with ;header
autocmd FileType h,cpp inoremap ;header <esc>:call MakeHeader()<cr>i
" {{{ def MakeHeader
function! MakeHeader()
  let filename = expand('%:t')
  execute "normal! gg"
  call setline('.\', filename)
  execute "normal! gUU0f.r_I#ifndef "
  execute "normal! o"
  call setline('.\', filename)
  execute "normal! gUU0f.r_I#define "
  execute "normal! o#endif"
  execute "normal! O"
  execute "normal! o"
endfunction
" }}}
" make ;const
autocmd FileType h,cpp inoremap ;co const
" make ;ros
autocmd FileType h,cpp inoremap ;r ros::
" }}}

" 14. Macros {{{

" }}}

" 15. Abbreviations {{{
abbr teh the
abbr PIR PIRATE
" }}}

" 16. Movement mappings {{{
" edit in next ()
onoremap in( :<c-u>normal! f(vi(<cr>
" edit in prev ()
onoremap inl( :<c-u>normal! F)vi(<cr>
" edit around next ()
onoremap an( :<c-u>normal! f(va(<cr>
" edit around prev ()
onoremap anl( :<c-u>normal! F)va(<cr>
" edit in next ()
onoremap in{ :<c-u>normal! f{vi{<cr>
" edit in prev ()
onoremap inl{ :<c-u>normal! F}vi{<cr>
" }}}

" {{{ 17. Haskell bindings
autocmd FileType haskell set expandtab

" }}}
