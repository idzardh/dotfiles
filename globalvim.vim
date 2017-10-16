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
" Fancy statusbar + themes {{{
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Nord theme
Plugin 'arcticicestudio/nord-vim'
" }}}
" File tree exploration with git and highlighting enabled {{{
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
" }}}
" Git helpers and plugins {{{
Plugin 'airblade/vim-gitgutter'
" Git plugin
Plugin 'tpope/vim-fugitive'
" }}}
" Tpope helpers for projects and editing {{{
" vim projectionist
Plugin 'tpope/vim-projectionist'
" Surround with ' " or (
Plugin 'tpope/vim-surround'
" Repeat
Plugin 'tpope/vim-repeat'
" extra mappings?
Plugin 'tpope/vim-unimpaired'
" closing brackets
Plugin 'cohama/lexima.vim'
" }}}
" Documentation and tags {{{
Plugin 'KabbAmine/zeavim.vim'
" Ctags sidebar
Plugin 'majutsushi/tagbar'
" }}}
" Syntax checking {{{
Plugin 'scrooloose/syntastic'
" Cpp completion
source /usr/share/vim/vimfiles/plugin/youcompleteme.vim
" Comment plugin
Plugin 'tpope/vim-commentary'
" }}}
" Other {{{
Plugin 'christoomey/vim-tmux-navigator'
" pdf viewer for latex
Plugin 'xuhdev/vim-latex-live-preview'
" personal wiki
Plugin 'vimwiki/vimwiki'
" fancy icons
Plugin 'ryanoasis/vim-devicons'
" }}}
" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
" do not change the above two lines!
" }}}
" 3. Plugin Settings {{{
" Airline settings {{{
let g:airline_theme='nord'
let g:airline#extensions#tabline#enabled = 0
let g:airline_powerline_fonts = 1

let g:WebDevIconsUnicodeDecorateFolderNodes = 1
let g:DevIconsEnableFolderExtensionPatternMatching = 1

set ttimeoutlen=10
set laststatus=2
set noshowmode
" }}}
" NERDTree settings {{{
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
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

let s:lightGreen = 'AEC6CF'
let s:darkOrange = '88BE95'

let g:NERDTreeExtensionHighlightColor = {} " needed
let g:NERDTreeExtensionHighlightColor['tex'] = s:lightGreen
let g:NERDTreeExtensionHighlightColor['pdf'] = s:darkOrange

" let nerdtree use netrw
let NERDTreeHijackNetrw=1
" }}}
" vim-projectionist {{{

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
" latex preview {{{
let g:livepreview_previewer = 'evince'
" }}}
" vim wiki {{{
let g:vimwiki_list = [{'path': '~/Dropbox/my_wiki/', 'path_html': '~/Dropbox/my_wiki/HTML', 'syntax': 'markdown'}]
" }}}
"}}}
" 4. Vim Settings {{{
set nowrap
set linebreak
set showcmd
syntax on
set incsearch
set hlsearch
set ignorecase
set smartcase
set lazyredraw
set encoding=utf-8
set listchars=trail:ᚲ,eol:¬,extends:⟩,precedes:⟨,tab:►\ 
set t_Co=256
set splitright
set splitbelow
set shell=/bin/zsh
set foldlevelstart=0
set foldmethod=marker
set tags=tags;

" language
set spelllang=en_gb

" cpp include path (ros)
set path+=../include/

colorscheme slate
hi Normal ctermbg=NONE
let g:tex_flavor = 'tex'
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.hi,*.dyn_hi,*.dyn_o,*.zip,*.pdf
set wildcharm=<C-z>
set wildmode=longest,full
set wildignorecase

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

" list buffers and enable easy switching
nnoremap gb :ls<cr>:b<space>

set tabstop=2     "display tab as 2 spaces
set shiftwidth=2  "insert two spaces for tab
set softtabstop=2 "insert two spaces for tab

" use pageup and pagedown for something useful
nnoremap <PageUp>   :bprevious<cr>
nnoremap <PageDown> :bnext<cr>

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
nnoremap <F6> i<c-r>=strftime("%Y-%m-%d")<cr><esc>
inoremap <F6> <c-r>=strftime("%Y-%m-%d")<cr>

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
" clang-format
" nnoremap <C-K> :pyf /usr/share/clang/clang-format.py<cr>
" vnoremap <C-K> :pyf /usr/share/clang/clang-format.py<cr>

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
" open file also in a new tab (editing a file in two places) (split)
nnoremap <leader>v <c-w>v
" Open buffer list more easily
nnoremap <silent> <leader>b :buffer <C-z><S-Tab>
nnoremap <silent> <leader>B :sbuffer <C-z><S-Tab>
" use 0 to go first non-blank character and <leader>0 to go to start of line
nnoremap 0 ^
nnoremap <leader>0 0
"switch between wrap and nowrap
nnoremap <silent> <leader>W :set wrap!<cr>
"switch folds
nnoremap <leader>a za
nnoremap <leader>A zMzvzz
" toggle invisible characters
nnoremap <silent> <leader>i :set list!<cr>
" open nerdtree in this window
nnoremap <silent> <leader>f :e .<cr>
" open nerdtree in vsp
nnoremap <silent> <leader>F :vsp .<cr>
" open tagbar
nnoremap <silent> <leader>t :TagbarToggle<cr>
" }}}
" Double character {{{
" do not use: ' " a b fF  i t v wW z
" write a file in two key strokes
nnoremap <leader>wf :w<cr>
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
set cursorline

highlight LineNr ctermfg=darkgrey
highlight CursorLineNr ctermfg=grey
highlight EndOfBuffer ctermfg=grey

augroup CursorLine
	au!
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END

function! NumberToggle()
	if(&relativenumber == 1)
		set norelativenumber
	else
		set relativenumber
	endif
endfunc

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" lines that exceed length 80 are marked
highlight ColorColumn ctermbg=darkgrey
call matchadd('ColorColumn','\%82v', 100)

" }}}
" 11. Latex bindings {{{
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
autocmd FileType tex,plaintex inoremap ;rec chapter~\ref{cha:}<++><esc>F:a
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
" 12. Cpp bindings {{{
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
" access specifier
autocmd FileType h,cpp inoremap ;; ::

" Doxygen templates
" File header
autocmd FileType h,cpp inoremap ;dofi /*!<cr>\file <cr><cr>\author <++><cr>\date <++><cr><cr><++><cr>/<esc>6kA
autocmd FileType h,cpp inoremap ;docl /*!<cr>\class <cr><cr>\brief <++><cr><cr><++><cr><cr>\author <++><cr>\date <++><cr>/<esc>8kA
autocmd FileType h,cpp inoremap ;dofu /*!<cr>\brief <cr><cr><++><cr><cr>\param[in] <++><cr>\param[out] <++><cr>\return <++><cr>\sa <++><cr>\note <++><cr>/<esc>9kA
" }}}
" 13. Macros {{{

" }}}
" 14. Abbreviations {{{
abbr teh the
abbr PIR PIRATE
" }}}
" 15. Movement mappings {{{
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
" {{{ 16. Haskell bindings
autocmd FileType haskell set expandtab

" }}}
" {{{ 17. XML bindings
autocmd FileType xml set expandtab
autocmd FileType xml inoremap ;co <!-- --><esc>F<space>a

" }}}
