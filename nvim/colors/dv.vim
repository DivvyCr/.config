"
" SETUP:
"

set background=dark
hi clear

if exists("syntax on")
  syntax reset
endif

let g:colors_name = "dv"

"
" COLOURS (use :Inspect command to find highlight group of character)
"

" Fundamentals:
hi Normal guibg=#1b1b1b guifg=#dfdfdf
hi LineNr guibg=NONE guifg=#666666

set cursorline
hi CursorLine guibg=NONE guifg=NONE
hi CursorLineNr guibg=NONE guifg=#eeeeee

set colorcolumn=""
hi ColorColumn guibg=#333333 guifg=NONE
hi SignColumn guibg=#1b1b1b guifg=NONE

hi Visual guifg=NONE guibg=#444444
hi VisualNOS guifg=NONE guibg=#333333

" Statusline:
hi StatusLine guibg=#121212 guifg=#dcdcdc gui=NONE
hi StatusLineNC guibg=#222222 guifg=#1b1b1b
hi DVStatusDefault guibg=#121212 guifg=#dcdcdc
hi DVStatusHidden guibg=#121212 guifg=#121212
hi DVStatusNormal guibg=#3676e8
hi DVStatusInsert guibg=#85ea4d
hi DVStatusVisual guibg=#eab24d 
hi DVStatusReplace guibg=#ea644d
hi DVStatusCommand guibg=#121212
hi DVStatusTerminal guibg=#dcdcdc
hi DVStatusRed guibg=#121212 guifg=#ff3333
hi DVStatusGreen guibg=#121212 guifg=#85ea4d
hi DVPathBase guibg=#121212 guifg=#2f71e7
hi DVPathBaseNC guifg=#666666
hi DVPathMain guibg=#121212 guifg=#5289eb
hi DVPathMainNC guifg=#a0a0a0
hi DVPathFile guibg=#121212 guifg=#7ba5f0
hi DVPathFileNC guifg=#dcdcdc

" Tabline:
hi TabLine guibg=#121212 guifg=#a0a0a0 gui=none
hi TabLineFill guibg=#121212 guifg=#121212
hi TabLineSel guibg=#121212 guifg=#dcdcdc gui=bold

" Search:
hi! link Search Visual
hi IncSearch guifg=NONE guibg=NONE gui=inverse

" Syntax:
hi Matchparen guifg=#ff8ad5 guibg=NONE

hi Constant guifg=#5289eb
hi String guifg=#94ea94
hi Character guifg=#94ea94 gui=italic
hi! link Number Identifier
hi! link Float Number
hi! link Boolean Keyword

hi Identifier guifg=#99b9f3
hi Function guifg=#6495ed gui=italic
hi Statement guifg=#5289eb gui=bold
hi Keyword guifg=#5289eb 
hi Type guifg=#ebb452 gui=bold
hi StorageClass guifg=#ebb452 gui=NONE

hi Special guifg=#ff8ad5
hi Tag guifg=#ff8ad5 gui=underline
hi Delimiter guifg=#dcdcdc gui=bold
hi! link Operator Delimiter

hi PreProc guifg=#87adf1
hi Error guibg=NONE guifg=#ff3533 gui=bold
hi Comment guifg=#575d69

hi! link DiagnosticError Error
hi! link DiagnosticWarn Todo
hi DiagnosticUnderlineError guibg=#5a0100 guisp=#ff3533
hi DiagnosticUnderlineWarn guibg=#4c391a guisp=#ffcc34
hi DiagnosticUnderlineHint guibg=#193a19 guisp=#4dff4e
"hi DiagnosticUnderlineInfo guibg=NONE gui=underdotted guisp=#dcdcdc
"hi DiagnosticUnderlineOk guibg=NONE gui=underdotted guisp=#4dff4e

" Syntax (TreeSitter and/or LSP):
hi! link @text.literal String

" Diff:
hi DiffAdd guibg=#193a19 guifg=NONE
hi DiffChange guibg=NONE guifg=NONE
hi DiffDelete guibg=#5a0100 guifg=NONE
hi DiffText guibg=#4c391a guifg=NONE

" Spell:
hi SpellBad guisp=#ff3533
hi SpellCap guisp=#dcdcdc
hi SpellRare guisp=#ff71cc
hi SpellLocal guisp=#ffcc34

" Text format:
hi Title guibg=NONE guifg=#5289eb
hi NonText guibg=NONE guifg=#333333
hi SpecialKey guibg=NONE guifg=#ff8ad5 gui=bold
hi Todo guibg=NONE guifg=#ffcc34
hi! link Directory Identifier

" Status messages:
hi MoreMsg guibg=#1b1b1b guifg=#dfdfdf gui=inverse
hi! link WarningMsg Todo
hi! link ErrorMsg Error
hi! link Question MoreMsg

" Pmenu:
hi Pmenu guifg=#dcdcdc guibg=#282828
hi PmenuSel guifg=#dcdcdc guibg=#121212 gui=bold

" Misc:
hi WinSeparator guibg=#1b1b1b guifg=#1b1b1b
