if exists("g:dv_tabline")
  finish
endif
let g:dv_tabline=1

augroup dv_tabline
  au!
  au WinEnter,BufEnter * setlocal tabline=%!DvTabLine()
augroup END

function! DvTabLine()
  let tabs = "%#DVStatusDefault#  "

  for i in range(tabpagenr('$'))
    let t:dv_tab_label=""
    call DvTabStatus(i)

    let tab_name = gettabvar(i+1, "dv_tab_name")
    if tab_name != ""
      let t:dv_tab_label ..= tab_name
    else
      call DvTabAutoName(i)
    endif
    
    call DvTabWindows(i)

    " Append tab to TabLine:
    let tabs ..= t:dv_tab_label .. ' '
  endfor
  let tabs ..= '%#TabLineFill#%T'

  return tabs
endfunction

function! DvTabStatus(i)
  if a:i+1 == tabpagenr()
    let t:dv_tab_label ..= (a:i+1 == tabpagenr() ? '%#TabLineSel#  ' : '%#TabLine#  ')
    return
  endif

  for buf in tabpagebuflist(a:i+1)
    if getbufvar(buf, '&modified')
      let t:dv_tab_label ..= '%#DVStatusRed# 󰉉 '
      return
    endif
  endfor
  let t:dv_tab_label ..= (a:i+1 == tabpagenr() ? '%#TabLineSel#  ' : '%#TabLine#  ')
endfunction

function! DvTabAutoName(i)
  let curwin = tabpagewinnr(a:i+1)
  let buflist = tabpagebuflist(a:i+1)
  let filename = fnamemodify(bufname(buflist[curwin-1]), ':t')
  let t:dv_tab_label ..= (filename == '' ? '[New]' : filename)
endfunction

function! DvTabWindows(i)
  let winnr = tabpagewinnr(a:i+1, '$')
  if winnr > 1
      let t:dv_tab_label ..= ' (+' .. (winnr-1) .. ')'
  endif
endfunction

function! DvTabEditName()
  let t:dv_tab_name = input("Rename tab: ")
  setlocal tabline=%!DvTabLine()
endfunction
