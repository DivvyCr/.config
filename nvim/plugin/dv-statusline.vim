if exists("g:dv_statusline")
  finish
endif
let g:dv_statusline=1

augroup dv_statusline
  au!
  au WinLeave,BufLeave * setlocal statusline=%!DvStatusLineInactive()
  au WinEnter,BufEnter * setlocal statusline=%!DvStatusLine()
augroup END

function! DvStatusLine()
  let line = "%#DVStatusDefault#"
  let line ..= DvMode()
  let line ..= DvPath(1)
  let line ..= DvFlags()
  let line ..= "%="
  let line ..= DvLineColumn()
  return line
endfunction

function! DvStatusLineInactive()
  let line = "%#StatusLineNC#"
  let line ..= DvWinNum()
  let line ..= DvPath(0)
  return line
endfunction

function! DvWinNum()
  let num = win_id2win(g:statusline_winid)
  return " %#DVPathBaseNC#[%#DVPathFileNC#" .. num .. "%#DVPathBaseNC#]"
endfunction

function! DvMode()
  let current_mode = mode()

  let mode_color = ""
  if current_mode == "n" 
    let mode_color = "%#DVStatusNormal#"
  elseif (current_mode == "i" || current_mode == "ic")
    let mode_color = "%#DVStatusInsert#"
  elseif (current_mode == "v" || current_mode == "V" || current_mode == "")
    let mode_color = "%#DVStatusVisual#"
  elseif current_mode == "R" 
    let mode_color = "%#DVStatusReplace#"
  elseif current_mode == "c" 
    let mode_color = "%#DVStatusCommand#"
  elseif current_mode == "t" 
    let mode_color = "%#DVStatusTerminal#" 
  endif

  return mode_color .. " "
endfunction

function! DvPath(is_active)
  let ret = ""
  let file_name = "%t"
  let file_path = fnamemodify(nvim_buf_get_name(nvim_win_get_buf(g:statusline_winid)), ":p:h")

  let git_dir_path = system("git -C " .. file_path .. " rev-parse --show-toplevel")
  if v:shell_error == 0
    let git_dir = fnamemodify(trim(git_dir_path), ":t")
    let git_file_path = system("git -C " .. file_path .. " rev-parse --show-prefix | tr -d '\n'")
    let git_branch = system("git -C " .. file_path .. " rev-parse --abbrev-ref HEAD")

    if a:is_active == 0
      let ret = "%#DVPathBaseNC# " .. trim(git_branch) .. ":/"
      return ret .. "%#DVPathMainNC#" .. git_dir .. "/" .. git_file_path .. "%#DVPathFileNC#" .. file_name
    else
      let ret = "%#DVPathBase# " .. trim(git_branch) .. ":/"
      return ret .. "%#DVPathMain#" .. git_dir .. "/" .. git_file_path .. "%#DVPathFile#" .. file_name
    endif
  endif

  let file_path = fnamemodify(nvim_buf_get_name(nvim_win_get_buf(g:statusline_winid)), ":.:h")
  if a:is_active == 0
    let ret ..= "%#DVPathMainNC# " .. file_path .. "/%#DVPathFileNC#" .. file_name
  else
    let ret ..= "%#DVPathMain# " .. file_path .. "/%#DVPathFile#" .. file_name
  endif

  return ret
endfunction

function! DvFlags()
  if (&readonly)
    return " %#DVStatusDefault#"
  endif
  return (&modified ? " %#DVStatusRed#󰉉" : " %#DVStatusHidden#󰉉")
endfunction

function! DvLineColumn()
  return "%#DVStatusDefault#%l %c "
endfunction

function! DvGitStatus()
  let ret = ""
  let branch_name = gitbranch#name()
  if branch_name != ""
    let ret ..= "%#DVStatusDefault# "
    let ret ..= gitbranch#name()
  endif
  return ret
endfunction
