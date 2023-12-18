" lua << EOF
" vim.lsp.start({ name = 'Clangd', cmd = {'clangd-17'}, root_dir = vim.fs.dirname(vim.fs.find({'.git', 'Makefile'}, {path=vim.api.nvim_buf_get_name(0), upward=true})[1]) })
" EOF

let s:dv_cpp_cmd_stem="!g++ % -o out && ./out"

function DvCppRun()
  write
  exe s:dv_cpp_cmd_stem
endfunction

function DvCppRunWithArg()
  write
  exe s:dv_cpp_cmd_stem . " " . input("Arguments: ")
endfunction

let b:dv_default_input=""

function RunAndLogCpp()
  write
  let file = input("Input file: ", b:dv_default_input, "file")
  if filereadable(file)
    let b:dv_default_input=file
    sil exe s:dv_cpp_cmd_stem . " < " . file . " > %:r.log"
    let log = expand("%:r") . ".log"
    if bufwinnr(log) < 0
      exe "vne" log
    else
      exe "sb" log
    endif
    exe "norm p"
  else
    echo " Could not read file: " . file
  endif
endfunction

function RunAndLogCppFromClipboard()
  write
  sil exe "!g++ % -o out && powershell.exe Get-Clipboard | sed 's/\r$//' | ./out > %:r.log"
  let log = expand("%:r") . ".log"
  if bufwinnr(log) < 0
    exe "vne" log
  else
    exe "sb" log
  endif
  exe "norm p"
endfunction

function CopyLastLogLine()
  let log = expand("%:r") . ".log"
  exe "sb" log " | norm GyiWp"
endfunction

nnoremap <F5> :call RunAndLogCppFromClipboard()<CR>
nnoremap <F6> :call CopyLastLogLine()<CR>
nnoremap <F10> :call RunAndLogCpp()<CR>

lua << EOF

local function GetRoot()
  local filepath = vim.fn.expand("%:p")
  local projectfiles = vim.fs.find({'.git', 'Makefile'}, { upward = true, path=filepath })
  if projectfiles == {} then
    return vim.fs.dirname(filepath)
  else
    return vim.fs.dirname(projectfiles[1])
  end
end

--vim.lsp.start({
--  name = 'cpp-server',
--  cmd = {'clangd-17'}, 
--  root_dir = GetRoot(),
--})

vim.diagnostic.config({
  virtual_text = false,
})

EOF
