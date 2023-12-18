lua << EOF

vim.lsp.start({
  name = 'py-server',
  cmd = {'pyright-langserver', "--stdio"}, 
  root_dir = vim.fs.dirname(vim.fn.expand("%:p")),
})

vim.diagnostic.config({
  virtual_text = false,
})

EOF
