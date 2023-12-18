lua << EOF
require("nvim-treesitter.configs").setup({
    ensure_installed = { "vim", "lua", "c", "cpp", "json", "html" },
    sync_install = false,
    auto_install = true,
    highlight = {
      enable = true,
      disable = { "latex" }
    },
})
EOF
