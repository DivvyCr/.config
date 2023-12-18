lua << EOF
require("luasnip").config.set_config({
  enable_autosnippets = true,
  store_selection_keys = "<Tab>",
  update_events = "TextChanged,TextChangedI",
})

require("luasnip.loaders.from_lua").lazy_load({paths = "~/.config/nvim/snippets/"})
EOF

imap <silent><expr> <Tab> luasnip#expandable() ? '<Plug>luasnip-expand-snippet' : '<Tab>'

imap <silent><expr> <Tab> luasnip#jumpable(1) ? '<Plug>luasnip-jump-next' : '<Tab>'
smap <silent><expr> <Tab> luasnip#jumpable(1) ? '<Plug>luasnip-jump-next' : '<Tab>'

imap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
smap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
