local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta

return {
  s({ trig="for+", snippetType="autosnippet" },
    fmta([[
      for (int i = <>; i << <>; i++) {
        <>
      }<>
    ]], { i(1, "0"), i(2), i(3), i(0) })
  ),
  s({ trig="for-", snippetType="autosnippet" },
    fmta([[
      for (int i = <>; i >> <>; i--) {
        <>
      }<>
    ]], { i(1, "0"), i(2), i(3), i(0) })
  ),
  s({ trig="for*", snippetType="autosnippet" },
    fmta([[
      for (auto &<> : <>) {
        <>
      }<>
    ]], { i(1), i(2), i(3), i(0) })
  ),

  s({ trig="stdvec", snippetType="autosnippet" },
    fmt([[
      std::vector<{}> {}
    ]], { i(1), i(0) })
  ),
  s({ trig="stdmap", snippetType="autosnippet" },
    fmt([[
      std::map<{}> {}
    ]], { i(1), i(0) })
  ),

  s({ trig="cout<<", snippetType="autosnippet" },
    fmta([[
      std::cout <<<< <> <<<< std::endl;
    ]], { i(0) })
  )

}
