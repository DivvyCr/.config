local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local extras = require("luasnip.extras")
local rep = extras.rep

--
-- Conditions:
--

local function in_math()
  return vim.api.nvim_eval("vimtex#syntax#in_mathzone()") == 1
end

local function in_comment()
  return vim.fn["vimtex#syntax#in_comment"]() == 1
end

--

local function env(name)
  local is_inside = vim.fn["vimtex#env#is_inside"](name)
  return (is_inside[1] > 0 and is_inside[2] > 0)
end

local function in_preamble()
  return not env("document")
end

local function in_text()
  return env("document") and not in_math()
end

local function in_bullets()
  return env("itemize") or env("enumerate")
end

local function in_align()
  return env("align") or env("align*") or env("aligned")
end

--
-- Snippets:
--

return {
  s({ trig="\\\\", snippetType="autosnippet" },
    fmta([[
      \<>{<>}<>
    ]], { i(1), i(2), i(0) })
  ),
  s({ trig="\\env", snippetType="autosnippet", regTrig=true, wordTrig=false },
    fmta([[
      \begin{<>}
        <>
      \end{<>}
    ]], { i(1), i(0), rep(1) })
  ),
  s({ trig="\\fig", snippetType="autosnippet" },
    fmta([[
      \begin{figure}
        \centering
        \includegraphics[width=0.<>\linewidth]{<>}
        \caption{<>}
        \label{<>}
      \end{figure}<>
    ]], { i(1), i(2), i(3), i(4), i(0) })
  ),
  s({ trig="\\ve", snippetType="autosnippet" },
    fmta([[
      \verb|<>|<>
    ]], { i(1), i(0) })
  ),
  s({ trig="\\vs", snippetType="autosnippet" },
    fmta([[
      \vspace{<>mm}<>
    ]], { i(1), i(0) })
  ),
  s({ trig="\\ni", snippetType="autosnippet" },
    fmta([[
      \noindent<>
    ]], { i(0) })
  ),
  s({ trig="\\i", snippetType="autosnippet" },
    fmta([[
      \item <>
    ]], { i(0) })
  )
}
