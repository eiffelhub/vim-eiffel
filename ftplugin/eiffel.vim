" Vim filetype plugin file
" Language:	Eiffel
" Maintainer:	Jocelyn Fiat <jfiat@eiffel.com>
" 				(https://github.com/eiffelhub/vim-eiffel)
" URL: https://github.com/eiffelhub/vim-eiffel
" Last Change:	Tue 22 Apr 2003 09:50:08

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

" Matchit handling

" The following lines enable the macros/matchit.vim plugin for
" extended matching with the % key.

if exists("loaded_matchit")

  let b:match_ignorecase = 0
  if !exists("b:match_words") |
    let b:match_words = '\<\%(do\|if\|from\|check\|inspect\)\>:' . '\<\%(else\|elseif\|until\|loop\|when\):'. '\<end\>'
  endif

endif " exists("loaded_matchit")
