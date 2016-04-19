" Eiffel syntax file
" Language:	Eiffel
" Maintainer: Jocelyn Fiat <jfiat@eiffel.com>
" Previous maintainer:	Reimer Behrends <behrends@cse.msu.edu>
" Contributions from: Thilo Six
"
" URL: https://github.com/eiffelhub/vim-eiffel
" Prelude {{{1
if exists("b:current_syntax")
  finish
endif

let s:keepcpo= &cpo
set cpo&vim

" Whitespace errors {{{1
if exists("eiffel_space_errors")
  if !exists("eiffel_no_trail_space_error")
    syn match eiffelSpaceError display excludenl "\s\+$"
  endif
  if !exists("eiffel_no_tab_space_error")
    syn match eiffelSpaceError display " \+\t"me=e-1
  endif
endif

" Case sensitivity {{{1
if exists("eiffel_ignore_case") && !exists("eiffel_liberty")
  syn case ignore
else
  syn case match
  if exists("eiffel_pedantic") || exists("eiffel_strict")
    syn keyword eiffelError	current void result precursor
    syn keyword eiffelError	CURRENT VOID RESULT PRECURSOR
    syn keyword eiffelError	none None
    syn keyword eiffelError	TRUE FALSE
  endif
  if exists("eiffel_pedantic")
    syn keyword eiffelError	true false
    syn match	eiffelError	"\<[a-z_]\+[A-Z][a-zA_Z_]*\>"
    syn match	eiffelError	"\<[A-Z][a-z_]*[A-Z][a-zA-Z_]*\>"
  else
    syn keyword eiffelBoolean	true false
  endif
  if exists("eiffel_lower_case_predef")
    syn keyword eiffelPredefined current void result precursor
  endif
endif

syn cluster eiffelContained contains=eiffelPercentCode,eiffelPercentCodeError,eiffelTodo,eiffelCommentName,eiffelSpecialFeatureSet,eiffelSpecialNoteName,eiffelConstraint

" Parentheses {{{1
" TODO: only use this in feature do/once blocks?
" syn match   eiffelParenError	")"
" syn region  eiffelParen		transparent start="(" end=")" contains=ALLBUT,eiffelParenError,@eiffelContained,@eiffelTopClauses
" syn cluster eiffelParen		contains=eiffelParen,eiffelParenError

" Brackets {{{1
syn match  eiffelBracketError	"\]"
syn region eiffelBrackets	transparent matchgroup=eiffelBracket start="\[" end="]" contains=ALLBUT,eiffelBracketError,@eiffelContained,@eiffelTopClauses

" Keywords {{{1
syn keyword eiffelDeclaration	alias assign attribute deferred do external local unique
syn keyword eiffelDeclaration	alias contained containedin=eiffelInheritClause
syn keyword eiffelDeclaration	convert contained containedin=eiffelInheritClause,@eiffelFeature
syn match   eiffelDeclaration	"\<\%(obsolete\|note\)\>" contained containedin=@eiffelFeature
if exists("eiffel_liberty")
  syn match   eiffelDeclaration	"\<indexing\>" contained containedin=@eiffelFeature
end
syn match   eiffelDeclaration	"\<once\>"
if !exists("eiffel_liberty")
  syn keyword eiffelAttachment	attached detachable
  syn keyword eiffelProperty	expanded reference contained containedin=eiffelFormalGeneric
  syn keyword eiffelProperty	separate
else
  syn keyword eiffelError	separate
end
syn keyword eiffelProperty	expanded separate frozen
syn keyword eiffelProperty	prefix infix contained containedin=eiffelInheritClause,@eiffelFeature
syn keyword eiffelInherit	rename contained containedin=eiffelFormalGeneric
syn keyword eiffelInherit	rename redefine undefine select export
syn keyword eiffelConditional	if else elseif inspect when then
syn keyword eiffelRepeat	from until loop
if !exists("eiffel_liberty")
  syn keyword eiffelRepeat	across all some
end
syn keyword eiffelException	rescue retry
syn keyword eiffelKeyword	as
syn match   eiffelKeyword	"\<end\>" contained containedin=@eiffelFeature

syn match   eiffelAssertion	"\<require\%(\s\+else\)\=\>"
syn match   eiffelAssertion	"\<ensure\%(\s\+then\)\=\>"
syn match   eiffelAssertion	"\<invariant\>" contained containedin=@eiffelFeature
syn keyword eiffelAssertion	variant check

syn keyword eiffelAgent		agent
syn keyword eiffelDebug		debug
syn match   eiffelCreate	"\<create\>" contained containedin=@eiffelFeature
syn match   eiffelOnce		'\<once\>\ze\s\+"'
if exists("eiffel_liberty")
  syn match   eiffelOnce	'\<once\>\ze\s\+U"'
end

" Predefined names {{{1
syn keyword eiffelPredefined	Current Void Result Precursor
syn keyword eiffelAnchor	Current contained

" Special names {{{1
syn keyword eiffelSpecialFeatureSet	all contained
syn keyword eiffelAnchored		like nextgroup=eiffelAnchor skipwhite
syn keyword eiffelSpecialNoteName	EIS

" Operators {{{1
if exists("eiffel_liberty")
  syn match eiffelFreeOperator		"[-+*/\=<>@#|&~]\%([.?{}]*[-+*/\=<>@#|&~]\)*"
else
  syn match eiffelFreeOperator		"[@#|&][^%[:space:][:cntrl:]]*"
endif
syn match   eiffelPredefinedOperator	"\%(=\|/=\|\~\|/\~\)"
syn match   eiffelStandardOperator	"[-+*/^<>]"
syn match   eiffelStandardOperator	"\%(<=\|>=\|//\|\\\\\|\.\.\)"
syn match   eiffelStandardOperatorWord	"\<and\%(\s\+then\)\=\>"
syn match   eiffelStandardOperatorWord	"\<or\%(\s\+else\)\=\>"
syn keyword eiffelStandardOperatorWord	xor implies not

" postcondition 'operators'
if !exists("eiffel_liberty")
  syn keyword eiffelOperator	strip
end
syn keyword eiffelOperator	old

syn match   eiffelAddress	"\$"
syn match   eiffelAssignment	":="
if exists("eiffel_liberty")
  syn match   eiffelAssignment	"[:?]:="
end
syn match   eiffelConstraint	"->"
syn match   eiffelTypeBrace	"[{}]"

" Class names {{{1
syn match   eiffelClassName	"\C\<[A-Z][A-Z0-9_]*\>" nextgroup=eiffelFormalGeneric skipwhite contained containedin=eiffelClassClause
syn match   eiffelClassName	"\C\<[A-Z][A-Z0-9_]*\>"						contained containedin=eiffelClients
syn match   eiffelClassName	"\C\<[A-Z][A-Z0-9_]*\>" nextgroup=eiffelActualGeneric skipwhite

if exists("eiffel_liberty")
  syn keyword eiffelError NONE containedin=ALLBUT,@eiffelString,@eiffelComment
end

" actual generics, tuples, conversions
syn cluster eiffelTypeList contains=eiffelClassName,eiffelAttachment,eiffelTypeBrace,eiffelAnchored
" exports, create, feature
syn region eiffelClients matchgroup=eiffelTypeBrace start="{" end="}" contained contains=@eiffelComment "contains=eiffelClassName

" Generics {{{1
" TODO: remove contained eActualGeneric? Utilise eTypeList?
syn region  eiffelFormalGeneric	transparent matchgroup=eiffelBracket start="\[" end="]" contained contains=eiffelActualGeneric,eiffelClassName,eiffelTypeBrace,eiffelConstraint,eiffelAttachment,eiffelCreate,eiffelKeyword
syn region  eiffelActualGeneric	transparent matchgroup=eiffelBracket start="\[" end="]" contained contains=eiffelActualGeneric,eiffelClassName,eiffelTypeBrace,eiffelAttachment,eiffelAnchored

" Constants and other manifest forms {{{1
syn keyword eiffelBoolean		True False

syn cluster eiffelString		contains=eiffelString,eiffelVerbatimString

syn match   eiffelStringError		'"%\+"'

syn match   eiffelPercentCode		contained "\c%/\%(\d\+\|0x\x\+\|0c\o\+\|0b[01]\+\)/" containedin=eiffelCharacter
syn match   eiffelPercentCodeError	contained "%."
syn match   eiffelPercentCode		contained "%[ABCDFHLNQRSTUV%'"()<>]"
syn match   eiffelPercentCodeError	contained "%/[^/]*/\="
syn match   eiffelPercentCode		contained "%/\d\+/"
if exists("eiffel_liberty")
  syn match   eiffelPercentCode		contained "\c%/0x\%(\x\x\)\+/"
  syn match   eiffelUnicodePercentCode	contained "%/Ux\x\+\%(_\x\+\)*/"
else
  syn match eiffelStringError		'\<U"'
end

syn region  eiffelString		start=+"+		 skip=+%%\|%"+ end=+"+ contains=eiffelPercentCode,eiffelPercentCodeError oneline
syn region  eiffelString		start=+"\ze.*%\s*\n\s*%+ skip=+%%\|%"+ end=+"+ contains=eiffelPercentCode,eiffelPercentCodeError,eiffelStringLineWrap fold
if exists("eiffel_liberty")
  syn region  eiffelUnicodeString	start=+U"+		  skip=+%%\|%"+ end=+"+ contains=eiffelPercentCode,eiffelUnicodePercentCode,eiffelPercentCodeError oneline
  syn region  eiffelUnicodeString	start=+U"\ze.*%\s*\n\s*%+ skip=+%%\|%"+ end=+"+ contains=eiffelPercentCode,eiffelUnicodePercentCode,eiffelPercentCodeError,eiffelStringLineWrap fold
  syn cluster eiffelString		add=eiffelUnicodeString
end

syn match   eiffelPercentCodeError	contained "\%(%%\)\+\n"
syn match   eiffelStringLineWrap	contained "%\s*\n\s*%"

syn region  eiffelVerbatimString	start=+"\z([^"]*\)\[\s*$+ end=+^\s*]\z1"+ fold
syn region  eiffelVerbatimString	start=+"\z([^"]*\){\s*$+  end=+^\s*}\z1"+ fold

syn match   eiffelCharacterError	"'[^']*'"

syn match   eiffelCharacter		"'%\@!\p'"
syn match   eiffelCharacter		"\C'%[ABCDFHLNQRSTUV%'"()<>]'" contains=eiffelPercentCode
if exists("eiffel_liberty")
  syn match   eiffelCharacter		"'\c%/\%(\d\+\|0x\x\x\)/'" contains=eiffelPercentCode
else
  syn match   eiffelCharacter		"'\c%/\%(\d\+\|0x\x\+\|0c\o\+\|0b[01]\+\)/'" contains=eiffelPercentCode
end
syn cluster eiffelCharacter		contains=eiffelCharacter,eiffelCharacterError

if !exists("eiffel_liberty")
  syn match   eiffelInteger		"\c-\=\<0b[01]\+\%(_[01]\+\)*\>"
  syn match   eiffelInteger		"\c-\=\<0c\o\+\%(_\o\+\)*\>"
end
syn match   eiffelInteger		"-\=\<\d\+\%(_\d\+\)*\>"
syn match   eiffelInteger		"\c-\=\<0x\x\+\%(_\x\+\)*\>"
syn match   eiffelReal			"-\=\<\d\+\%(_\d\+\)*\.\%(\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)\="
syn match   eiffelReal			"-\=\.\d\+\%(_\d\+\)*\%([eE][-+]\=\d\+\%(_\d\+\)*\)\="
syn cluster eiffelNumber		contains=eiffelInteger,eiffelReal

syn region  eiffelArray			matchgroup=eiffelArrayDelimiter start="<<" end=">>" transparent fold

syn cluster eiffelManifestType		contains=eiffelClassName,eiffelAttachment,eiffelAnchored,eiffelTypeBrace
syn cluster eiffelConstant		contains=eiffelBoolean,@eiffelCharacter,@eiffelNumber,@eiffelString,@eiffelManifestType

" Folding support {{{1
if has("folding") && exists("eiffel_fold")
  setlocal foldtext=EiffelFoldText()
  setlocal foldmethod=syntax
endif

" join multiline class headers
function! EiffelFoldText()
	let fdt = foldtext()
	if getline(v:foldstart) =~ 'class\s*$'
		let classname = substitute(getline(nextnonblank(v:foldstart + 1)), '\s*', '', '')
		let fdt .= ' ' . classname
	endif
	return fdt
endfunction

function! s:IsFoldable(name)
  let default = "note,obsolete,inherit,create,convert,feature,feature-body,invariant" " exclude class headers
  return index(split(get(g:, "eiffel_fold_groups", default), '\s*,\s*'), a:name, 0, 1) != -1
endfunction

" Top level clauses {{{1
function! s:MakeTopClause(name, start, end, contains)
  call add(a:end, '^end\>\ze\(\(^\(\s*--\|\s*$\)\@!\)\@!\_.\)*\%$')
  execute "syn region eiffel" . a:name . "Clause transparent matchgroup=eiffelTopStruct" .
	\ ' start="' . a:start . '"'
	\ join(map(a:end, '"end=\"" . v:val . "\""')) .
	\ " contains=" . a:contains . " " . (s:IsFoldable(a:name) ? "fold" : "")
endfunction

syn cluster eiffelTopClauses contains=eiffelNoteClause,eiffelClassClause,eiffelObsoleteClause,eiffelInheritClause,eiffelCreateClause,eiffelConvertClause,eiffelFeatureClause,eiffelInvariantClause

if exists("eiffel_liberty")
  let s:inherit = 'inherit\|insert'
  let s:note	= '\%(note\|indexing\)'
else
  let s:inherit = "inherit"
  let s:note	= "note"
end

call s:MakeTopClause("Note", '\<' . s:note . '\>',
		   \ ['\ze\<class\>',
		   \  '\ze\<deferred\s\+class\>',
		   \  '\ze\<\%(\%\(frozen\)\=\s*\%(expanded\)\=\s*\%(external\)\=\)\s\+class\>'],
		   \ "@eiffelConstant,eiffelSpecialNoteName,@eiffelComment")
call s:MakeTopClause("Class", '\<class\>',
		   \ ['\ze\<\%(obsolete\|' . s:inherit . '\|create\|convert\|feature\|invariant\)\>', '\ze;\=' . s:note . '\>'],
		   \ "@eiffelComment")
call s:MakeTopClause("Obsolete", '\<obsolete\>',
		   \ ['\ze\<\%(' . s:inherit . '\|create\|convert\|feature\|invariant\)\>', '\ze;\=' . s:note . '\>'],
		   \ "@eiffelString,@eiffelComment")
call s:MakeTopClause("Inherit", '\<\%(' . s:inherit . '\)\>',
		   \ ['\ze\<\%(' . s:inherit . '\|create\|_convert\|feature\|invariant\)\>', '^\zeconvert\>', '\ze;\=' . s:note . '\>'],
		   \ "eiffelClassName,eiffelInherit,eiffelKeyword,eiffelClients,eiffelSpecialFeatureSet,@eiffelString,@eiffelComment")
call s:MakeTopClause("Create", '\<create\>',
		   \ ['\ze\<\%(create\|convert\|feature\|invariant\)\>', '\ze;\=' . s:note . '\>'],
		   \ "TOP,@eiffelTopClauses")
call s:MakeTopClause("Convert", '\<convert\>',
		   \ ['\ze\<\%(feature\|invariant\)\>', '\ze;\=' . s:note . '\>'],
		   \ "eiffelTypeBrace,eiffelClassName,eiffelAttachment,@eiffelComment")
call s:MakeTopClause("Feature", '\<feature\>',
		   \ ['\ze\%(\<feature\|^invariant\)\>', '^\ze;\=' . s:note . '\>'],
		   \ "TOP,@eiffelTopClauses")
call s:MakeTopClause("Invariant", '\<invariant\>',
		   \ ['\ze;\=' . s:note . '\>'],
		   \ "TOP,@eiffelTopClauses")

" Features {{{1
syn cluster eiffelFeature contains=eiffelFeatureClause,eiffelFeature

exec 'syn region eiffelFeature ' .
   \ 'start="^\s\+\%' .   (&sw + 1)	. 'vfrozen\>" ' .
   \ 'start="^\s\+\%' .   (&sw + 1)	. 'v\a\w*.\{-}\(:\|\_s*(\_s*\a\w*:\|\(\_s*--.*\|\_s\)*\<\(do\|once\|deferred\|external\|obsolete\|note\|require\|local\|attribute\)\>\)" ' .
   \ 'end="\ze\n\_s*\%' . (&sw + 1)	. 'v\a\w*" ' .
   \ 'end="^\s\+\%' .	  (&sw * 2 + 1) . 'v\<end\>" ' .
   \ 'end="\ze\n\%(\s*\n\)*\%(\%(feature\|invariant\|note\|end\)\>\)" ' .
   \ 'contained containedin=eiffelFeatureClause contains=TOP,@eiffelTopClauses ' . (s:IsFoldable("feature-body") ? 'fold' : '')

" Comments {{{1
syn match   eiffelComment		"--.*" contains=eiffelTodo,eiffelCommentName,@Spell
syn match   eiffelComment		"\%(^end\s*\)\@<=--\sclass \u\w*\s*$" contains=eiffelClassName
" TODO: from memory, the official recommendation is to conceal quotes and italicize - desirable?
syn match   eiffelCommentName		"`[^']\+'" contained
syn match   eiffelTodo			"\C\%(TODO\|XXX\|FIXME\|NOTE\)" contained
syn region  eiffelMultilineComment	start="^\s*--.*\n\%(\s*--\)\@=" end="^\s*--.*\n\%(\s*--\)\@!" contains=eiffelComment transparent keepend fold
syn cluster eiffelComment		contains=eiffelComment,eiffelMultilineComment

" Syncing {{{1
syn sync fromstart

" Default highlighting {{{1
hi def link eiffelKeyword		Statement
hi def link eiffelProperty		Statement
hi def link eiffelInherit		Statement
hi def link eiffelConditional		Statement
hi def link eiffelRepeat		Statement
hi def link eiffelDeclaration		Statement
hi def link eiffelAttachment		Statement
hi def link eiffelAssertion		Statement
hi def link eiffelDebug			Statement
hi def link eiffelException		Statement

hi def link eiffelAgent			Statement
hi def link eiffelCreate		Statement
hi def link eiffelNote			Statement
hi def link eiffelObsolete		Statement
hi def link eiffelOnce			Statement

hi def link eiffelTopStruct		PreProc

hi def link eiffelAnchored		Special
hi def link eiffelSpecialFeatureSet	Special
hi def link eiffelSpecialNoteName	Special

hi def link eiffelClassName		Type

hi def link eiffelBoolean		Boolean
hi def link eiffelCharacter		Character
hi def link eiffelString		String
hi def link eiffelUnicodeString		eiffelString
hi def link eiffelVerbatimString	eiffelString
hi def link eiffelInteger		Number
hi def link eiffelReal			Float

hi def link eiffelPercentCode		Special
hi def link eiffelStringLineWrap	eiffelPercentCode
hi def link eiffelUnicodePercentCode	eiffelPercentCode

hi def link eiffelOperator		Special
hi def link eiffelFreeOperator		eiffelOperator
hi def link eiffelStandardOperatorWord	eiffelOperator
hi def link eiffelArrayDelimiter	Special
hi def link eiffelTypeBrace		Special
hi def link eiffelBracket		Special
hi def link eiffelAddress		Special
hi def link eiffelConstraint		Special

hi def link eiffelPredefined		Constant
hi def link eiffelAnchor		eiffelPredefined

hi def link eiffelComment		Comment
hi def link eiffelCommentName		SpecialComment
hi def link eiffelTodo			Todo

hi def link eiffelError			Error
hi def link eiffelStringError		Error
hi def link eiffelCharacterError	Error
hi def link eiffelPercentCodeError	Error
hi def link eiffelParenError		Error
hi def link eiffelBracketError		Error
hi def link eiffelSpaceError		Error

" Postscript {{{1
let b:current_syntax = "eiffel"

let &cpo = s:keepcpo
unlet s:keepcpo

