let b:current_syntax = "vim"
syntax match Comment /^[ \t:]*".*$/
syntax match vimContinue "^\s*\\"
syntax region String start="^\s*\\\z(['"]\)" skip='\\\\\|\\\z1' end="\z1" oneline keepend contains=@vimStringGroup,vimContinue

nnoremap ,, :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

syntax region Constant oneline keepend    start=+[^:a-zA-Z>!\\@]"+lc=1 skip=+\\\\\|\\"+ end=+"+   contains=@vimStringGroup
syntax region Constant oneline keepend    start=+[^:a-zA-Z>!\\@]'+lc=1 end=+'+
syntax region Constant oneline    start=+=!+lc=1  skip=+\\\\\|\\!+ end=+!+    contains=@vimStringGroup
syntax region Constant oneline    start="=+"lc=1  skip="\\\\\|\\+" end="+"    contains=@vimStringGroup
syntax region Constant oneline    start="\s/\s*\A"lc=1 skip="\\\\\|\\+" end="/"   contains=@vimStringGroup
syntax match String contained   +"[^"]*\\$+ skipnl nextgroup=vimStringCont
syntax match StringCont contained   +\(\\\\\|.\)\{-}[^\\]"+

syntax match vimNumber "\<\d\+\%(\.\d\+\%([eE][+-]\=\d\+\)\=\)\=" skipwhite nextgroup=vimGlobal,vimSubst,vimCommand
syntax match vimNumber "-\d\+\%(\.\d\+\%([eE][+-]\=\d\+\)\=\)\="  skipwhite nextgroup=vimGlobal,vimSubst,vimCommand
syntax match vimNumber "\<0[xX]\x\+"
syntax match vimNumber "#\x\{6}"

finish

" unsupported settings: these are supported by vi but don't do anything in vim {{{2
syntax keyword vimErrSetting contained	hardtabs ht w1200 w300 w9600 

" AutoCmd Events {{{2
syntax case ignore
syntax keyword vimAutoEvent contained	BufAdd BufCreate BufDelete BufEnter BufFilePost BufFilePre BufHidden BufLeave BufNew BufNewFile BufRead BufReadCmd BufReadPost BufReadPre BufUnload BufWinEnter BufWinLeave BufWipeout BufWrite BufWriteCmd BufWritePost BufWritePre Cmd-event CmdwinEnter CmdwinLeave ColorScheme CursorHold CursorHoldI CursorMoved CursorMovedI EncodingChanged FileAppendCmd FileAppendPost FileAppendPre FileChangedRO FileChangedShell FileChangedShellPost FileEncoding FileReadCmd FileReadPost FileReadPre FileType FileWriteCmd FileWritePost FileWritePre FilterReadPost FilterReadPre FilterWritePost FilterWritePre FocusGained FocusLost FuncUndefined GUIEnter GUIFailed InsertChange InsertCharPre InsertEnter InsertLeave MenuPopup QuickFixCmdPost QuickFixCmdPre RemoteReply SessionLoadPost ShellCmdPost ShellFilterPost SourceCmd SourcePre SpellFileMissing StdinReadPost StdinReadPre SwapExists Syntax TabEnter TabLeave TermChanged TermResponse User UserGettingBored VimEnter VimLeave VimLeavePre VimResized WinEnter WinLeave

" Highlight commonly used Groupnames {{{2
syntax keyword vimGroup contained	Comment Constant String Character Number Boolean Float Identifier Function Statement Conditional Repeat Label Operator Keyword Exception PreProc Include Define Macro PreCondit Type StorageClass Structure Typedef Special SpecialChar Tag Delimiter SpecialComment Debug Underlined Ignore Error Todo

" Default highlighting groups {{{2
syntax keyword vimHLGroup contained	ColorColumn Cursor CursorColumn CursorIM CursorLine DiffAdd DiffChange DiffDelete DiffText Directory ErrorMsg FoldColumn Folded IncSearch LineNr MatchParen Menu ModeMsg MoreMsg NonText Normal Pmenu PmenuSbar PmenuSel PmenuThumb Question Scrollbar Search SignColumn SpecialKey SpellBad SpellCap SpellLocal SpellRare StatusLine StatusLineNC TabLine TabLineFill TabLineSel Title Tooltip VertSplit Visual VisualNOS WarningMsg WildMenu
syntax match vimHLGroup contained	"Conceal"
syntax case match

" Function Names {{{2
syntax keyword vimFuncName contained	abs and argidx atan browsedir bufloaded bufwinnr call char2nr col complete_check cos cscope_connection delete diff_hlID eval exists expr8 filereadable finddir floor fnamemodify foldlevel foreground get getchar getcmdpos getfontname getftime getloclist getpos getregtype getwinposx glob has_key histadd histnr hostname index inputlist inputsecret isdirectory join libcall line2byte log map match matchdelete matchstr mkdir nextnonblank pathshorten printf pyeval reltime remote_foreground remote_read remove repeat reverse search searchpair searchpos serverlist setcmdpos setloclist setpos setreg settabwinvar shellescape simplify sinh soundfold spellsuggest sqrt str2nr strdisplaywidth stridx strlen strridx strwidth substitute synID synIDtrans system tabpagenr tagfiles tan tempname toupper trunc undofile values visualmode wincol winline winrestcmd winsaveview writefile
syntax keyword vimFuncName contained	acos append argv atan2 bufexists bufname byte2line ceil cindent complete confirm cosh cursor did_filetype empty eventhandler exp extend filewritable findfile fmod foldclosed foldtext function getbufline getcharmod getcmdtype getfperm getftype getmatches getqflist gettabvar getwinposy globpath haslocaldir histdel hlexists iconv input inputrestore insert islocked keys libcallnr lispindent log10 maparg matchadd matchend max mode nr2char pow pumvisible range reltimestr remote_peek remote_send rename resolve round searchdecl searchpairpos server2client setbufvar setline setmatches setqflist settabvar setwinvar shiftwidth sin sort spellbadword split str2float strchars strftime string strpart strtrans submatch synconcealed synIDattr synstack tabpagebuflist tabpagewinnr taglist tanh tolower tr type undotree virtcol winbufnr winheight winnr winrestview winwidth xor
syntax keyword vimFuncName contained	add argc asin browse buflisted bufnr byteidx changenr clearmatches complete_add copy count deepcopy diff_filler escape executable expand feedkeys filter float2nr fnameescape foldclosedend foldtextresult garbagecollect getbufvar getcmdline getcwd getfsize getline getpid getreg gettabwinvar getwinvar has hasmapto histget hlID indent inputdialog inputsave invert items len line localtime luaeval mapcheck matcharg matchlist min mzeval or prevnonblank py3eval readfile remote_expr

" Numbers {{{2
" =======
syntax match vimNumber	"\<\d\+\%(\.\d\+\%([eE][+-]\=\d\+\)\=\)\=" skipwhite nextgroup=vimGlobal,vimSubst,vimCommand
syntax match vimNumber	"-\d\+\%(\.\d\+\%([eE][+-]\=\d\+\)\=\)\="  skipwhite nextgroup=vimGlobal,vimSubst,vimCommand
syntax match vimNumber	"\<0[xX]\x\+"
syntax match vimNumber	"#\x\{6}"

" All vimCommands are contained by vimIsCommands. {{{2
syntax match vimCmdSep	"[:|]\+"	skipwhite nextgroup=vimAddress,vimAutoCmd,vimCommand,vimExtCmd,vimFilter,vimLet,vimMap,vimMark,vimSet,vimSyntax,vimUserCmd
syntax match vimIsCommand	"\<\h\w*\>"	contains=vimCommand
syntax match vimVar        contained	"\<\h[a-zA-Z0-9#_]*\>"
syntax match vimVar		"\<[bwglsav]:\h[a-zA-Z0-9#_]*\>"
syntax match vimFBVar      contained   "\<[bwglsav]:\h[a-zA-Z0-9#_]*\>"
syntax keyword vimCommand  contained	in

" Filetypes {{{2
" =========
syntax match   vimFiletype	"\<filet\%[ype]\(\s\+\I\i*\)*"	skipwhite contains=vimFTCmd,vimFTOption,vimFTError
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_vimFTError")
    syntax match   vimFTError  contained	"\I\i*"
endif
syntax keyword vimFTCmd    contained	filet[ype]
syntax keyword vimFTOption contained	detect indent off on plugin

" Augroup : vimAugroupError removed because long augroups caused sync'ing problems. {{{2
" ======= : Trade-off: Increasing synclines with slower editing vs augroup END error checking.
syntax cluster vimAugroupList	contains=vimIsCommand,vimCommand,vimUserCmd,vimExecute,vimNotFunc,vimFuncName,vimFunction,vimFunctionError,vimLineComment,vimSpecFile,vimOper,vimNumber,vimOperParen,vimComment,vimString,vimSubst,vimMark,vimRegister,vimAddress,vimFilter,vimCmplxRepeat,vimComment,vimLet,vimSet,vimAutoCmd,vimRegion,vimSynLine,vimNotation,vimCtrlChar,vimFuncVar,vimContinue
if exists("g:vimsyn_folding") && g:vimsyn_folding =~ 'a'
    syntax region  vimAugroup	fold start="\<aug\%[roup]\>\s\+\h\w*" end="\<aug\%[roup]\>\s\+[eE][nN][dD]\>"	contains=vimAugroupKey,vimAutoCmd,@vimAugroupList keepend
else
    syntax region  vimAugroup	start="\<aug\%[roup]\>\s\+\h\w*" end="\<aug\%[roup]\>\s\+[eE][nN][dD]\>"	contains=vimAugroupKey,vimAutoCmd,@vimAugroupList keepend
endif
syntax match   vimAugroup	"aug\%[roup]!" contains=vimAugroupKey
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_noaugrouperror")
    syntax match   vimAugroupError	"\<aug\%[roup]\>\s\+[eE][nN][dD]\>"
endif
syntax keyword vimAugroupKey contained	aug[roup]

" Operators: {{{2
" =========
" COMBAK: vimOperParen used to have "oneline"
syntax cluster	vimOperGroup	contains=vimEnvvar,vimFunc,vimFuncVar,vimOper,vimOperParen,vimNumber,vimString,vimRegister,vimContinue
syntax match	vimOper	"\(==\|!=\|>=\|<=\|=\~\|!\~\|>\|<\|=\)[?#]\{0,2}"	skipwhite nextgroup=vimString,vimSpecFile
syntax match	vimOper	"||\|&&\|[-+.]"	skipwhite nextgroup=vimString,vimSpecFile
syntax region	vimOperParen 	matchgroup=vimParenSep	start="(" end=")" contains=@vimOperGroup
syntax region	vimOperParen	matchgroup=vimSep	start="{" end="}" contains=@vimOperGroup nextgroup=vimVar,vimFuncVar
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_noopererror")
    syntax match	vimOperError	")"
endif

" Functions : Tag is provided for those who wish to highlight tagged functions {{{2
" =========
syntax cluster	vimFuncList	contains=vimCommand,vimFunctionError,vimFuncKey,Tag,vimFuncSID
syntax cluster	vimFuncBodyList	contains=vimAbb,vimAddress,vimAugroupKey,vimAutoCmd,vimCmplxRepeat,vimComment,vimComment,vimContinue,vimCtrlChar,vimEcho,vimEchoHL,vimExecute,vimIf,vimIsCommand,vimFBVar,vimFunc,vimFunction,vimFuncVar,vimHighlight,vimIsCommand,vimLet,vimLineComment,vimMap,vimMark,vimNorm,vimNotation,vimNotFunc,vimNumber,vimOper,vimOperParen,vimRegion,vimRegister,vimSet,vimSpecFile,vimString,vimSubst,vimSynLine,vimUnmap,vimUserCommand
syntax match	vimFunction	"\<fu\%[nction]!\=\s\+\%(<[sS][iI][dD]>\|[sSgGbBwWtTlL]:\)\=\%(\i\|[#.]\|{.\{-1,}}\)*\ze\s*("	contains=@vimFuncList nextgroup=vimFuncBody

if exists("g:vimsyn_folding") && g:vimsyn_folding =~ 'f'
    syntax region	vimFuncBody  contained	fold start="\ze("	matchgroup=vimCommand end="\<\(endf\>\|endfu\%[nction]\>\)"		contains=@vimFuncBodyList
else
    syntax region	vimFuncBody  contained	start="\ze("	matchgroup=vimCommand end="\<\(endf\>\|endfu\%[nction]\>\)"		contains=@vimFuncBodyList
endif
syntax match	vimFuncVar   contained	"a:\(\h\w*\|\d\+\)"
syntax match	vimFuncSID   contained	"\c<sid>\|\<s:"
syntax keyword	vimFuncKey   contained	fu[nction]
syntax match	vimFuncBlank contained	"\s\+"

syntax keyword	vimPattern   contained	start	skip	end

" Special Filenames, Modifiers, Extension Removal: {{{2
" ===============================================
syntax match	vimSpecFile	"<c\(word\|WORD\)>"	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFile	"<\([acs]file\|amatch\|abuf\)>"	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFile	"\s%[ \t:]"ms=s+1,me=e-1	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFile	"\s%$"ms=s+1	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFile	"\s%<"ms=s+1,me=e-1	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFile	"#\d\+\|[#%]<\>"	nextgroup=vimSpecFileMod,vimSubst
syntax match	vimSpecFileMod	"\(:[phtre]\)\+"	contained

" Environment Variables: {{{2
" =====================
syntax match	vimEnvvar	"\$\I\i*"
syntax match	vimEnvvar	"\${\I\i*}"

" In-String Specials: {{{2
" Try to catch strings, if nothing else matches (therefore it must precede the others!)
"  vimEscapeBrace handles ["]  []"] (ie. "s don't terminate string inside [])
syntax region	vimEscapeBrace	oneline   contained transparent start="[^\\]\(\\\\\)*\[\zs\^\=\]\=" skip="\\\\\|\\\]" end="]"me=e-1
syntax match	vimPatSepErr	contained	"\\)"
syntax match	vimPatSep	contained	"\\|"
syntax region	vimPatSepZone	oneline   contained   matchgroup=vimPatSepZ start="\\%\=\ze(" skip="\\\\" end="\\)\|[^\]['"]"	contains=@vimStringGroup
syntax region	vimPatRegion	contained transparent matchgroup=vimPatSepR start="\\[z%]\=(" end="\\)"	contains=@vimSubstList oneline
syntax match	vimNotPatSep	contained	"\\\\"
syntax cluster	vimStringGroup	contains=vimEscapeBrace,vimPatSep,vimNotPatSep,vimPatSepErr,vimPatSepZone,@Spell
syntax region	vimString	oneline keepend	start=+[^:a-zA-Z>!\\@]"+lc=1 skip=+\\\\\|\\"+ end=+"+	contains=@vimStringGroup
syntax region	vimString	oneline keepend	start=+[^:a-zA-Z>!\\@]'+lc=1 end=+'+
syntax region	vimString	oneline	start=+=!+lc=1	skip=+\\\\\|\\!+ end=+!+	contains=@vimStringGroup
syntax region	vimString	oneline	start="=+"lc=1	skip="\\\\\|\\+" end="+"	contains=@vimStringGroup
syntax region	vimString	oneline	start="\s/\s*\A"lc=1 skip="\\\\\|\\+" end="/"	contains=@vimStringGroup
syntax match	vimString	contained	+"[^"]*\\$+	skipnl nextgroup=vimStringCont
syntax match	vimStringCont	contained	+\(\\\\\|.\)\{-}[^\\]"+

" Substitutions: {{{2
" =============
syntax cluster	vimSubstList	contains=vimPatSep,vimPatRegion,vimPatSepErr,vimSubstTwoBS,vimSubstRange,vimNotation
syntax cluster	vimSubstRepList	contains=vimSubstSubstr,vimSubstTwoBS,vimNotation
syntax cluster	vimSubstList	add=vimCollection
syntax match	vimSubst	"\(:\+\s*\|^\s*\||\s*\)\<\%(s\%[ubstitute]\|sm\%[agic]\|sno\%[magic]\)[:[:alpha:]]\@!" nextgroup=vimSubstPat
syntax match	vimSubst	"s\%[ubstitute][:#[:alpha:]]\@!"	nextgroup=vimSubstPat contained
syntax match	vimSubst	"/\zss\%[ubstitute]\ze/"	nextgroup=vimSubstPat
syntax match	vimSubst1       contained	"s\%[ubstitute]\>"	nextgroup=vimSubstPat
syntax region	vimSubstPat     contained	matchgroup=vimSubstDelim start="\z([^a-zA-Z( \t[\]&]\)"rs=s+1 skip="\\\\\|\\\z1" end="\z1"re=e-1,me=e-1	 contains=@vimSubstList	nextgroup=vimSubstRep4	oneline
syntax region	vimSubstRep4    contained	matchgroup=vimSubstDelim start="\z(.\)" skip="\\\\\|\\\z1" end="\z1" matchgroup=vimNotation end="<[cC][rR]>" contains=@vimSubstRepList	nextgroup=vimSubstFlagErr	oneline
syntax region	vimCollection   contained transparent	start="\\\@<!\[" skip="\\\[" end="\]"	contains=vimCollClass
syntax match	vimCollClassErr contained	"\[:.\{-\}:\]"
syntax match	vimCollClass    contained transparent	"\[:\(alnum\|alpha\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|xdigit\|return\|tab\|escape\|backspace\):\]"
syntax match	vimSubstSubstr  contained	"\\z\=\d"
syntax match	vimSubstTwoBS   contained	"\\\\"
syntax match	vimSubstFlagErr contained	"[^< \t\r|]\+" contains=vimSubstFlags
syntax match	vimSubstFlags   contained	"[&cegiIpr]\+"

" 'String': {{{2
syntax match	vimString	"[^(,]'[^']\{-}\zs'"

" Marks, Registers, Addresses, Filters: {{{2
syntax match	vimMark	"'[a-zA-Z0-9]\ze[-+,!]"	nextgroup=vimOper,vimMarkNumber,vimSubst
syntax match	vimMark	"'[<>]\ze[-+,!]"		nextgroup=vimOper,vimMarkNumber,vimSubst
syntax match	vimMark	",\zs'[<>]\ze"		nextgroup=vimOper,vimMarkNumber,vimSubst
syntax match	vimMark	"[!,:]\zs'[a-zA-Z0-9]"	nextgroup=vimOper,vimMarkNumber,vimSubst
syntax match	vimMark	"\<norm\%[al]\s\zs'[a-zA-Z0-9]"	nextgroup=vimOper,vimMarkNumber,vimSubst
syntax match	vimMarkNumber	"[-+]\d\+"		nextgroup=vimSubst contained contains=vimOper
syntax match	vimPlainMark contained	"'[a-zA-Z0-9]"

syntax match	vimRegister	'[^,;[{]\zs"[a-zA-Z0-9.%#:_\-/]\ze[^a-zA-Z_":0-9]'
syntax match	vimRegister	'\<norm\s\+\zs"[a-zA-Z0-9]'
syntax match	vimRegister	'\<normal\s\+\zs"[a-zA-Z0-9]'
syntax match	vimRegister	'@"'
syntax match	vimPlainRegister contained	'"[a-zA-Z0-9\-:.%#*+=]'

syntax match	vimAddress	",\zs[.$]"	skipwhite nextgroup=vimSubst1
syntax match	vimAddress	"%\ze\a"	skipwhite nextgroup=vimString,vimSubst1

syntax match	vimFilter contained	"^!.\{-}\(|\|$\)"		contains=vimSpecFile
syntax match	vimFilter contained	"\A!.\{-}\(|\|$\)"ms=s+1	contains=vimSpecFile,vimFunction,vimFuncName,vimOperParen

" Complex repeats (:h complex-repeat) {{{2
syntax match	vimCmplxRepeat	'[^a-zA-Z_/\\()]q[0-9a-zA-Z"]\>'lc=1
syntax match	vimCmplxRepeat	'@[0-9a-z".=@:]\ze\($\|[^a-zA-Z]\>\)'

" Set command and associated set-options (vimOptions) with comment {{{2
syntax region	vimSet		matchgroup=vimCommand start="\<\%(setl\%[ocal]\|setg\%[lobal]\|se\%[t]\)\>" skip="\%(\\\\\)*\\." end="$" matchgroup=vimNotation end="<[cC][rR]>" keepend oneline contains=vimSetEqual,vimOption,vimErrSetting,vimComment,vimSetString,vimSetMod
syntax region	vimSetEqual	contained	start="[=:]\|[-+^]=" skip="\\\\\|\\\s" end="[| \t]\|$"me=e-1 contains=vimCtrlChar,vimSetSep,vimNotation,vimEnvvar oneline
syntax region	vimSetString	contained	start=+="+hs=s+1	skip=+\\\\\|\\"+  end=+"+	contains=vimCtrlChar
syntax match	vimSetSep	contained	"[,:]"
syntax match	vimSetMod	contained	"&vim\=\|[!&?<]\|all&"

" Let {{{2
" ===
syntax keyword	vimLet	let	unl[et]	skipwhite nextgroup=vimVar,vimFuncVar

" Abbreviations {{{2
" =============
syntax keyword vimAbb	ab[breviate] ca[bbrev] inorea[bbrev] cnorea[bbrev] norea[bbrev] ia[bbrev] skipwhite nextgroup=vimMapMod,vimMapLhs

" Autocmd {{{2
" =======
syntax match	vimAutoEventList	contained	"\(!\s\+\)\=\(\a\+,\)*\a\+"	contains=vimAutoEvent nextgroup=vimAutoCmdSpace
syntax match	vimAutoCmdSpace	contained	"\s\+"	nextgroup=vimAutoCmdSfxList
syntax match	vimAutoCmdSfxList	contained	"\S*"
syntax keyword	vimAutoCmd	au[tocmd] do[autocmd] doautoa[ll]	skipwhite nextgroup=vimAutoEventList

" Echo and Execute -- prefer strings! {{{2
" ================
syntax region	vimEcho	oneline excludenl matchgroup=vimCommand start="\<ec\%[ho]\>" skip="\(\\\\\)*\\|" end="$\||" contains=vimFunc,vimFuncVar,vimString,vimVar
syntax region	vimExecute	oneline excludenl matchgroup=vimCommand start="\<exe\%[cute]\>" skip="\(\\\\\)*\\|" end="$\||\|<[cC][rR]>" contains=vimFuncVar,vimIsCommand,vimOper,vimNotation,vimOperParen,vimString,vimVar
syntax match	vimEchoHL	"echohl\="	skipwhite nextgroup=vimGroup,vimHLGroup,vimEchoHLNone
syntax case ignore
syntax keyword	vimEchoHLNone	none
syntax case match

" Maps {{{2
" ====
syntax match	vimMap		"\<map\>!\=\ze\s*[^(]" skipwhite nextgroup=vimMapMod,vimMapLhs
syntax keyword	vimMap		cm[ap] cno[remap] im[ap] ino[remap] lm[ap] ln[oremap] nm[ap] nn[oremap] no[remap] om[ap] ono[remap] smap snor[emap] vm[ap] vn[oremap] xm[ap] xn[oremap] skipwhite nextgroup=vimMapBang,vimMapMod,vimMapLhs
syntax keyword	vimMap		mapc[lear] smapc[lear]
syntax keyword	vimUnmap		cu[nmap] iu[nmap] lu[nmap] nun[map] ou[nmap] sunm[ap] unm[ap] unm[ap] vu[nmap] xu[nmap] skipwhite nextgroup=vimMapBang,vimMapMod,vimMapLhs
syntax match	vimMapLhs	contained	"\S\+"			contains=vimNotation,vimCtrlChar skipwhite nextgroup=vimMapRhs
syntax match	vimMapBang	contained	"!"			skipwhite nextgroup=vimMapMod,vimMapLhs
syntax match	vimMapMod	contained	"\c<\(buffer\|expr\|\(local\)\=leader\|plug\|script\|sid\|unique\|silent\)\+>" contains=vimMapModKey,vimMapModErr skipwhite nextgroup=vimMapMod,vimMapLhs
syntax match	vimMapRhs	contained	".*" contains=vimNotation,vimCtrlChar	skipnl nextgroup=vimMapRhsExtend
syntax match	vimMapRhsExtend	contained	"^\s*\\.*$"			contains=vimNotation,vimCtrlChar,vimContinue	skipnl nextgroup=vimMapRhsExtend
syntax case ignore
syntax keyword	vimMapModKey	contained	buffer	expr	leader	localleader	plug	script	sid	silent	unique
syntax case match

" Menus {{{2
" =====
syntax cluster	vimMenuList contains=vimMenuBang,vimMenuPriority,vimMenuName,vimMenuMod
syntax keyword	vimCommand	am[enu] an[oremenu] aun[menu] cme[nu] cnoreme[nu] cunme[nu] ime[nu] inoreme[nu] iunme[nu] me[nu] nme[nu] nnoreme[nu] noreme[nu] nunme[nu] ome[nu] onoreme[nu] ounme[nu] unme[nu] vme[nu] vnoreme[nu] vunme[nu] skipwhite nextgroup=@vimMenuList
syntax match	vimMenuName	"[^ \t\\<]\+"	contained nextgroup=vimMenuNameMore,vimMenuMap
syntax match	vimMenuPriority	"\d\+\(\.\d\+\)*"	contained skipwhite nextgroup=vimMenuName
syntax match	vimMenuNameMore	"\c\\\s\|<tab>\|\\\."	contained nextgroup=vimMenuName,vimMenuNameMore contains=vimNotation
syntax match	vimMenuMod    contained	"\c<\(script\|silent\)\+>"  skipwhite contains=vimMapModKey,vimMapModErr nextgroup=@vimMenuList
syntax match	vimMenuMap	"\s"	contained skipwhite nextgroup=vimMenuRhs
syntax match	vimMenuRhs	".*$"	contained contains=vimString,vimComment,vimIsCommand
syntax match	vimMenuBang	"!"	contained skipwhite nextgroup=@vimMenuList

" Angle-Bracket Notation (tnx to Michael Geddes) {{{2
" ======================
syntax case ignore
syntax match	vimNotation	"\(\\\|<lt>\)\=<\([scamd]-\)\{0,4}x\=\(f\d\{1,2}\|[^ \t:]\|cr\|lf\|linefeed\|return\|k\=del\%[ete]\|bs\|backspace\|tab\|esc\|right\|left\|help\|undo\|insert\|ins\|k\=home\|k\=end\|kplus\|kminus\|kdivide\|kmultiply\|kenter\|kpoint\|space\|k\=\(page\)\=\(\|down\|up\|k\d\>\)\)>" contains=vimBracket
syntax match	vimNotation	"\(\\\|<lt>\)\=<\([scam2-4]-\)\{0,4}\(right\|left\|middle\)\(mouse\)\=\(drag\|release\)\=>"	contains=vimBracket
syntax match	vimNotation	"\(\\\|<lt>\)\=<\(bslash\|plug\|sid\|space\|bar\|nop\|nul\|lt\)>"		contains=vimBracket
syntax match	vimNotation	'\(\\\|<lt>\)\=<C-R>[0-9a-z"%#:.\-=]'he=e-1			contains=vimBracket
syntax match	vimNotation	'\(\\\|<lt>\)\=<\%(q-\)\=\(line[12]\|count\|bang\|reg\|args\|f-args\|lt\)>'	contains=vimBracket
syntax match	vimNotation	"\(\\\|<lt>\)\=<\([cas]file\|abuf\|amatch\|cword\|cWORD\|client\)>"		contains=vimBracket
syntax match	vimBracket contained	"[\\<>]"
syntax case match

" User Function Highlighting {{{2
" (following Gautam Iyer's suggestion)
" ==========================
syntax match vimFunc		"\%(\%([sSgGbBwWtTlL]:\|<[sS][iI][dD]>\)\=\%([a-zA-Z0-9_]\+\.\)*\I[a-zA-Z0-9_.]*\)\ze\s*("		contains=vimFuncName,vimUserFunc,vimExecute
syntax match vimUserFunc contained	"\%(\%([sSgGbBwWtTlL]:\|<[sS][iI][dD]>\)\=\%([a-zA-Z0-9_]\+\.\)*\I[a-zA-Z0-9_.]*\)\|\<\u[a-zA-Z0-9.]*\>\|\<if\>"	contains=vimNotation
syntax match vimNotFunc	"\<if\>\|\<el\%[seif]\>\|\<return\>\|\<while\>"

" Errors And Warnings: {{{2
" ====================
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_novimfunctionerror")
    syntax match	vimFunctionError	"\s\zs[a-z0-9]\i\{-}\ze\s*("			contained contains=vimFuncKey,vimFuncBlank
    " syntax match	vimFunctionError	"\s\zs\%(<[sS][iI][dD]>\|[sSgGbBwWtTlL]:\)[0-9]\i\{-}\ze\s*("	contained contains=vimFuncKey,vimFuncBlank
    syntax match	vimElseIfErr	"\<else\s\+if\>"
    syntax match	vimBufnrWarn	/\<bufnr\s*(\s*["']\.['"]\s*)/
endif

" Norm {{{2
" ====
syntax match	vimNorm		"\<norm\%[al]!\=" skipwhite nextgroup=vimNormCmds
syntax match	vimNormCmds contained	".*$"

" Syntax {{{2
"=======
syntax match	vimGroupList	contained	"@\=[^ \t,]*"	contains=vimGroupSpecial,vimPatSep
syntax match	vimGroupList	contained	"@\=[^ \t,]*,"	nextgroup=vimGroupList contains=vimGroupSpecial,vimPatSep
syntax keyword	vimGroupSpecial	contained	ALL	ALLBUT	CONTAINED	TOP
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_novimsynerror")
    syntax match	vimSynError	contained	"\i\+"
    syntax match	vimSynError	contained	"\i\+="	nextgroup=vimGroupList
endif
syntax match	vimSynContains	contained	"\<contain\(s\|edin\)="	nextgroup=vimGroupList
syntax match	vimSynKeyContainedin	contained	"\<containedin="	nextgroup=vimGroupList
syntax match	vimSynNextgroup	contained	"nextgroup="	nextgroup=vimGroupList

syntax match	vimSyntax	"\<sy\%[ntax]\>"	contains=vimCommand skipwhite nextgroup=vimSynType,vimComment
syntax match	vimAuSyntax	contained	"\s+sy\%[ntax]"	contains=vimCommand skipwhite nextgroup=vimSynType,vimComment
syntax cluster vimFuncBodyList add=vimSyntax

" Syntax: case {{{2
syntax keyword	vimSynType	contained	case	skipwhite nextgroup=vimSynCase,vimSynCaseError
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_novimsyncaseerror")
    syntax match	vimSynCaseError	contained	"\i\+"
endif
syntax keyword	vimSynCase	contained	ignore	match

" Syntax: clear {{{2
syntax keyword	vimSynType	contained	clear	skipwhite nextgroup=vimGroupList

" Syntax: cluster {{{2
syntax keyword	vimSynType	contained	cluster	skipwhite nextgroup=vimClusterName
syntax region	vimClusterName	contained	matchgroup=vimGroupName start="\h\w*" skip="\\\\\|\\|" matchgroup=vimSep end="$\||" contains=vimGroupAdd,vimGroupRem,vimSynContains,vimSynError
syntax match	vimGroupAdd	contained	"add="	nextgroup=vimGroupList
syntax match	vimGroupRem	contained	"remove="	nextgroup=vimGroupList
syntax cluster vimFuncBodyList add=vimSynType,vimGroupAdd,vimGroupRem

" Syntax: include {{{2
syntax keyword	vimSynType	contained	include	skipwhite nextgroup=vimGroupList
syntax cluster vimFuncBodyList add=vimSynType

" Syntax: keyword {{{2
syntax cluster	vimSynKeyGroup	contains=vimSynNextgroup,vimSynKeyOpt,vimSynKeyContainedin
syntax keyword	vimSynType	contained	keyword	skipwhite nextgroup=vimSynKeyRegion
syntax region	vimSynKeyRegion	contained oneline keepend	matchgroup=vimGroupName start="\h\w*" skip="\\\\\|\\|" matchgroup=vimSep end="|\|$" contains=@vimSynKeyGroup
syntax match	vimSynKeyOpt	contained	"\<\(conceal\|contained\|transparent\|skipempty\|skipwhite\|skipnl\)\>"
syntax cluster vimFuncBodyList add=vimSynType

" Syntax: match {{{2
syntax cluster	vimSynMtchGroup	contains=vimMtchComment,vimSynContains,vimSynError,vimSynMtchOpt,vimSynNextgroup,vimSynRegPat,vimNotation
syntax keyword	vimSynType	contained	match	skipwhite nextgroup=vimSynMatchRegion
syntax region	vimSynMatchRegion	contained keepend	matchgroup=vimGroupName start="\h\w*" matchgroup=vimSep end="|\|$" contains=@vimSynMtchGroup
syntax match	vimSynMtchOpt	contained	"\<\(conceal\|transparent\|contained\|excludenl\|skipempty\|skipwhite\|display\|extend\|skipnl\|fold\)\>"
if has("conceal")
    syntax match	vimSynMtchOpt	contained	"\<cchar="	nextgroup=vimSynMtchCchar
    syntax match	vimSynMtchCchar	contained	"\S"
endif
syntax cluster vimFuncBodyList add=vimSynMtchGroup

" Syntax: off and on {{{2
syntax keyword	vimSynType	contained	enable	list	manual	off	on	reset

" Syntax: region {{{2
syntax cluster	vimSynRegPatGroup	contains=vimPatSep,vimNotPatSep,vimSynPatRange,vimSynNotPatRange,vimSubstSubstr,vimPatRegion,vimPatSepErr,vimNotation
syntax cluster	vimSynRegGroup	contains=vimSynContains,vimSynNextgroup,vimSynRegOpt,vimSynReg,vimSynMtchGrp
syntax keyword	vimSynType	contained	region	skipwhite nextgroup=vimSynRegion
syntax region	vimSynRegion	contained keepend	matchgroup=vimGroupName start="\h\w*" skip="\\\\\|\\|" end="|\|$" contains=@vimSynRegGroup
syntax match	vimSynRegOpt	contained	"\<\(conceal\(ends\)\=\|transparent\|contained\|excludenl\|skipempty\|skipwhite\|display\|keepend\|oneline\|extend\|skipnl\|fold\)\>"
syntax match	vimSynReg	contained	"\(start\|skip\|end\)="he=e-1	nextgroup=vimSynRegPat
syntax match	vimSynMtchGrp	contained	"matchgroup="	nextgroup=vimGroup,vimHLGroup
syntax region	vimSynRegPat	contained extend	start="\z([-`~!@#$%^&*_=+;:'",./?]\)"  skip="\\\\\|\\\z1"  end="\z1"  contains=@vimSynRegPatGroup skipwhite nextgroup=vimSynPatMod,vimSynReg
syntax match	vimSynPatMod	contained	"\(hs\|ms\|me\|hs\|he\|rs\|re\)=[se]\([-+]\d\+\)\="
syntax match	vimSynPatMod	contained	"\(hs\|ms\|me\|hs\|he\|rs\|re\)=[se]\([-+]\d\+\)\=," nextgroup=vimSynPatMod
syntax match	vimSynPatMod	contained	"lc=\d\+"
syntax match	vimSynPatMod	contained	"lc=\d\+," nextgroup=vimSynPatMod
syntax region	vimSynPatRange	contained	start="\["	skip="\\\\\|\\]"   end="]"
syntax match	vimSynNotPatRange	contained	"\\\\\|\\\["
syntax match	vimMtchComment	contained	'"[^"]\+$'
syntax cluster vimFuncBodyList add=vimSynType

" Syntax: sync {{{2
" ============
syntax keyword vimSynType	contained	sync	skipwhite	nextgroup=vimSyncC,vimSyncLines,vimSyncMatch,vimSyncError,vimSyncLinebreak,vimSyncLinecont,vimSyncRegion
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_novimsyncerror")
    syntax match	vimSyncError	contained	"\i\+"
endif
syntax keyword	vimSyncC	contained	ccomment	clear	fromstart
syntax keyword	vimSyncMatch	contained	match	skipwhite	nextgroup=vimSyncGroupName
syntax keyword	vimSyncRegion	contained	region	skipwhite	nextgroup=vimSynReg
syntax match	vimSyncLinebreak	contained	"\<linebreaks="	skipwhite	nextgroup=vimNumber
syntax keyword	vimSyncLinecont	contained	linecont	skipwhite	nextgroup=vimSynRegPat
syntax match	vimSyncLines	contained	"\(min\|max\)\=lines="	nextgroup=vimNumber
syntax match	vimSyncGroupName	contained	"\h\w*"	skipwhite	nextgroup=vimSyncKey
syntax match	vimSyncKey	contained	"\<groupthere\|grouphere\>"	skipwhite nextgroup=vimSyncGroup
syntax match	vimSyncGroup	contained	"\h\w*"	skipwhite	nextgroup=vimSynRegPat,vimSyncNone
syntax keyword	vimSyncNone	contained	NONE

" Additional IsCommand, here by reasons of precedence {{{2
" ====================
syntax match	vimIsCommand	"<Bar>\s*\a\+"	transparent contains=vimCommand,vimNotation

" Highlighting {{{2
" ============
syntax cluster	vimHighlightCluster		contains=vimHiLink,vimHiClear,vimHiKeyList,vimComment
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_novimhictermerror")
    syntax match	vimHiCtermError	contained	"[^0-9]\i*"
endif
syntax match	vimHighlight	"\<hi\%[ghlight]\>"	skipwhite nextgroup=vimHiBang,@vimHighlightCluster
syntax match	vimHiBang	contained	"!"	skipwhite nextgroup=@vimHighlightCluster

syntax match	vimHiGroup	contained	"\i\+"
syntax case ignore
syntax keyword	vimHiAttrib	contained	none bold inverse italic reverse standout underline undercurl
syntax keyword	vimFgBgAttrib	contained	none bg background fg foreground
syntax case match
syntax match	vimHiAttribList	contained	"\i\+"	contains=vimHiAttrib
syntax match	vimHiAttribList	contained	"\i\+,"he=e-1	contains=vimHiAttrib nextgroup=vimHiAttribList
syntax case ignore
syntax keyword	vimHiCtermColor	contained	black blue brown cyan darkblue darkcyan darkgray darkgreen darkgrey darkmagenta darkred darkyellow gray green grey lightblue lightcyan lightgray lightgreen lightgrey lightmagenta lightred magenta red white yellow
syntax match	vimHiCtermColor	contained	"\<color\d\{1,3}\>"

syntax case match
syntax match	vimHiFontname	contained	"[a-zA-Z\-*]\+"
syntax match	vimHiGuiFontname	contained	"'[a-zA-Z\-* ]\+'"
syntax match	vimHiGuiRgb	contained	"#\x\{6}"

" Highlighting: hi group key=arg ... {{{2
syntax cluster	vimHiCluster contains=vimGroup,vimHiGroup,vimHiTerm,vimHiCTerm,vimHiStartStop,vimHiCtermFgBg,vimHiGui,vimHiGuiFont,vimHiGuiFgBg,vimHiKeyError,vimNotation
syntax region	vimHiKeyList	contained oneline start="\i\+" skip="\\\\\|\\|" end="$\||"	contains=@vimHiCluster
if !exists("g:vimsyn_noerror") && !exists("g:vimsyn_vimhikeyerror")
    syntax match	vimHiKeyError	contained	"\i\+="he=e-1
endif
syntax match	vimHiTerm	contained	"\cterm="he=e-1		nextgroup=vimHiAttribList
syntax match	vimHiStartStop	contained	"\c\(start\|stop\)="he=e-1	nextgroup=vimHiTermcap,vimOption
syntax match	vimHiCTerm	contained	"\ccterm="he=e-1		nextgroup=vimHiAttribList
syntax match	vimHiCtermFgBg	contained	"\ccterm[fb]g="he=e-1	nextgroup=vimHiNmbr,vimHiCtermColor,vimFgBgAttrib,vimHiCtermError
syntax match	vimHiGui	contained	"\cgui="he=e-1		nextgroup=vimHiAttribList
syntax match	vimHiGuiFont	contained	"\cfont="he=e-1		nextgroup=vimHiFontname
syntax match	vimHiGuiFgBg	contained	"\cgui\%([fb]g\|sp\)="he=e-1	nextgroup=vimHiGroup,vimHiGuiFontname,vimHiGuiRgb,vimFgBgAttrib
syntax match	vimHiTermcap	contained	"\S\+"		contains=vimNotation
syntax match	vimHiNmbr	contained	'\d\+'

" Highlight: clear {{{2
syntax keyword	vimHiClear	contained	clear	nextgroup=vimHiGroup

" Highlight: link {{{2
syntax region	vimHiLink	contained oneline matchgroup=vimCommand start="\(\<hi\%[ghlight]\s\+\)\@<=\(\(def\%[ault]\s\+\)\=link\>\|\<def\>\)" end="$"	contains=vimHiGroup,vimGroup,vimHLGroup,vimNotation
syntax cluster vimFuncBodyList add=vimHiLink

" Control Characters {{{2
" ==================
syntax match	vimCtrlChar	"[--]"

" Beginners - Patterns that involve ^ {{{2
" =========
syntax match	vimLineComment	+^[ \t:]*".*$+	contains=@vimCommentGroup,vimCommentString,vimCommentTitle
syntax match	vimCommentTitle	'"\s*\%([sS]:\|\h\w*#\)\=\u\w*\(\s\+\u\w*\)*:'hs=s+1	contained contains=vimCommentTitleLeader,vimTodo,@vimCommentGroup
syntax match	vimContinue	"^\s*\\"
syntax region	vimString	start="^\s*\\\z(['"]\)" skip='\\\\\|\\\z1' end="\z1" oneline keepend contains=@vimStringGroup,vimContinue
syntax match	vimCommentTitleLeader	'"\s\+'ms=s+1	contained

" Searches And Globals: {{{2
" ====================
syntax match	vimSearch	'^\s*[/?].*'		contains=vimSearchDelim
syntax match	vimSearchDelim	'^\s*\zs[/?]\|[/?]$'	contained
syntax region	vimGlobal	matchgroup=Statement start='\<g\%[lobal]!\=/'  skip='\\.' end='/'	skipwhite nextgroup=vimSubst
syntax region	vimGlobal	matchgroup=Statement start='\<v\%[global]!\=/' skip='\\.' end='/'	skipwhite nextgroup=vimSubst

" Scripts  : perl,ruby : Benoit Cerrina {{{2
" =======    python,tcl: Johannes Zellner
"            lua

" Allows users to specify the type of embedded script highlighting
" they want:  (perl/python/ruby/tcl support)
"   g:vimsyn_embed == 0   : don't embed any scripts
"   g:vimsyn_embed ~= 'l' : embed lua
"   g:vimsyn_embed ~= 'm' : embed mzscheme
"   g:vimsyn_embed ~= 'p' : embed perl
"   g:vimsyn_embed ~= 'P' : embed python
"   g:vimsyn_embed ~= 'r' : embed ruby
"   g:vimsyn_embed ~= 't' : embed tcl
if has("win32") || has("win95") || has("win64") || has("win16")
    " apparently has("tcl") has been hanging vim on some windows systems with cygwin
    let s:trytcl= (&shell !~ '\<\%(bash\>\|4[nN][tT]\|\<zsh\)\>\%(\.exe\)\=$')
else
    let s:trytcl= 1
endif
if !exists("g:vimsyn_embed")
    let g:vimsyn_embed= ""
    if has("lua")        |let g:vimsyn_embed= g:vimsyn_embed."l"|endif
    if has("mzscheme")   |let g:vimsyn_embed= g:vimsyn_embed."m"|endif
    if has("perl")       |let g:vimsyn_embed= g:vimsyn_embed."p"|endif
    if     has("python") |let g:vimsyn_embed= g:vimsyn_embed."P"
    elseif has("python3")|let g:vimsyn_embed= g:vimsyn_embed."P"|endif
    if has("ruby")       |let g:vimsyn_embed= g:vimsyn_embed."r"|endif
    if s:trytcl         
        if has("tcl")       |let g:vimsyn_embed= g:vimsyn_embed."t"|endif
    endif
endif
unlet s:trytcl

" Synchronize (speed) {{{2
"============
syntax sync maxlines=60
syntax sync linecont "^\s\+\\"
syntax sync match vimAugroupSyncA groupthere NONE "\<aug\%[roup]\>\s\+[eE][nN][dD]"

" Current Syntax Variable: {{{2
