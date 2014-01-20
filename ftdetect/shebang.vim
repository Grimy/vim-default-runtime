autocmd BufRead,BufNewFile * call s:detect_shebang()

let g:filetype_for_executable = {}
let g:filetype_for_executable['node'] = 'javascript'

function! s:detect_shebang()
	if &l:filetype !=# ''    " Don’t override an existing filetype
		return
	endif
	let groups = matchlist(getline(1), '\v^#!\f+/(\f+)\s*(\f*)')
	if !len(groups)  " No match
		return
	endif
	let executable = groups[1] ==# 'env' ? groups[2] : groups[1]
	let &l:filetype = get(g:filetype_for_executable, executable, executable)
endfunction
