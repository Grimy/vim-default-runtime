autocmd BufNewFile,BufRead,StdinReadPost * call s:DetectManPage()

function! s:DetectManPage()
	if getline(1) =~ '\v^([a-zA-Z0-9_-]+\(\d\)).*\1$'
		setfiletype man
	endif
endfunction
