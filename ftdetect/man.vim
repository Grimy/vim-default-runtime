autocmd BufNewFile,BufRead,StdinReadPost * call s:DetectManPage()

function! s:DetectManPage()
	if getline(1) =~ '\v^(.+\(..?\)).*\1$'
		setfiletype man
	endif
endfunction
