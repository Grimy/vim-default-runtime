
" Emulate default behaviour
if !exists("did_load_filetypes")
	augroup FileTypeDetect
		autocmd!
		autocmd BufWritePost * if &l:filetype ==# '' | filetype detect | endif
		runtime! ftdetect/*.vim
	augroup END

	augroup UpdateFileType
		autocmd!
		autocmd FileType * for name in split(expand('<amatch>'), '\.')
		autocmd FileType *     execute 'runtime! syntax/'   . name . '.vim'
		autocmd FileType *     execute 'runtime! indent/'   . name . '.vim'
		autocmd FileType *     execute 'runtime! ftplugin/' . name . '.vim'
		autocmd FileType * endfor
	augroup END

	let g:did_load_filetypes = 1
	let g:did_load_ftplugin  = 1
endif
