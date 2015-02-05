let g:did_load_filetypes = 1
let g:did_load_ftplugin  = 1

" Emulate default behaviour
augroup FileTypeDetect
	autocmd!
	autocmd BufWritePost * if &l:filetype ==# '' | filetype detect | endif
	runtime! ftdetect/*.vim
augroup END

augroup UpdateFileType
	autocmd!
	autocmd FileType * syntax clear
	autocmd FileType * augroup FileTypePlugin
	autocmd FileType *     autocmd!
	autocmd FileType *     for name in split(expand('<amatch>'), '\.')
	autocmd FileType *         execute 'runtime syntax/'   . name . '.vim'
	autocmd FileType *         execute 'runtime indent/'   . name . '.vim'
	autocmd FileType *         execute 'runtime ftplugin/' . name . '.vim'
	autocmd FileType *     endfor
	autocmd FileType *     execute 'runtime ftplugin/after.vim'
	autocmd FileType * augroup END
augroup END
