autocmd BufRead,BufNewFile *.fish setf fish

" Detect fish scripts by the shebang line.
autocmd BufRead * if getline(1) =~# '\v^#!.*\<fish$' | setf fish | endif

" Fish histories are YAML documents.
autocmd BufRead,BufNewFile ~/.config/fish/fish_{read_,}history setf yaml

" Universal variable storages should not be hand edited.
autocmd BufRead,BufNewFile ~/.config/fish/fishd.* setlocal readonly
