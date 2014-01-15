" Generic configuration file (check this last, it's just guessing!)
au filetypedetect BufNewFile,BufRead,StdinReadPost *
			\ if expand("<amatch>") !~ g:ft_ignore_pat
			\    && (getline(1) =~ '^#' || getline(2) =~ '^#' || getline(3) =~ '^#'
			\    ||  getline(4) =~ '^#' || getline(5) =~ '^#') |
			\       setf conf |
			\ endif

