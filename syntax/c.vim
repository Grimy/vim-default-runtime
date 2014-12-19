syn keyword Type if else switch case default do while for break continue goto return
syn keyword Type void char short int long float double enum struct union typedef
syn keyword Type auto register const extern signed sizeof static unsigned volatile
syn keyword Type restrict inline _Bool _Imaginary _Complex
syn region Comment start='\V/*' skip='\\.' end='\V*/'
syn region Comment start='//'   skip='\\$' end='$'
syn region String  start='"'    skip='\\.' end='\v"|\n'
silent call rainbow#toggle()
