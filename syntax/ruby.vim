syn keyword PreProc BEGIN END __ENCODING__ __END__ __FILE__ __LINE__
syn keyword Type alias and begin class def defined? false
syn keyword Type do while until for if unless else elsif end ensure next redo break case when
syn keyword Type in module nil not or rescue retry return self super then true undef
syn region String start=/"/ skip=/\\./ end=/\v"|\n/
syn region String start=/'/ skip=/\\./ end=/'/
syn region String start='/' skip='\\.' end='/[a-z]*' oneline
syn match Comment /#.*/
