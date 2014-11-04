syntax region String start='/' end='/' oneline
syntax match String /'[^']\{-}'/
syntax match String /"[^"]\{-}"/
syntax match Comment /^[ \t:]*".*$/
