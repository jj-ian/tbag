#!/bin/sh
#
echo '
    let my_engine c d lexbuf =
        let res = Lexing.engine c d lexbuf in
        Printf.printf "Saw token [%s]'\\\\'n" (Lexing.lexeme lexbuf);
        res
'
sed 's/Lexing\.engine/my_engine/g' "$@"