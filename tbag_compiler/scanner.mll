{ open Parser }

rule token = parse
        "func"                  { FUNC }
        | "room"                { ROOM }
        | '('                   { LPAREN }
        | ')'                   { RPAREN }
        | '{'                   { LBRACE }
        | '}'                   { RBRACE }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
        | eof                   { EOF }
