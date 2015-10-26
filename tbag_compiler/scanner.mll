{ open Parser }

rule token = parse
        "func"                  { FUNC }
        | "room"                { ROOM }
        | '('                   { LPAREN }
        | ')'                   { RPAREN }
        | '{'                   { LBRACE }
        | '}'                   { RBRACE }
        | ';'					{ SEMI }
        | "int"					{ INT }
        | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
        | eof                   { EOF }
