{ open Parser }

rule token = parse
        [' ' '\t' '\r' '\n']    { token lexbuf }
        | "/*"                  { comment lexbuf }
        | '('   { LPAREN }      | ')'   { RPAREN }
        | '{'   { LBRACE }      | '}'   { RBRACE }
        | ';'   { SEMI }        | ','   { COMMA }
        | '+'   { PLUS }        | '-'   { MINUS }
        | '*'   { TIMES }       | '/'   { DIVIDE }
        | '='   { ASSIGN }      | "=="  { EQ }
        | "!="  { NEQ }         | '<'   { LT }
        | "<="  { LEQ }         | ">"   { GT }
        | ">="  { GEQ }
        | "else" { ELSE }       | "if"  { IF } (* Keywords *)
        | "while" { WHILE }     | "for" { FOR }
        | "int" { INT }         | "return" { RETURN }
        | eof   { EOF }
        | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
        | _ as char { raise (Failure("illegal character " ^ Char.excaped char)) }

and comment = parse
        "*/"    { token lexbuf }
        | _     { comment lexbuf }

