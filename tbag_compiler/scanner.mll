{ open Parser }

rule token = parse
         [' ' '\t' '\r' '\n'] { token lexbuf }          (* Whitespce *)
        | "/*"                  { comment lexbuf } 
        | "func"                  { FUNC }
        | "room"                { ROOM }
        | '('                   { LPAREN }
        | ')'                   { RPAREN }
        | '{'                   { LBRACE }
        | '}'                   { RBRACE }
        | '='                   { ASSIGN }  
        | ';'					{ SEMI }
        | "int"					{ INT }         (* types  *)
        (*| "string"                              { STRLIT } what???*)
        | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
        | eof                   { EOF }

        and comment = parse 
        "*/" { token lexbuf }   (* End of comment *)
        | _ { comment lexbuf }  (* eat everything else *)
