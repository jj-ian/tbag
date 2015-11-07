{ open Parser }

rule token = parse
         [' ' '\t' '\r' '\n'] { token lexbuf }          (* Whitespce *)
        | "/*"                                  { comment lexbuf } 
        | "func"                                { FUNC }
        | "room"                                { ROOM }
        | '('                                   { LPAREN }
        | ')'                                   { RPAREN }
        | '{'                                   { LBRACE }
        | '}'                                   { RBRACE }
        | '+'                                   { PLUS }        (* operators*)
        | '-'                                   { MINUS }       
        | '*'                                   { TIMES }
        | '/'                                   { DIVIDE } 
        | "=="                                  { EQ }
        | "!="                                  { NEQ }
        | '<'                                   { LT }
        | "<="                                  { LEQ }
        | '>'                                   { GT }
        | ">="                                  { GEQ }
        | '='                                   { ASSIGN }  
        | ';'					{ SEMI }
        | "int"					{ INT }         (* types  *)
        | "if"                                  { IF }
        | "else"                                { ELSE }
        | "while"                               { WHILE }
        | "return"                              { RETURN }
        (*| "string"                            { STRLIT } what???*)
        | ['0'-'9']+                            as lxm { INT_LITERAL(int_of_string lxm) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*       as lxm { ID(lxm) }
        | eof                                   { EOF }

        and comment = parse 
        "*/" { token lexbuf }   (* End of comment *)
        | _ { comment lexbuf }  (* eat everything else *)


{
    
let _ =
let lexbuf = Lexing.from_channel stdin in
try
    while true do
        match token lexbuf with
         | FUNC -> print_string ("FUNC ")
         | ROOM -> print_string ("ROOM ")
         | LPAREN -> print_string ("LPAREN ")
         | RPAREN -> print_string ("RPAREN ")
         | LBRACE -> print_string ("LBRACE ")
         | RBRACE -> print_string ("RBRACE ")
         | PLUS -> print_string ("PLUS ")
         | MINUS -> print_string ("MINUS ")
         | TIMES -> print_string ("TIMES ")
         | DIVIDE -> print_string ("DIVIDE ")
         | EQ -> print_string ("EQ ")
         | NEQ -> print_string ("NEQ ")
         | LT -> print_string ("LT ")
         | LEQ -> print_string ("LEQ ")
         | GT -> print_string ("GT ")
         | GEQ -> print_string ("GEQ ")
         | INT_LITERAL _ -> print_string("INT_LITERAL ")
         (*| lxm -> print_string ("LITERAL ")*)
         | EOF -> print_endline "wwwwwww\nd 0 0 b\n|  j  |\n| \\_/ |\n \\___/"
    done
 with _-> exit 0

}
