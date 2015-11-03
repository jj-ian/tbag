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
        (*| "string"                            { STRLIT } what???*)
        | ['0'-'9']+                            as lxm { LITERAL(int_of_string lxm) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*       as lxm { ID(lxm) }
        | eof                                   { EOF }

        and comment = parse 
        "*/" { token lexbuf }   (* End of comment *)
        | _ { comment lexbuf }  (* eat everything else *)

{
let _ =
let lexbuf = Lexing.from_channel stdin in
match token lexbuf with
(* |FLOATINGPOINT(fp) -> print_endline (string_of_float fp ^ " is a valid floating point number") *)
 | FUNC -> print_endline ("funky")
 (*| BADFLOAT -> print_endline "not valid floating point number"*)
 | EOF -> print_endline "wwwwwww\nd 0 0 b\n|  j  |\n| \\_/ |\n \\___/"

}