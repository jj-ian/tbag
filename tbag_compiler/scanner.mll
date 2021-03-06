(* Authors: All *)

{ open Parser }

rule token = parse
        [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespce *)
        | "/*"                                  { comment lexbuf } 
        | "func"                                { FUNC }
        | "room"                                { ROOM }
        | "start"                               { START }
        | "endgame"                             { END }
        | "<->"                                 { ADJ }
        | "->"                                  { GOTO }
        | "npc"                                 { NPC }
        | "item"                                { ITEM }
        | '('                                   { LPAREN }
        | ')'                                   { RPAREN }
        | '{'                                   { LBRACE }
        | '}'                                   { RBRACE }
        | '['                                   { LBRACK }
        | ']'                                   { RBRACK }
        | ','                                   { COMMA }
        | '+'                                   { PLUS }        (* operators*)
        | '-'                                   { MINUS }       
        | '*'                                   { TIMES }
        | '/'                                   { DIVIDE } 
        | "=="                                  { EQ }
        | "~~"                                  { STREQ }
        | "!="                                  { NEQ }
        | '<'                                   { LT }
        | "<="                                  { LEQ }
        | '>'                                   { GT }
        | ">="                                  { GEQ }
        | '='                                   { ASSIGN }
        | ';'					{ SEMI }
        | '.'                                   { ACCESS }
        | "int"					{ INT }
        | "string"                              { STRING }
        | "neg"                                 { NEG }
        | "void"                                { VOID }
        | "boolean"                             { BOOLEAN }
        | "if"                                  { IF }
        | "else"                                { ELSE }
        | "while"                               { WHILE }
        | "return"                              { RETURN }
        | "AND"                                 { AND }
        | "OR"                                  { OR }
        | "NOT"                                 { NOT }
        | "true"                                as lxm { BOOL_LITERAL(bool_of_string lxm) }
        | "false"                               as lxm { BOOL_LITERAL(bool_of_string lxm) }
        | ['0'-'9']+                            as lxm { INT_LITERAL(int_of_string lxm) }
        | '"'('\\'_|[^'"'])*'"'                 as str { STRING_LITERAL(str) }
        | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*       as lxm { ID(lxm) }
        | eof                                   { EOF }

        and comment = parse 
        "*/"                            { token lexbuf }   (* End of comment *)
        | _                             { comment lexbuf }  (* eat everything else *)

(*
{
    
let _ =
let lexbuf = Lexing.from_channel stdin in
try
    while true do
        match token lexbuf with
         | FUNC -> print_string ("FUNC ")
         | ROOM -> print_string ("ROOM ")
         | ADJ -> print_string ("ADJ ")    
         | ADJ2 -> print_string ("working")     
         | NPC -> print_string ("NPC ")
         | ITEM -> print_string ("ITEM ")
         | LPAREN -> print_string ("LPAREN ")
         | RPAREN -> print_string ("RPAREN ")
         | LBRACE -> print_string ("LBRACE ")
         | RBRACE -> print_string ("RBRACE ")
         | LBRACK -> print_string ("LBRACK ")
         | RBRACK -> print_string ("RBRACK ")         
         | COMMA -> print_string ("COMMA ") 
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
         | ASSIGN -> print_string ("ASSIGN ")
         | SEMI -> print_string ("SEMI ")
         | INT -> print_string ("INT ")
         | STRING -> print_string ("STRING ")

         | IF -> print_string ("IF ")
         | ELSE -> print_string ("ELSE ")
         | WHILE -> print_string ("WHILE ")
         | RETURN -> print_string ("RETURN ")

         | INT_LITERAL _ -> print_string("INT_LITERAL ")
         | STRING_LITERAL _ -> print_string("STRING_LITERAL ")
         | ID _ -> print_string("ID ")
         | EOF -> print_endline "\nwwwwwww\nd 0 0 b\n|  j  |\n| \\_/ |\n \\___/"
         | _ -> print_string("||put this token in the token printer|| ") (* this happens when there's some rule matched by the scanner above that isn't printed out here. add the rule to the pattern match here *)
    done
 with _-> exit 0

}

*)
