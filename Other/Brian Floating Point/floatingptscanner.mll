{ type token = EOF | FLOATINGPOINT of float | BADFLOAT}

let number = ['0'-'9']
let frac = '.' number*
let exp = 'e' ('+'|'-')? number+
let float_to_parse = '-'? ((number* frac exp?) | (number exp))

rule token = parse
 [' ' '\t' '\r' '\n'] { token lexbuf }
| float_to_parse as fp { FLOATINGPOINT(float_of_string fp)}
| eof { EOF }
| _ { BADFLOAT }

{
let _ =
let lexbuf = Lexing.from_channel stdin in
match token lexbuf with
 |FLOATINGPOINT(fp) -> print_endline (string_of_float fp ^ " is a valid floating point number")
 | BADFLOAT -> print_endline "not valid floating point number"
 | EOF -> print_endline "wwwwwww\nd 0 0 b\n|  j  |\n| \\_/ |\n \\___/"

}