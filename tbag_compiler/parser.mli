type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | FUNC
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
