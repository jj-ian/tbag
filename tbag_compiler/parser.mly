%{ open Ast %}

%%

prog:
	decls EOF {$1}

decls:
	/* nothing */ { [], [] }
	| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNC ID WITH formals_opt LBRACE /*vdecl_list*/ stmt_list RBRACE
     { { fname = $2;
	 formals = $4;
	 locals = List.rev $6;
	 body = List.rev $7 } }

/*
fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }
*/
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

