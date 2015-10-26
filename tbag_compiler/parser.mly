%{ open Ast %}
%token LPAREN RPAREN LBRACE RBRACE COMMA FUNC
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%%

program:
	decls EOF {$1}

decls:
	/* nothing */ { [], [] }
	| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNC ID LPAREN formals_opt RPAREN LBRACE /*vdecl_list stmt_list*/ RBRACE
     { { fname = $2;
	 formals = $4;
	 } }

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



