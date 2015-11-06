%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA FUNC ROOM
%token ASSIGN EQ NEQ LT LEQ GT GEQ
%token PLUS MINUS TIMES DIVIDE
%token IF ELSE WHILE RETURN
%token INT
%token <int> LITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
        /*decls EOF {$1}*/
        rdecl_list fdecl EOF {$1, $2}

/*
decls:
	 { [], [] }
	| decls fdecl { fst $1, ($2 :: snd $1) }
        | decls rdecl { fst $1, ($2 :: snd $1) }
        | decls vdecl { ($2 :: fst $1), snd $1 }*/


fdecl:
        FUNC ID LPAREN formals_opt RPAREN LBRACE /*vdecl_list*/ stmt_list RBRACE
        { {     fname = $2;
	        formals = $4;
                body = List.rev $7      } }

rdecl_list:
        rdecl rdecl             { [$1; $2] }
        | rdecl_list rdecl      { $2 :: $1 }

rdecl:
        ROOM ID LBRACE stmt_list RBRACE
        { {     rname = $2;
                body = List.rev $4      } }

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
        ID                      { [$1] }
        | formal_list COMMA ID  { $3 :: $1 }

stmt_list:
        /* nothing */           { [] }
        | stmt_list stmt        { $2 :: $1 }

stmt:
        expr SEMI { Expr($1) }
        | RETURN expr SEMI { Return($2) }
        | LBRACE stmt_list RBRACE { Block(List.rev $2) }
        | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
        | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr:
        LITERAL                 { Literal($1) }
        | ID                    { Id($1) }
        | expr PLUS expr        { Binop($1, Add, $3) }
        | expr MINUS expr       { Binop($1, Sub, $3) }
        | expr TIMES expr       { Binop($1, Mult, $3) }
        | expr DIVIDE expr      { Binop($1, Div, $3) }
        | expr EQ expr          { Binop($1, Equal, $3) }
        | expr NEQ expr         { Binop($1, Neq, $3) }
        | expr LT expr          { Binop($1, Less, $3) }
        | expr LEQ expr         { Binop($1, Leq, $3) }
        | expr GT expr          { Binop($1, Greater, $3) }
        | expr GEQ expr         { Binop($1, Geq, $3) }
        | ID ASSIGN expr   { Assign($1, $3) }

/*
vdecl_list:
        nothing                 { [] }
        | vdecl_list vdecl      { $2 :: $1 }

vdecl:
        INT ID SEMI             { $2 }*/



