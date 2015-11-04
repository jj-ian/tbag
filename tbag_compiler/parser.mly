%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA FUNC ROOM 
%token ASSIGN EQ NEQ LT LEQ GT GEQ IF ELSE (*WHILE*) 
%token PLUS MINUS TIMES DIVIDE
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
        rdecl fdecl EOF {$1, $2}

/*
decls:
	 { [], [] }
	| decls fdecl { fst $1, ($2 :: snd $1) }
        | decls rdecl { fst $1, ($2 :: snd $1) }
        | decls vdecl { ($2 :: fst $1), snd $1 }*/


fdecl:
   FUNC ID LPAREN formals_opt RPAREN LBRACE /*vdecl_list*/ stmt_list RBRACE
     { { fname = $2;
	 formals = $4;
   body = List.rev $7
	 } }

rdecl:
   ROOM ID LBRACE /*assignments*/ RBRACE
     { { rname = $2;
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

stmt_list:
        /* nothing */  { [] }
        | stmt_list stmt { $2 :: $1 }

stmt:
        expr SEMI { Expr($1) }
        | IF LPAREN expr RPAREN LBRACE stmt_list SEMI RBRACE ELSE LBRACE
        stmt_list SEMI
        RBRACE { If($3,
        Block(List.rev $6), Block(List.rev $11)) }

expr:
        LITERAL          { Literal($1) }
        | ID               { Id($1) }
        | ID ASSIGN expr   { Assign($1, $3) }

/*
vdecl_list:
        nothing     { [] }
        | vdecl_list vdecl { $2 :: $1 }

vdecl:
        INT ID SEMI { $2 }*/



