%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA 
%token FUNC ROOM ADJ ITEM NPC
%token ASSIGN EQ NEQ LT LEQ GT GEQ
%token PLUS MINUS TIMES DIVIDE
%token IF ELSE WHILE RETURN
%token INT STRING VOID
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

/*
%start basic_program
%type <Ast.basic_program> basic_program
*/


%start simple_program
%type <Ast.simple_program> simple_program

/*
%start complex_program
%type <Ast.complex_program> complex_program
*/  
%%

basic_program:
    fdecl_list EOF { $1 }

simple_program:
        rdecl_list fdecl_list EOF {$1, $2}

complex_program:
        rdecl_list adecl_list ndecl_list idecl_list fdecl_list EOF {$1, $2, $3,
        $4, $5}

data_type:
        INT             { Int }
        | STRING        { String }
        /* | data_type LBRACK RBRACK { Array( type_of_string $1) } */
        | VOID          { Void }
        | INT LBRACK int_opt RBRACK     { Array(Int, $3) }
        | STRING LBRACK int_opt RBRACK  { Array(String, $3) }

fdecl_list:
        /* nothing */           {[]}
        | fdecl_list fdecl      { $2 :: $1 }

fdecl:
        FUNC data_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
        { { 
                freturntype = $2;    
                fname = $3;
	        formals = $5;
                locals = $8;
                body = List.rev $9      
            } }

formals_opt:
        /* nothing */   { [] }
        | formal_list   { List.rev $1 }

formal_list:
        data_type ID            { [Argument($1, $2)] }
        | formal_list COMMA data_type ID        { Argument($3, $4) :: $1 }

actuals_opt:
        /* nothing */   { [] }
        | actuals_list  { List.rev $1 }

actuals_list:
        expr                        { [$1] }
        | actuals_list COMMA expr   { $3 :: $1 }

vdecl_list:
        /* nothing */           { [] }
        | vdecl_list vdecl      { $2 :: $1 }

vdecl:
        data_type ID SEMI { Var($1, $2) }
        | data_type ID ASSIGN expr SEMI { Var_Init($1, $2, $4) }

rdecl_list:
        rdecl rdecl             { [$1; $2] }
        | rdecl_list rdecl      { $2 :: $1 }

rdecl:
        ROOM ID LBRACE stmt_list RBRACE
        { {     rname = $2;
                body = List.rev $4      } }

adecl_list:
        adecl                   { [$1] }
        | adecl_list adecl      { $2 :: $1 }

adecl:
        ADJ LBRACE adj_list RBRACE SEMI
        { {     body = List.rev $3      } }

adj_list:
        ID COMMA ID { [$1; $3] }

ndecl_list:
        /* nothing */           { [] }
        | ndecl_list ndecl      { $2 :: $1 }

ndecl:
        NPC ID LBRACE stmt_list RBRACE
        {{      nname = $2;
                nbody = List.rev $4       }}

idecl_list:
        /* nothing */ { [] }
        | idecl_list idecl { $2 :: $1 }

idecl:
        ITEM ID LBRACE stmt_list RBRACE
        {{      iname = $2;
                ibody = List.rev $4       }}

stmt_list:
        /* nothing */           { [] }
        | stmt_list stmt        { $2 :: $1 }

stmt:
        expr SEMI { Expr($1) }
        | RETURN expr SEMI { Return($2) }
        | LBRACE stmt_list RBRACE { Block(List.rev $2) }
        | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
        | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

int_opt:
        INT_LITERAL             { $1 }

expr:
        INT_LITERAL             { IntLiteral($1) }
        | STRING_LITERAL        { StrLiteral($1) }
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
        | ID LBRACK INT_LITERAL RBRACK ASSIGN expr { ArrayAssign($1, $3, $6) }
        | ID LBRACK INT_LITERAL RBRACK { ArrayAccess($1, $3) }
        | ID LPAREN actuals_opt RPAREN      { Call ($1, $3) }
        | LPAREN expr RPAREN { $2 }

