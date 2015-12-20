/* Authors: All */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA 
%token FUNC ROOM ADJ GOTO ITEM NPC START END NEG
%token ASSIGN EQ STREQ NEQ LT LEQ GT GEQ AND OR NOT ACCESS
%token PLUS MINUS TIMES DIVIDE
%token IF ELSE WHILE RETURN
%token INT STRING VOID BOOLEAN
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ STREQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left ACCESS

%start program
%type <Ast.program> program

%%


program:
        /* rooms, npcs, items */
        rdef rdecl_list adecl_list start ndef ndecl_list idef idecl_list vdecl_list
        predicate_list fdecl_list EOF         
                { ($1, $2, $3, $4, $5, $6, $7, $8, $9, List.rev $10, List.rev $11) }
        | /* rooms, npcs, !items */
        rdef rdecl_list adecl_list start ndef ndecl_list vdecl_list
        predicate_list fdecl_list EOF
                { ($1, $2, $3, $4, $5, $6, [], [], $7, List.rev $8, List.rev $9) }      
        | /* rooms, !npcs, items */
        rdef rdecl_list adecl_list start idef idecl_list vdecl_list
        predicate_list fdecl_list EOF
                { ($1, $2, $3, $4, [], [], $5, $6, $7, List.rev $8, List.rev $9) }      
        | /* rooms, !npcs, !items */
        rdef rdecl_list adecl_list start vdecl_list predicate_list fdecl_list EOF
                { ($1, $2, $3, $4, [], [], [], [], $5, List.rev $6, List.rev $7) }      
        | /* !rooms, npcs, items */
        ndef ndecl_list idef idecl_list vdecl_list predicate_list fdecl_list EOF         
                { ([], [], [], "null", $1, $2, $3, $4, $5, List.rev $6, List.rev $7) }
        | /* !rooms, npcs, !items */
        ndef ndecl_list  vdecl_list predicate_list fdecl_list EOF         
                { ([], [], [], "null", $1, $2, [], [], $3, List.rev $4, List.rev $5) }
        | /* !rooms, !npcs, items */
        idef idecl_list vdecl_list predicate_list fdecl_list EOF         
                { ([], [], [], "null", [], [], $1, $2, $3, List.rev $4, List.rev $5) }
        | /* !rooms, !npcs, !items */
        vdecl_list predicate_list fdecl_list EOF
                { ([], [], [], "null", [], [], [], [], $1, List.rev $2, List.rev $3) }      

data_type:
        INT                                     { Int }
        | STRING                                { String }
        | VOID                                  { Void }
        | BOOLEAN                               { Boolean }

pred_stmt:
        expr LBRACE vdecl_list stmt_list RBRACE
        { {
                pred = $1;
                locals = List.rev $3;
                body = List.rev $4;
        } }

predicate_list:
        /* nothing */                           { [] }
        | predicate_list pred_stmt              { $2 :: $1 }

        
fdecl_list:
        /* nothing */                           { [] }
        | fdecl_list fdecl                      { $2 :: $1 }

fdecl:
        FUNC data_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
        { { 
                freturntype = $2;    
                fname = $3;
                formals = $5;
                locals = List.rev $8;
                body = List.rev $9      
        } }

formals_opt:
        /* nothing */                           { [] }
        | formal_list                           { List.rev $1 }

formal_list:
        data_type ID                            { [Var($1, $2)] }
        | formal_list COMMA data_type ID        { Var($3, $4) :: $1 }

actuals_opt:
        /* nothing */                           { [] }
        | actuals_list                          { List.rev $1 }

actuals_list:
        expr                                    { [$1] }
        | actuals_list COMMA expr               { $3 :: $1 }

vdecl_list:
        /* nothing */                           { [] }
        | vdecl_list vdecl                      { $2 :: $1 }

vdecl:
        data_type LBRACK expr RBRACK ID SEMI    { Array_decl($1, $3, $5) }
        | data_type ID SEMI                     { Var($1, $2) }
        | data_type ID ASSIGN expr SEMI         { VarInit($1, $2, $4) }

rdef:
        ROOM LBRACE vdecl_list RBRACE           { $3 }

rdecl_list:
        rdecl rdecl                             { [$1; $2] }
        | rdecl_list rdecl                      { $2 :: $1 }

rdecl:
        ROOM ID LBRACE stmt_list RBRACE
        { {     
                rname = $2;
                rbody = List.rev $4      
        } }

start:
        START LBRACE ID RBRACE                  { $3 }        

adecl_list:
        adecl                                   { [$1] }
        | adecl_list adecl                      { $2 :: $1 }

adecl:
        adj_list SEMI                           { List.rev $1 }

adj_list:
        ID ADJ ID                               { [$1; $3] }

ndef:
        NPC LBRACE vdecl_list RBRACE            { $3 }

ndecl_list:
        /* nothing */                           { [] }
        | ndecl_list ndecl                      { $2 :: $1 }

ndecl:
        NPC ID LBRACE stmt_list RBRACE
        { {      
                nname = $2;
                nbody = List.rev $4       
        } }

idef:
        ITEM LBRACE vdecl_list RBRACE           { $3 }

idecl_list:
        /* nothing */                           { [] }
        | idecl_list idecl                      { $2 :: $1 }

idecl:
        ITEM ID LBRACE stmt_list RBRACE
        { {      
                iname = $2;
                ibody = List.rev $4       
        } }

stmt_list:
        /* nothing */                           { [] }
        | stmt_list stmt                        { $2 :: $1 }

stmt:
        expr SEMI                               { Expr($1) }
        | RETURN expr SEMI                      { Return($2) }
        | LBRACE stmt_list RBRACE               { Block(List.rev $2) }
        | IF LPAREN expr RPAREN stmt ELSE stmt  { If($3, $5, $7) }
        | WHILE LPAREN expr RPAREN stmt         { While($3, $5) }
        | GOTO ID                               { Goto($2) }
/*
int_opt:
        INT_LITERAL                             { $1 }
*/

expr:
        INT_LITERAL                             { IntLiteral($1) }
        | NEG INT_LITERAL                       { NegIntLiteral($2) }
        | STRING_LITERAL                        { StrLiteral($1) }
        | END                                   { End }
        | BOOL_LITERAL                          { BoolLiteral($1) }
        | ID                                    { Id($1) }
        | expr PLUS expr                        { Binop($1, Add, $3) }
        | expr MINUS expr                       { Binop($1, Sub, $3) }
        | expr TIMES expr                       { Binop($1, Mult, $3) }
        | expr DIVIDE expr                      { Binop($1, Div, $3) }
        | expr EQ expr                          { Binop($1, Equal, $3) }
        | expr STREQ expr                       { Binop($1, StrEqual, $3)}
        | expr NEQ expr                         { Binop($1, Neq, $3) }
        | expr LT expr                          { Binop($1, Less, $3) }
        | expr LEQ expr                         { Binop($1, Leq, $3) }
        | expr GT expr                          { Binop($1, Greater, $3) }
        | expr GEQ expr                         { Binop($1, Geq, $3) }
        | expr AND expr                         { Binop($1, And, $3)}
        | expr OR expr                          { Binop($1, Or, $3)}
        | NOT expr                              { Boolneg(Not, $2)}
        | ID ASSIGN expr                        { Assign($1, $3) }
        | ID LBRACK expr RBRACK ASSIGN expr     { ArrayAssign($1, $3, $6) }
        | ID LBRACK expr RBRACK                 { ArrayAccess($1, $3) }
        | ID LPAREN actuals_opt RPAREN          { Call ($1, $3) }
        | LPAREN expr RPAREN                    { $2 }
        | ID ACCESS ID                          { Access ($1, $3) }

