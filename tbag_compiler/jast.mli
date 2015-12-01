open Ast

type variable_type =
        Int
        | Char
        | Void
        | Array of variable_type * int

type argument_decl =
        Argument of variable_type * string

type main_method = 
{
    mmethod: func_decl;
    rdecls: room_decl list;
	adecls: adj_decl list;
	ndecls: npc_decl list;
	idecls: item_decl list;
}

type func_statement = 
{
        freturntype: variable_type;
        fname : string;
        formals : argument_decl list;
}

type func_decls = func_statement list

type func_defs = func_decl list

type struct_defs = room_def * item_def * npc_def

type program = struct_defs * func_decls * func_defs * main_method