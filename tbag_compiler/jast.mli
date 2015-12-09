open Ast

type main_method = 
{
    predicates: pred_stmt list;
    rdecls: room_decl list;
	adecls: adj_decl list;
	start: start;
	ndecls: npc_decl list;
	idecls: item_decl list;
}

type other_classes = room_def * item_def * npc_def

type driver_class = var_decl list * main_method * func_decl list

type program = driver_class * other_classes