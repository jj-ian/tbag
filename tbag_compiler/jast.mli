open Ast
open Sast
	
type main_method = 
{
    predicates: pred_stmt list;
    rdecls: room_decl list;
	adecls: adj_decl list;
	start: start;
	ndecls: npc_decl list;
	idecls: item_decl list;
}

type other_classes = sast_room_def * sast_item_def * sast_npc_def

type driver_class = checked_var_decl list * main_method * checked_func_decl list

type program = driver_class * other_classes