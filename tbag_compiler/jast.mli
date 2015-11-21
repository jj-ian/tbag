open Sast

type main_method = 
{
		rdecls: room_decl list;
		adecls: adj_decl list;
		ndecls: npc_decl list;
		idecls: item_decl list;
		mbody: stmt list;
}

type driver_class = main_method *
					func_decl list


type program = driver_class *
			   room_def *
			   npc_def *
			   item_def