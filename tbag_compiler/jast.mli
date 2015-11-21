open Sast

type main_method = 
{
        mmethod: func_decl;
		rdecls: room_decl list;
		adecls: adj_decl list;
		ndecls: npc_decl list;
		idecls: item_decl list;
}

type driver_class = main_method *
					func_decl list

type program = driver_class *
			   room_def *
			   npc_def *
			   item_def

