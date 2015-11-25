open Cast
open Ast

let rec get_main_method = function
        []              ->      [] (* TODO ERROR STATE *)
        | hd::tl        ->      if hd.fname = "main" 
    				        then [hd]
    				        else ( get_main_method tl )

let build_main (functions, rooms, adjacencies, npcs, items) = 
	let main_function = List.hd (get_main_method functions) in
        { mmethod = main_function; rdecls = rooms; adecls = adjacencies; ndecls = npcs; idecls = items;}

let rec build_others = function
        []              ->      []
        | hd::tl        ->      if hd.fname = "main" 
    				        then tl
    				        else ( hd::build_others tl )

let build_driver (functions, rooms, adjacencies, npcs, items) =
        let main = build_main (functions, rooms, adjacencies, npcs, items) in
        let other_functions = build_others functions in
	(main, other_functions)

let rearrange (program) = 
	let (room_def, room_decl_list, adj_decl_list, npc_def, npc_decl_list,
                item_def, item_decl_list, func_decl_list) = program in
	let driver = build_driver (func_decl_list, room_decl_list, adj_decl_list,
                npc_decl_list, item_decl_list) in
	(driver, room_def, npc_def, item_def)


