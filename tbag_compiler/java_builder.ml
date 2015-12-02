open Jast
open Ast

let build_main (preds, rooms, adjacencies, npcs, items) = 
    { predicates = preds; rdecls = rooms; adecls = adjacencies; ndecls = npcs; idecls = items;}

let build_driver (vars, preds, functions, rooms, adjacencies, npcs, items) =
    let main = build_main (preds, rooms, adjacencies, npcs, items) in
	   (vars, main, functions)

let rearrange (program) = 
	let (room_def, room_decl_list, adj_decl_list, npc_def, npc_decl_list,
                item_def, item_decl_list, vdecl_list, func_decl_list, predicate_list) = program in
	let driver = build_driver (vdecl_list, predicate_list, func_decl_list, room_decl_list, adj_decl_list,
                npc_decl_list, item_decl_list) in
	(driver, room_def, npc_def, item_def)


