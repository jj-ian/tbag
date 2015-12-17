open Jast
open Ast
open Sast
open Printf

(* http://langref.org/fantom+ocaml+erlang/files/reading/read-into-string *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let build_main (preds, rooms, adjacencies, start, npcs, items) = 
    { predicates = preds; rdecls = rooms; adecls = adjacencies; start = start; ndecls = npcs; idecls = items;}

let build_driver (vars, preds, functions, rooms, adjacencies, start, npcs, items) =
    let main = build_main (preds, rooms, adjacencies, start, npcs, items) in
    let default_funcs = load_file("java_lib/driver_functions.txt") in
		(vars, main, functions, default_funcs)

let rearrange (program) = 
	let (room_def, room_decl_list, adj_decl_list, start, npc_def, npc_decl_list,
                item_def, item_decl_list, vdecl_list, func_decl_list, predicate_list) = program in
	let driver = build_driver (vdecl_list, predicate_list, func_decl_list, room_decl_list, adj_decl_list, start,
                npc_decl_list, item_decl_list) in
	(driver, room_def, npc_def, item_def)
