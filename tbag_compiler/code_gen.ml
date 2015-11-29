open Printf
open Cast
open Ast

let file = "game.c"

let struct_code (p) = ""

let f_headers = function
        []              ->      ""
        | hd::tl        ->      ""

let f_defs = function
        []              ->      ""
        | hd::tl        ->      ""


let code_gen (room_def, item_def, npc_def, functions) = 
        let oc = open_out file in
        fprintf oc "%s" ((struct_code room_def) ^ (struct_code item_def) ^
        (struct_code npc_def) ^ (f_headers functions) ^ (f_defs functions));
        close_out oc;
     
