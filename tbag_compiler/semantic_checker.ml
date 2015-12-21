open Ast
(* TODO: pred_stmt checking, adj_decl checking, room/item/npc_decl checking,
 * room/item/npc_def checking, start checking, Access operator of expr*)
(*type room_table = var_decl list;*)

(* environment *)
type symbol_table = {
    parent : symbol_table option;
    mutable variables : var_decl list;
} 

type translation_environment = {
    scope : symbol_table;
    mutable return_type: variable_type;
    mutable current_func: func_decl option;
    mutable functions : func_decl list;
    mutable room_def: var_decl list;
    mutable rooms: room_decl list;
    mutable npc_def: var_decl list;
    mutable npcs: npc_decl list;
    mutable item_def: var_decl list;
    mutable items: item_decl list;
    mutable pred_stmts : pred_stmt list;
}

(* helper functions *)
let check_valid_var_type (v : Ast.variable_type) = match v with
      Ast.Int -> Ast.Int
    | Ast.String -> Ast.String
    | Ast.Boolean -> Ast.Boolean
    | Ast.Array(v, i) -> Ast.Array(v, i)
    | _ -> raise (Failure ("Invalid variable type used"))
    (* vars can't be declared as "void" *)

let get_var_type_name var_decl = 
    begin match var_decl with 
    Array_decl(t, _, s) -> (t, s)
    | Var(t, s) -> (t, s)
    | VarInit(t, s, _) -> (t, s)
    end 

let var_is_array var_decl = 
    begin match var_decl with 
    Array_decl(_, _, _) -> true 
    | Var(_, _) -> false 
    | VarInit(_, _, _) -> false 
    end 

let expr_is_strlit expr = 
    begin match expr with 
     StrLiteral(_) -> true
   | _ -> false
    end 

(* type enforcement functions *)

let require_integers tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            Int -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let require_strings tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            String -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let require_voids tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            Void -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let require_bools tlist str = 
    let _ = List.map(
        fun t ->  match t with 
            Boolean -> true
          | _ -> raise (Failure(str))
    ) tlist in
    true

let require_eq tlist str = 
    let typ = List.hd tlist in
    let _ = List.map(
        fun t ->  if typ <> t then raise (Failure(str)) 
    ) tlist in
    true


(* find functions *)
let rec find_variable (scope : symbol_table) name = 
    try 
        (* do match with the different types of variables in the List.find
         * function *)
        List.find ( fun var_decl ->
            begin match var_decl with 
            Array_decl(_, _, s) -> s = name
            | Var(_, s) -> s = name 
            | VarInit(_, s, _) -> s = name
            end ) scope.variables
    with Not_found ->
        match scope.parent with 
          Some(parent) -> find_variable parent name
          | _ -> raise Not_found

let get_var_if_exists (scope : symbol_table) name = 
    let var_decl = (try find_variable scope name with
    Not_found -> raise (Failure ("undeclared identifier " ^
                    name))) in var_decl

let find_room (env: translation_environment) (name) = 
    try 
        List.find (fun room_decl -> room_decl.rname = name) env.rooms
    with Not_found-> raise Not_found 

let find_room_field (env: translation_environment) fieldName =
     let field_decl = (try (List.find ( fun var_decl -> begin match var_decl with 
                                            Var(t, s) -> s = fieldName 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.room_def ) 
                        with
                            Not_found -> raise(Failure "room field referenced does not exist."))
     in let (typ, n) = get_var_type_name field_decl in 
     (typ, n)

let find_npc (env: translation_environment) (name) = 
    try 
        List.find (fun npc_decl -> npc_decl.nname = name) env.npcs
    with Not_found-> raise Not_found 

let find_npc_field (env: translation_environment) fieldName =
     let field_decl = (try (List.find ( fun var_decl -> begin match var_decl with 
                                            Var(t, s) -> s = fieldName 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.npc_def ) 
                        with
                            Not_found -> raise(Failure "npc field referenced does not exist."))
     in let (typ, n) = get_var_type_name field_decl in 
     (typ, n)

let find_item (env: translation_environment) (name) = 
    try 
        List.find (fun item_decl -> item_decl.iname = name) env.items
    with Not_found-> raise Not_found 

let find_item_field (env: translation_environment) fieldName =
     let field_decl = (try (List.find ( fun var_decl -> begin match var_decl with 
                                            Var(t, s) -> s = fieldName 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.item_def ) 
                        with
                            Not_found -> raise(Failure "item field referenced does not exist."))
     in let (typ, n) = get_var_type_name field_decl in 
     (typ, n)

(* check that op matches with types of t1, t2 and return type of result *)
let get_binop_type op t1 t2 = 
    begin match op with
        (Add | Sub | Mult | Div)  -> 
            let _ = require_integers [t1;t2] "Types to arithmetic operators +, -, *, /
            must both be Int" in
            Int
      | (Equal | Neq) ->
              let _ = 
                  (try require_integers[t1;t2]  ""
                   with _ -> try require_bools[t1;t2] ""
                       with _ -> require_voids[t1;t2] "Types to equality operators ==, !=
                        must be the same and be integers, booleans, or
                        rooms" ) in
              Boolean

     | (Less | Leq | Greater | Geq) ->
             let _ = require_integers [t1;t2] "Types to integer comparison
                        operators <, <=, >, >= must be integers" in
             Boolean
     | StrEqual ->
             let _ = require_strings [t1;t2] "Types to ~~ must both be String"
             in Boolean
     | (And | Or) ->
             let _ = require_bools [t1;t2] "Types to binary boolean operators AND, OR must both be Boolean" in
             Boolean
     | _ -> raise (Failure "Should not reach here") 
    end

(* Expr checking *)
let rec check_expr env = function
        Ast.IntLiteral(v) -> (Ast.IntLiteral(v), Ast.Int)
        | Ast.NegIntLiteral(v) -> (Ast.NegIntLiteral(v), Ast.Int)
        | Ast.StrLiteral(v) -> (Ast.StrLiteral(v), Ast.String)
        | Ast.BoolLiteral(v) -> (Ast.BoolLiteral(v), Ast.Boolean)
        | Ast.Id(vname) -> 
                let vdecl = (try
                find_variable env.scope vname 
                with Not_found -> 
                    let _ = (try find_room env vname with
                    Not_found -> raise (Failure ("undeclared identifier " ^
                    vname))) in
                    Var(Ast.Void, vname)) in
                let (typ, vname) = get_var_type_name vdecl 
                in (Ast.Id(vname), typ)
        | Ast.Binop(e1, op, e2) ->
                let (e1, t1) = check_expr env e1
                and (e2, t2) = check_expr env e2 in
                (Ast.Binop(e1, op, e2), get_binop_type op t1 t2)
        | Ast.Assign(name, expr) ->
                let vdecl = get_var_if_exists env.scope name in
                let (vtyp, name) = get_var_type_name vdecl in
                let (expr, etyp) = check_expr env expr in
                if not (var_is_array vdecl) then 
                    let _ = require_eq [vtyp;etyp] "Type mismatch in assignment statement" in (Ast.Assign(name, expr), vtyp)
                else raise (Failure "Left hand side of assignment statement must
                be a non-array variable")
        | Ast.ArrayAssign(name, expr1, expr2) ->
                let vdecl = get_var_if_exists env.scope name in
                let (typ, name) = get_var_type_name vdecl in
                let (pos, postyp) = check_expr env expr1 in
                let (expr, exprtyp) = check_expr env expr2 in
                let _ = require_integers [postyp] "Positional array access specifier must be an
                    Integer" in
                    if var_is_array vdecl then
                        let _ = require_eq [typ;exprtyp] "Right hand side of assignment statement does
                    not match type of array" in
                        (Ast.ArrayAssign(name, pos, expr), typ)
                    else raise (Failure "Left hand side of array assignment must
                    be an array")
        | Ast.ArrayAccess(name, expr) ->
                let vdecl = get_var_if_exists env.scope name in
                let (typ, name) = get_var_type_name vdecl in 
                let (pos, postyp) = check_expr env expr in
                let _ = require_integers [postyp] "Positional array access specifier must be an
                    Integer" in
                if var_is_array vdecl then (Ast.ArrayAccess(name, expr), typ)
                else raise (Failure "Array access must be used on an array
type")
        | Ast.Boolneg(op, expr) ->
                let (expr, typ) = check_expr env expr in
                let _ = require_bools [typ] "Type to unary boolean NOT operator must be
                    boolean" in
                (Ast.Boolneg(op, expr), typ)
       | Ast.Call(fname, expr_list) ->
               (* TODO: make sure recursive calls to function also match:
                * expr_list *)
                if fname = "arrLen" && List.length expr_list = 1 then 
                    let arr_name = 
                      let e = List.hd expr_list in
                      begin match e with 
                        Ast.Id(vname) -> vname
                      | _ -> raise (Failure("arrLen expects an array
                      argument"))
                        end in
                     let arr_decl = get_var_if_exists env.scope arr_name in
                     if var_is_array arr_decl then
                    (Ast.Call(fname, expr_list), Ast.Int)
                     else raise (Failure "arrLen expects an array
                      argument")
                 else if fname = "getInputFromOptions" then 
                     let _ = List.map(
                         fun e -> if not (expr_is_strlit e) then raise (Failure("getInputFromOptions expects 
                      one or more string arguments"))) expr_list in
                    (Ast.Call(fname, expr_list), Ast.Void)
                 else if fname = "getInputAdjacentRooms" && List.length expr_list = 1 then
                     let rname = 
                      begin match List.hd expr_list with 
                        Ast.Id(r) -> r 
                      | _ -> raise (Failure("getInputAdjacentRooms expects a room argument"))
                        end in
                       let _ = (try find_room env rname with 
                            Not_found -> raise (Failure ("undeclared identifier " ^ rname))) in
                        (Ast.Call(fname, expr_list), Ast.Void)
                 else
                     let fdecl = (try find_function_with_exprs env fname expr_list
                             with Not_found -> begin match env.current_func with 
                             Some(current_func) -> 
                                if (current_func.fname = fname &&
                                check_matching_args env current_func.formals
                                expr_list) then current_func 
                                else 
                                    raise (Failure ("Function " ^ fname ^ " does
                                    not exist with the given parameters."))
                            |_-> raise (Failure ("Function " ^ fname ^ " does
                            not exist with the given parameters."))end) in
                let typ = fdecl.freturntype in
                    (Ast.Call(fname, expr_list), typ)
       | Ast.End -> (Ast.End, Ast.Int) (* This type is BS; will remove later *)
      (* TODO: Access operator for rooms, need to check that the thing is in the
       * room_decl, which will be stored in the environment *)
       | Ast.Access(name, field) -> 
            (*print_string "trying to access name field";*)
            try let _ =  find_room env name in 
                let (ftyp, fname) = find_room_field env field in 
                    (Ast.Access(name, field), ftyp)
            with Not_found -> 
                try let _ = find_npc env name in 
                    (*print_string "didn't find the room name in access... trying to find npc";*)
                    let (ftyp, fname) = find_npc_field env field in 
                        (Ast.Access(name, field), ftyp)
                with Not_found -> 
                    try let _ = find_item env name in 
                    (*print_string "didn't find the npc name in access... trying to find item";*)
                        let (ftyp, fname) = find_item_field env field in 
                            (Ast.Access(name, field), ftyp)
                    with Not_found -> 
                        raise(Failure("Trying to access field " ^ field ^ ", which does not exist for that structure."))

            (*let ndecl = (try find_npc env name with Not_found -> 
                raise(Failure("Trying to access NPC " ^ name ^ " which does not exist."))) in 
            let (ftyp, fname) = find_npc_field env field in 
            (Ast.Access(name, field), ftyp)*)

(* check formal arg list with expr list of called function *)
and check_matching_args_helper (env: translation_environment) ref_vars target_exprs =
    let result = true in
    let _ = (try List.map2 (
        fun r t -> let (rtyp, rname) = get_var_type_name r in 
                   let (texpr, ttyp) = check_expr env t in 
                   if ttyp <> rtyp then raise Not_found) ref_vars target_exprs
    with Invalid_argument(_) -> raise Not_found) in result

and check_matching_args (env: translation_environment) ref_vars target_exprs =
    let result = (try check_matching_args_helper env ref_vars target_exprs with
    Not_found -> false) in result

and find_function_with_exprs (env : translation_environment) name expr_list = 
    try
        List.find( fun func_decl -> func_decl.fname = name &&
        check_matching_args env func_decl.formals expr_list) env.functions
    with Not_found -> raise Not_found

let check_matching_decls_helper (env: translation_environment) ref_vars target_decls =
    let result = true in
    let _ = (try List.map2 (
        fun r t -> let (rtyp, rname) = get_var_type_name r in 
                   let (ttyp, tname) = get_var_type_name t in 
                   if ttyp <> rtyp then raise Not_found) ref_vars target_decls
    with Invalid_argument(_) -> raise Not_found) in result

let check_matching_decls (env: translation_environment) ref_vars target_decls =
    let result = (try check_matching_decls_helper env ref_vars target_decls with
    Not_found -> false) in result

let find_function_with_decls (env : translation_environment) name decl_list = 
    try
        List.find( fun func_decl -> func_decl.fname = name &&
        check_matching_decls env func_decl.formals decl_list) env.functions
    with Not_found -> raise Not_found

(* Stmt checking*)
let rec check_stmt env = function
        Block(stmt_list) -> 
            let sl = List.map (fun s -> check_stmt env s) stmt_list in Block(sl)
        | Expr(expr) -> let (expr, _) = check_expr env expr in Expr(expr)
        | Return(expr) -> let (expr, typ) = check_expr env expr in 
          let _ = require_eq [typ;env.return_type] "Return type of expression does not match return
type of function" in
          Return(expr)
        | If(expr, stmt1, stmt2) ->
            let (expr, typ) = check_expr env expr in
            let _ = require_bools [typ] "If statement must have a boolean expression
            conditional" in
            If(expr, check_stmt env stmt1, check_stmt env stmt2) 
        | While(expr, stmt) -> 
            let (expr, typ) = check_expr env expr in
            let _ = require_bools [typ] "While statement must have a boolean expression
            conditional" in
            While(expr, check_stmt env stmt) 
        | Goto(rname) ->
            let rdecl = try find_room env rname with
                    Not_found -> raise( Failure "Goto parameter name not a valid room.") 
            in Goto(rdecl.rname)

(* Variable checking, both global and local *)
let check_var_decl (env: translation_environment) vdecl = 
        let (typ, vname) = get_var_type_name vdecl in
        try let _ = find_variable env.scope vname in raise(Failure ("Variable with name " ^ vname ^
        " exists."))
        with Not_found ->
            (* add this var to the variables list of this environment *)
            (* also check that the expr type matches up with the type of the var
             * *)
            (* check that type is valid*)
            match vdecl with 
            Array_decl(typ, expr, name) -> 
                    let (expr, exprtyp) = check_expr env expr in
                    let _ = require_integers [exprtyp] "Array size must be integer" in 
                    let typ = check_valid_var_type typ in
                    (env.scope.variables <- Array_decl (typ,expr,name)::env.scope.variables; Array_decl (typ,expr,name)) 
              | Var(typ, name) -> let typ = check_valid_var_type typ in 
                     env.scope.variables <- Var(typ, name)::env.scope.variables; Var(typ, name)
              | VarInit(typ, name, expr) -> let typ = check_valid_var_type typ in 
                    let (expr, exprtyp) = check_expr env expr in 
                    let _ = require_eq [exprtyp;typ] "Type mismatch in variable initialization" in 
                        (env.scope.variables <- VarInit (exprtyp,name,expr)::env.scope.variables; VarInit (exprtyp, name,expr)) 

let check_var_decls (env: translation_environment) var_decls = 
    let var_decls = List.map(fun vdecl -> check_var_decl env vdecl) var_decls in
    var_decls

(* Function checking*)
let check_func_decl (env: translation_environment) func_decl = 
    try let _ = find_function_with_decls env func_decl.fname func_decl.formals in 
    raise(Failure ("Function with name " ^ func_decl.fname ^ " and given
    argument types exists"))
    with Not_found -> 
        let scope' = { parent = Some(env.scope); variables = [];} in
        let env' = { env with scope = scope'; return_type =
            func_decl.freturntype; current_func = Some(func_decl)} in
        let fformals = List.map (
            fun f -> match f with 
                     Var(typ, name) ->
                         let typ = check_valid_var_type typ in 
                         env'.scope.variables <- Var(typ,
                         name)::env'.scope.variables; Var(typ, name)
                   | _ -> raise (Failure ("Formal argument must be of type Var"))
                   ) func_decl.formals in
        let flocals = check_var_decls env' func_decl.locals in 
        let fbody = func_decl.body in 
        let fbody = List.map (fun s -> check_stmt env' s) fbody in
        let ffreturntype = func_decl.freturntype in
        let new_func_decl = { func_decl with  body = fbody; locals = flocals;
        formals = fformals; freturntype = ffreturntype; } in
        env.functions <- new_func_decl::env.functions ; new_func_decl 

let check_func_decls env func_decls =
    let func_decls = List.map (fun f -> check_func_decl env f) func_decls in 
    func_decls

(* Room checking*)
let process_room_field (field: Ast.var_decl) (env: translation_environment) = match field with
    Ast.Var(typ, name) -> 
        let t = check_valid_var_type typ in
            if (List.exists ( fun var_decl -> begin match var_decl with 
                                            Var(_, s) -> s = name 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.room_def )
            then
                raise (Failure "room fields names cannot repeat.")
            else
                env.room_def <- Ast.Var(t, name):: env.room_def; (* side effect add room field to room_table *)
            Ast.Var(t, name) (*return this*)     
    | _ -> raise (Failure "room field not correct format. declare a type and name.") 


let process_room_decl_body (env: translation_environment) (rfa: Ast.stmt) = begin match rfa with
    Ast.Expr(roomAssign) -> begin match roomAssign with (* check that the expr is in the form of an assign*)
        Ast.Assign(fieldname, expr) ->
            let rdecl = 
            (try List.find(fun rdecl -> begin match rdecl with 
                    Array_decl(_, _, s) -> "0" = fieldname
                    | Var(t, s) -> s = fieldname 
                    | VarInit(_, s, _) -> "0" = fieldname end) env.room_def
            with 
                Not_found-> raise (Failure "field name in room decl does not
                exist.")) in 
            let (room_decl_typ, _) = get_var_type_name rdecl in
            (*CHECKING FOR ROOM DECL BODY EXPR RETURN TYPE HERE*)
            let (_, typ) = check_expr env expr in
            let _ = require_eq [typ;room_decl_typ] "room decl body does not match field type" in
            rfa (*return this*)
        | _ -> raise (Failure "room assignment not correct format.") end
    | _ -> raise (Failure "room assignment not correct format.") end


let check_room_decl (env: translation_environment) (room: Ast.room_decl) = 
    let name = room.rname in 
    let body = room.rbody in (* body is a list of stmts*)
        try let _ = find_room env name in raise(Failure ("Room with name " ^ name ^ " already exists."))
        with Not_found ->
        let checked_body = List.map ( fun unchecked -> process_room_decl_body env unchecked) body in
        (* check that number of fields in room_def match with number of fields in room decl*)
        let num_stmts = List.length(checked_body) in
            if (num_stmts <> List.length(env.room_def)) then
                raise (Failure "number of room decl fields do not match definition.")
            else 
                (* add the room_decls to the scope*)
                let checked_room_decl = { rname = name; rbody = checked_body } in 
                env.rooms <- checked_room_decl::env.rooms;
                checked_room_decl (* return this *)

let check_room_decls (env: translation_environment) rooms = 
        let checked_room_decls = List.map (fun unchecked -> check_room_decl env unchecked) rooms in
        checked_room_decls


let check_room_def (env: translation_environment) (r: Ast.room_def) = 
    try
        let checked_fields = List.map ( fun room_field -> process_room_field room_field env) r in
        checked_fields
    with
    | _ -> raise (Failure "room defs didn't check out")

(* NPC checking*)
let process_npc_field (field: Ast.var_decl) (env: translation_environment) = match field with
    Ast.Var(typ, name) -> 
        let t = check_valid_var_type typ in
            if (List.exists ( fun var_decl -> begin match var_decl with 
                                            Var(_, s) -> s = name 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.npc_def )
            then
                raise (Failure "npc fields names cannot repeat.")
            else
                env.npc_def <- Ast.Var(t, name):: env.npc_def; (* side effect add room field to room_table *)
            Ast.Var(t, name) (*return this*)     
    | _ -> raise (Failure "npc field not correct format. declare a type and name.") 


let process_npc_decl_body (env: translation_environment) (nfa: Ast.stmt) = begin match nfa with
    Ast.Expr(npcAssign) -> begin match npcAssign with (* check that the expr is in the form of an assign*)
        Ast.Assign(fieldname, expr) ->
            let ndecl = 
                (try List.find(fun ndecl -> begin match ndecl with 
                    Array_decl(_, _, s) -> "0" = fieldname
                    | Var(t, s) -> s = fieldname 
                    | VarInit(_, s, _) -> "0" = fieldname end) env.npc_def 
                 with 
                Not_found-> raise (Failure "field name in npc decl does not
                exist.")) in 
            let (npc_decl_typ, _) = get_var_type_name ndecl in
            let (_, typ) = check_expr env expr in
                let _ = require_eq [typ;npc_decl_typ] "npc decl body does not match field type" in 
                nfa (*return this*)
        | _ -> raise (Failure "npc assignment not correct format.") end
    | _ -> raise (Failure "npc assignment not correct format.") end


let check_npc_decl (env: translation_environment) (npc: Ast.npc_decl) = 
    let name = npc.nname in 
    let body = npc.nbody in (* body is a list of stmts*)
        try let _ = find_npc env name in raise(Failure ("NPC with name " ^ name ^ " already exists."))
        with Not_found ->
        let checked_body = List.map ( fun unchecked -> process_npc_decl_body env unchecked) body in
        (* check that number of fields in room_def match with number of fields in room decl*)
        let num_stmts = List.length(checked_body) in
            if (num_stmts <> List.length(env.npc_def)) then
                raise (Failure "number of npc decl fields do not match definition.")
            else 
                (* add the room_decls to the scope*)
                let checked_npc_decl = { nname = name; nbody = checked_body } in 
                env.npcs <- checked_npc_decl::env.npcs;
                checked_npc_decl (* return this *)

let check_npc_decls (env: translation_environment) npcs = 
        let checked_npc_decls = List.map (fun unchecked -> check_npc_decl env unchecked) npcs in
        checked_npc_decls


let check_npc_def (env: translation_environment) (n: Ast.npc_def) = 
    try
        let checked_fields = List.map ( fun npc_field -> process_npc_field npc_field env) n in
        checked_fields
    with
    | _ -> raise (Failure "npc defs didn't check out")


(* Item checking*)
let process_item_field (field: Ast.var_decl) (env: translation_environment) = match field with
    Ast.Var(typ, name) -> 
        let t = check_valid_var_type typ in
            if (List.exists ( fun var_decl -> begin match var_decl with 
                                            Var(_, s) -> s = name 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) env.item_def )
            then
                raise (Failure "item fields names cannot repeat.")
            else
                env.item_def <- Ast.Var(t, name):: env.item_def; (* side effect add item field to item_table *)
            Ast.Var(t, name) (*return this*)     
    | _ -> raise (Failure "item field not correct format. declare a type and name.") 

let process_item_decl_body (env: translation_environment) (ifa: Ast.stmt) = begin match ifa with
    Ast.Expr(itemAssign) -> begin match itemAssign with (* check that the expr is in the form of an assign*)
        Ast.Assign(fieldname, expr) ->
            let idecl = 
                (try List.find(fun idecl -> begin match idecl with 
                    Array_decl(_, _, s) -> "0" = fieldname
                    | Var(t, s) -> s = fieldname 
                    | VarInit(_, s, _) -> "0" = fieldname end) env.item_def
                    with Not_found -> raise (Failure "field name in npc decl does not
                exist.")) in
            let (item_decl_typ, _) = get_var_type_name idecl in
            let (_, typ) = check_expr env expr in
            let _ = require_eq[typ;item_decl_typ] "item decl body does not match field type" in 
            ifa (*return this*)
        | _ -> raise (Failure "item assignment not correct format.") end
    | _ -> raise (Failure "item assignment not correct format.") end


let check_item_decl (env: translation_environment) (item: Ast.item_decl) = 
    let name = item.iname in 
    let body = item.ibody in (* body is a list of stmts*)
        try let _ = find_item env name in raise(Failure ("Item with name " ^ name ^ " already exists."))
        with Not_found ->
        let checked_body = List.map ( fun unchecked -> process_item_decl_body env unchecked) body in
        (* check that number of fields in room_def match with number of fields in room decl*)
        let num_stmts = List.length(checked_body) in
            if (num_stmts <> List.length(env.item_def)) then
                raise (Failure "number of item decl fields do not match definition.")
            else 
                (* add the room_decls to the scope*)
                let checked_item_decl = { iname = name; ibody = checked_body } in 
                env.items <- checked_item_decl::env.items;
                checked_item_decl (* return this *)

let check_item_decls (env: translation_environment) items = 
        let checked_item_decls = List.map (fun unchecked -> check_item_decl env unchecked) items in
        checked_item_decls


let check_item_def (env: translation_environment) (i: Ast.item_def) = 
    try
        let checked_fields = List.map ( fun item_field -> process_item_field item_field env) i in
        checked_fields
    with
    | _ -> raise (Failure "item defs didn't check out")

(* Predicate checking *)
let check_pred_stmt (env: translation_environment) pstmt =
    (* check that the expr is a boolean expr, check all var decls, check all
     * stmts in body *)
    let scope' = { parent = Some(env.scope); variables = [];} in
    let env' = { env with scope = scope'; functions =
        env.functions; room_def = env.room_def; pred_stmts =
            env.pred_stmts; rooms = env.rooms } in
    let (checked_pred, typ) = check_expr env pstmt.pred in
    let _ = require_bools [typ] "Expression in predicate statement conditional must be
    of type Boolean" in 
    let checked_locals = check_var_decls env' pstmt.locals in
    let checked_body = List.map (fun s -> check_stmt env' s) pstmt.body in
    let new_pstmt = {pred = checked_pred; locals = checked_locals; body =
        checked_body;} in
    env.pred_stmts <- new_pstmt::env.pred_stmts ; new_pstmt

(* do we need to store the pred_stmts so that we can prevent multiple pred_stmts
 * with the same condition? Would we compare the exprs in that case? Decompose
 * expr into all possible boolean exprs, compare operands and operator*)
let check_pred_stmts (env: translation_environment) pstmts = 
    let new_pstmts = List.map (fun s -> check_pred_stmt env s) pstmts in
    new_pstmts

(* Adjacency checking *)
let find_adjacency (env : translation_environment) adj = 
    if (List.exists (fun rdecl -> rdecl.rname = (List.nth adj 0)) env.rooms && 
        List.exists (fun rdecl -> rdecl.rname = (List.nth adj 1)) env.rooms) then
        adj
    else raise (Failure "One of rooms in adjacency list not declared")


let check_adj_decls (env: translation_environment) adecls = 
    try
        let checked_adjs = List.map ( fun adecl -> find_adjacency env adecl) adecls in
        checked_adjs
    with
    | _ -> raise (Failure "adjacencies didn't check out")   

(* Entrance point that transforms Ast into semantically correct Ast *)
let check_program (p : Ast.program) =
       let print_int = { freturntype = Void; fname = "print"; formals =
           [Var(Ast.Int, "arg")]; locals = []; body = [];} in
       let print_bool = { freturntype = Void; fname = "print"; formals =
           [Var(Ast.Boolean, "arg")];locals = []; body = [];} in
       let print_str = { freturntype = Void; fname = "print"; formals =
           [Var(Ast.String, "arg")]; locals = []; body = [];} in
       let print_funcs = [print_int; print_bool; print_str] in
       (*let get_adj_func =  { freturntype = Void; fname = "getInputAdjacentRooms"; formals = [Ast.room_decl];
            locals = []; body = []} in*)
       (* adding name type String as default field in room_def*)
       let name_field = Ast.Var(String, "name") in 
       (* adding currentRoom as a global variable*)
       let current_room = { rname = "currentRoom" ; rbody = []} in
       let input = Var(Ast.String, "input") in
       let dummy_room = { rname = "input" ; rbody = [] } in 
       let dummy_npc = { nname = "input"; nbody = [] } in
       let dummy_item = { iname = "input"; ibody = [] } in
       let symbol_table = { parent = None; variables = [input];} in
       let env = { scope = symbol_table; return_type =
           Ast.Int; functions = print_funcs; room_def = [name_field]; rooms = [current_room; dummy_room]; npc_def = []; npcs = [dummy_npc];
           item_def = []; items = [dummy_item]; pred_stmts = []; current_func = None } in
        let (room_def, room_decls, adj_decls, start, npc_def, npc_decls, item_def,
             item_decls, var_decls, pred_stmts, funcs) = p in
       let checked_room_def = check_room_def env room_def in
       let checked_room_decls = check_room_decls env room_decls in
       let checked_npc_def = check_npc_def env npc_def in
       let checked_npc_decls = check_npc_decls env npc_decls in
       let checked_item_def = check_item_def env item_def in
       let checked_item_decls = check_item_decls env item_decls in
       let checked_adj_decls = check_adj_decls env adj_decls in
       let checked_var_decls = check_var_decls env var_decls in
       let checked_funcs = check_func_decls env funcs in
       let checked_pred_stmts = check_pred_stmts env pred_stmts in
    (checked_room_def, checked_room_decls, checked_adj_decls, start, checked_npc_def, checked_npc_decls, checked_item_def,
    checked_item_decls, checked_var_decls, checked_pred_stmts, checked_funcs)
