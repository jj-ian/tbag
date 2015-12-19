
(* extra functions for checking the call of print *)

let check_printable_type (v : Ast.variable_type) = match v with
    Ast.Void -> raise (Failure ("Cannot print void"))
    | Ast.Int -> ()
    | Ast.String -> ()
    | Ast.Boolean -> ()
    | Ast.Array(v, i) -> raise (Failure ("Cannot print array"))
    | _ -> raise (Failure ("Invalid type used"))

let check_var_init (scope: symbol_table) name =
    try 
        (* do match with the different types of variables in the List.find
         * function *)
        List.find ( fun var_decl ->
            begin match var_decl with 
            Array_decl(_, _, s) -> s = "0"
            | Var(_, s) -> s = "0"
            | VarInit(_, s, _) -> s = name
            end ) scope.variables
    with Not_found ->
        print_string "varaible not initialized";
        raise Not_found


(* adj decl stuff *)

let find_adjacency (scope : symbol_table) adj = 
    try
        List.find (fun rdecl -> rdecl.rname = (List.nth adj 1)) scope.room_decls;
        List.find (fun rdecl -> rdecl.rname = (List.nth adj 2)) scope.room_decls;
    with Not_found -> 
        print_string "One of rooms in adjacency list not declared"; raise Not_found

let check_adj_decls (env: translation_environment) adecls = 
    try
        let checked_adjs = List.map ( fun adecl -> find_adjacency env.scope adecl) adecls in
        checked_adjs
    with
    | _ -> raise (Failure "adjecencies didn't check out")   