open Ast
(* TODO: pred_stmt checking, adj_decl checking, room/item/npc_decl checking,
 * room/item/npc_def checking, start checking, Access operator of expr*)
(*type room_table = var_decl list;*)

type symbol_table = {
    parent : symbol_table option;
    mutable variables : var_decl list;
    mutable functions : func_decl list;
    mutable room_def: var_decl list;
} 

type translation_environment = {
    scope : symbol_table;
    mutable return_type: variable_type;
    (*room : room_table;*)
}

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

let find_function (scope : symbol_table) name = 
    try
        List.find( fun func_decl -> func_decl.fname = name ) scope.functions
    with Not_found ->
        raise Not_found

let get_var_type_name var_decl = 
    begin match var_decl with 
    Array_decl(t, _, s) -> (t, s)
    | Var(t, s) -> (t, s)
    | VarInit(t, s, _) -> (t, s)
    end 

let rec check_expr env = function
        Ast.IntLiteral(v) -> (Ast.IntLiteral(v), Ast.Int)
        | Ast.NegIntLiteral(v) -> (Ast.NegIntLiteral(v), Ast.Int)
        | Ast.StrLiteral(v) -> (Ast.StrLiteral(v), Ast.String)
        | Ast.BoolLiteral(v) -> (Ast.BoolLiteral(v), Ast.Boolean)
        | Ast.Id(vname) ->
                let vdecl = (try
                find_variable env.scope vname 
                with Not_found ->
                    raise (Failure ("undeclared identifier " ^ vname))) in
                let (typ, vname) = get_var_type_name vdecl
                in (Ast.Id(vname), typ)
        | Ast.Binop(e1, op, e2) ->
                let (e1, t1) = check_expr env e1
                and (e2, t2) = check_expr env e2 in
                let typ =
                begin match op with
                    Add -> 
                        if (t1 = Int && t2 = Int) then Int
                        else raise (Failure "Types to + must both be Int")
                  | Sub ->
                        if (t1 = Int && t2 = Int) then Int
                        else raise (Failure "Types to - must both be Int")
                  | Mult ->
                        if (t1 = Int && t2 = Int) then Int
                        else raise (Failure "Types to * must both be Int")
                  | Div ->
                        if (t1 = Int && t2 = Int) then Int
                        else raise (Failure "Types to / must both be Int")
                  | Equal ->
                        if (t1 = t2) then Boolean 
                        else raise (Failure "Types to == must be the same")
                  | StrEqual ->
                        if (t1 = String && t2 = String) then Boolean
                        else raise (Failure "Types to ~~ must both be String")
                  | Neq ->
                        if (t1 = t2) then Boolean 
                        else raise (Failure "Types to != must be the same")
                  | Less ->
                        if (t1 = Int && t2 = Int) then Boolean 
                        else raise (Failure "Types to < must both be Int")
                  | Leq ->
                        if (t1 = Int && t2 = Int) then Boolean 
                        else raise (Failure "Types to <= must both be Int")
                  | Greater ->
                        if (t1 = Int && t2 = Int) then Boolean 
                        else raise (Failure "Types to > must both be Int")
                  | Geq ->
                        if (t1 = Int && t2 = Int) then Boolean 
                        else raise (Failure "Types to >= must both be Int")
                  | And ->
                        if (t1 = Boolean && t2 = Boolean) then Boolean
                        else raise (Failure "Types to AND must both be Boolean")
                  | Or ->
                        if (t1 = Boolean && t2 = Boolean) then Boolean
                        else raise (Failure "Types to OR must both be Boolean")
                  | Not -> raise (Failure "NOT takes a single operand") 

                end in (Ast.Binop(e1, op, e2), typ)
        | Ast.Assign(name, expr) ->
                let vdecl = find_variable env.scope name in
                let (typ, name) = get_var_type_name vdecl in
                let (expr, typ) = check_expr env expr in
                (Ast.Assign(name, expr), typ)
        | Ast.ArrayAssign(name, expr1, expr2) ->
                let vdecl = find_variable env.scope name in
                let (typ, name) = get_var_type_name vdecl in
                let (pos, postyp) = check_expr env expr1 in
                let (expr, exprtyp) = check_expr env expr2 in
                if postyp = Int then
                    if typ = exprtyp then (Ast.ArrayAssign(name, pos, expr), typ)
                    else raise (Failure "Right hand side of assignment statement does
                    not match type of array")
                else raise (Failure "Positional array access specifier must be an
                    Integer")
        | Ast.ArrayAccess(name, expr) ->
                let vdecl = find_variable env.scope name in
                let (typ, name) = get_var_type_name vdecl in 
                let (pos, postyp) = check_expr env expr in
                if postyp = Int then (Ast.ArrayAccess(name, expr), typ)
                else raise (Failure "Positional array access specifier must be an
                    Integer")
        | Ast.Boolneg(op, expr) ->
                let (expr, typ) = check_expr env expr in
                let op = begin match op with
                             Not -> op
                             | _ -> raise (Failure "All other operators besides
                             NOT take two operators")
                         end in
                (Ast.Boolneg(op, expr), typ)
       | Ast.Call(fname, expr_list) ->
               (* TODO: put find_function in try/with block *)
                let fdecl = find_function env.scope fname in
                let (typ, fname) = (fdecl.freturntype, fdecl.fname) in
                let formals = fdecl.formals in
                let (_, expr_list) = List.fold_left2 (
                    fun a l1 l2 -> 
                        let (formaltyp, formalname) = get_var_type_name l1 in 
                        let (argexpr, argtyp) = check_expr env l2 in
                        if formaltyp = argtyp then argexpr :: a
                        else raise (Failure "Type mismatch between formal argument and parameter")
                ) [] formals, expr_list in
                (Ast.Call(fname, expr_list), typ)
       | Ast.End -> (Ast.End, Ast.Int) (* This type is BS; will remove later *)
      (* TODO: Access operator for rooms, need to check that the thing is in the
       * room_decl, which will be stored in the environment *)

let rec check_stmt env = function
        Block(stmt_list) -> 
            (* We don't need this scope stuff because vars are not
             * declared in this stmt type but keeping here for now *)
            let scope' = { parent = Some(env.scope); variables = []; functions = env.scope.functions } in
            let env' = { env with scope = scope';} in
            let sl = List.map (fun s -> check_stmt env' s) stmt_list in scope'.variables <- List.rev scope'.variables; Block(sl)
        | Expr(expr) -> let (expr, _) = check_expr env expr in Expr(expr)
        | Return(expr) -> let (expr, typ) = check_expr env expr in 
          if typ = env.return_type then Return(expr)
          else raise (Failure "Return type of expression does not match return
type of function")
        | If(expr, stmt1, stmt2) ->
            let (expr, typ) = check_expr env expr in
            if typ = Boolean then If(expr, check_stmt env stmt1, check_stmt env stmt2) 
            else raise (Failure "If statement must have a boolean expression
            conditional")
        | While(expr, stmt) -> 
            let (expr, typ) = check_expr env expr in
            if typ = Boolean then While(expr, check_stmt env stmt) 
            else raise (Failure "While statement must have a boolean expression
            conditional")
        (*| Goto(rname)*)

(* make sure return statement of function returns proper type, check the
 * statements inside the function, add the declared variables to the scope, have
 * a new scope for the function *)
let check_func_decl (env: translation_environment) func_decl = 
    if (List.exists(fun fdecl -> fdecl.fname = func_decl.fname) env.scope.functions)
    then raise (Failure ("Function with " ^ func_decl.fname ^ " already
    exists."))
    else
        let scope' = { parent = Some(env.scope); variables = []; functions = env.scope.functions; room_def = env.scope.room_def} in
        let env' = { env with scope = scope'; return_type = func_decl.freturntype } in
        let fbody = func_decl.body in 
        let fbody = List.map (fun s -> check_stmt env' s) fbody in
        let new_func_decl = { func_decl with  body = fbody} in
        env.scope.functions <- new_func_decl::env.scope.functions ; new_func_decl 

let check_func_decls env func_decls =
    let func_decls = List.map (fun f -> check_func_decl env f) func_decls in 
    func_decls

let check_valid_type (v : Ast.variable_type) = match v with
    Ast.Void -> Ast.Void
    | Ast.Int -> Ast.Int
    | Ast.String -> Ast.String
    | Ast.Boolean -> Ast.Boolean
    | Ast.Array(v, i) -> Ast.Array(v, i)


let process_room_field (field: Ast.var_decl) (scope: symbol_table ) = match field with
    Ast.Var(typ, name) -> 
        let t = check_valid_type typ in
            if (List.exists ( fun var_decl -> begin match var_decl with 
                                            Var(_, s) -> s = name 
                                            |_ -> raise (Failure "should never reach here")
                                            end ) scope.room_def )
            then
                raise (Failure "room fields names cannot repeat.")
            else
                scope.room_def <- Ast.Var(t, name):: scope.room_def; (* side affect add room field to room_table *)
            Ast.Var(t, name) (*return this*)     
    | _ -> raise (Failure "room field not correct format. declare a type and name.")


(* fields in room_def are valid variable types *)
let check_room_def (env: translation_environment) (r: Ast.room_def) = 
    try
        let checked_fields = List.fold_left ( fun checked unchecked -> process_room_field unchecked env.scope :: checked ) [] r in
        checked_fields
    with
    | _ -> raise (Failure "room defs didn't check out")


let check_program (p : Ast.program) =
        (* at the start symbol table is empty *)
       let symbol_table = { parent = None; variables = []; functions = []; room_def = []; } in
       let translation_environment = { scope = symbol_table; } in
        let (room_def, room_decls, adj_decls, start, npc_defs, npc_decls, item_defs,
             item_decls, var_decls, funcs, pred_stmt) = p in
       let checked_room_def = check_room_def translation_environment room_def in
    (checked_room_def, room_decls, adj_decls, start, npc_defs, npc_decls, item_defs,
    item_decls, var_decls, funcs, pred_stmt)
(* make sure return statement of function returns proper type, check the
 * statements inside the function, add the declared variables to the scope, have
 * a new scope for the function *)
let check_func_decl env func_decl = 
    let scope' = { parent = Some(env.scope); variables = []; functions = env.scope.functions } in
    let env' = { env with scope = scope'; return_type = func_decl.freturntype } in
    (* add return type to env *)
    let fbody = func_decl.body in 
    let fbody = List.map (fun s -> check_stmt env' s) fbody in
    let new_func_decl = { func_decl with  body = fbody} in
    func_decl = new_func_decl

let check_func_decls env func_decls =
    let func_decls = List.map (fun f -> check_func_decl env f) func_decls in 
    func_decls

