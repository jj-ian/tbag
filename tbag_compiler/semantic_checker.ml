open Ast
(* TODO: pred_stmt checking, adj_decl checking, room/item/npc_decl checking,
 * room/item/npc_def checking, start checking, Access operator of expr*)
(*type room_table = var_decl list;*)

type symbol_table = {
    parent : symbol_table option;
    mutable variables : var_decl list;
    mutable functions : func_decl list;
    mutable room_def: var_decl list;
    mutable pred_stmts : pred_stmt list;
    mutable rooms: room_decl list;
} 

type translation_environment = {
    scope : symbol_table;
    mutable return_type: variable_type;
    mutable current_func: func_decl option;
    (*room : room_table;*)
}

let check_valid_type (v : Ast.variable_type) = match v with
    Ast.Void -> Ast.Void
    | Ast.Int -> Ast.Int
    | Ast.String -> Ast.String
    | Ast.Boolean -> Ast.Boolean
    | Ast.Array(v, i) -> Ast.Array(v, i)
    | _ -> raise (Failure ("Invalid type used"))

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

let find_room (scope: symbol_table) (name) = 
    try 
        List.find (fun room_decl -> room_decl.rname = name) scope.rooms
    with Not_found-> raise Not_found

let find_function (scope : symbol_table) name = 
    try
        List.find( fun func_decl -> func_decl.fname = name ) scope.functions
    with Not_found -> raise Not_found

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
                   (Add | Sub | Mult | Div)  -> 
                        if (t1 = Int && t2 = Int) then Int
                        else raise (Failure "Types to arithmetic operators +, -, *, / must both be Int")
                  | (Equal | Neq | Less | Leq | Greater | Geq) ->
                        if (t1 = Int && t2 = Int) then Boolean 
                        else raise (Failure "Types to integer comparison
                        operators ==, !=, <, <=, >, >= must be the same")
                  | StrEqual ->
                        if (t1 = String && t2 = String) then Boolean
                        else raise (Failure "Types to ~~ must both be String")
                  | (And | Or) ->
                        if (t1 = Boolean && t2 = Boolean) then Boolean
                        else raise (Failure "Types to binary boolean operators AND, OR must both be Boolean")
                  | Not -> raise (Failure "NOT takes a single operand") 
                end in (Ast.Binop(e1, op, e2), typ)
        | Ast.Assign(name, expr) ->
                let vdecl = (try find_variable env.scope name 
                with Not_found -> raise (Failure ("undeclared identifier " ^
                name))) in
                let (typ, name) = get_var_type_name vdecl in
                let (expr, typ) = check_expr env expr in
                (Ast.Assign(name, expr), typ)
        | Ast.ArrayAssign(name, expr1, expr2) ->
                let vdecl = (try find_variable env.scope name 
                with Not_found -> raise (Failure ("undeclared identifier " ^
                name))) in
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
                let vdecl = (try find_variable env.scope name 
                with Not_found -> raise (Failure ("undeclared identifier " ^
                name))) in
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
                             NOT take two operands")
                         end in
                (Ast.Boolneg(op, expr), typ)
       | Ast.Call(fname, expr_list) ->
               (*TODO: figure out why Call doesn't catch err in tests *)
                let fdecl =  (try find_function env.scope fname  
                             with Not_found -> begin match env.current_func with 
                             Some(current_func) -> 
                                if (current_func.fname = fname) then
                                    current_func 
                                else 
                                    raise (Failure ("Function " ^ fname ^ " does not exist."))
                            |_-> raise (Failure ("Function " ^ fname ^ " does not exist."))end) in
                    let (typ, fname) = (fdecl.freturntype, fdecl.fname) in
                    let formals = fdecl.formals in
                    let (_, expr_list) = List.map2 ( 
                        fun args params ->
                            let (formaltyp, formalname) = get_var_type_name args in 
                            let (pexpr, ptyp) = check_expr env params in
                            if formaltyp = ptyp then pexpr
                            else raise (Failure "Type mismatch between formal argument and parameter")
                    ) formals, expr_list in
                    (Ast.Call(fname, expr_list), typ)
       | Ast.End -> (Ast.End, Ast.Int) (* This type is BS; will remove later *)
      (* TODO: Access operator for rooms, need to check that the thing is in the
       * room_decl, which will be stored in the environment *)

let rec check_stmt env = function
        Block(stmt_list) -> 
            (* We don't need this scope stuff because vars are not
             * declared in this stmt type but keeping here for now *)
            let scope' = { parent = Some(env.scope); variables = []; functions =
                env.scope.functions; room_def = env.scope.room_def; pred_stmts = env.scope.pred_stmts; rooms = env.scope.rooms } in
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

let check_var_decl (env: translation_environment) vdecl = 
        (*Array_decl of variable_type * expr * string
        | Var of variable_type * string
        | VarInit of variable_type * string * expr*)
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
                    if exprtyp = Int then 
                    (env.scope.variables <- Array_decl (exprtyp,expr,name)::env.scope.variables; Array_decl (exprtyp, expr,name)) 
                    else raise (Failure ("Array size must be integer"))
              | Var(typ, name) -> let typ = check_valid_type typ in 
                        (*we dont know why this isnt working*)
                     env.scope.variables <- Var(typ, name)::env.scope.variables; Var(typ, name)
              | VarInit(typ, name, expr) -> let typ = check_valid_type typ in 
                    let (expr, exprtyp) = check_expr env expr in 
                    if exprtyp = typ then 
                        (env.scope.variables <- VarInit (exprtyp,name,expr)::env.scope.variables; VarInit (exprtyp, name,expr)) 
                    else raise (Failure ("Type mismatch in variable initialization"))

let check_var_decls (env: translation_environment) var_decls = 
    let var_decls = List.map(fun vdecl -> check_var_decl env vdecl) var_decls in
    var_decls

(* make sure return statement of function returns proper type, check the
 * statements inside the function, add the declared variables to the scope, have
 * a new scope for the function *)
let check_func_decl (env: translation_environment) func_decl = 
    if (List.exists(fun fdecl -> fdecl.fname = func_decl.fname) env.scope.functions 
        || func_decl.fname = "print")
    then raise (Failure ("Function with " ^ func_decl.fname ^ " already
    exists."))
    else
        let scope' = { parent = Some(env.scope); variables = []; functions =
            env.scope.functions; room_def = env.scope.room_def; pred_stmts =
                env.scope.pred_stmts; rooms = env.scope.rooms} in
        let env' = { env with scope = scope'; return_type = func_decl.freturntype; current_func = Some(func_decl)} in
        let fformals = List.map (
            fun f -> match f with 
                     Var(typ, name) ->
                         let typ = check_valid_type typ in 
                         env'.scope.variables <- Var(typ,
                         name)::env'.scope.variables; Var(typ, name)
                   | _ -> raise (Failure ("Formal argument must be of type Var"))
                   ) func_decl.formals in
        let flocals = check_var_decls env' func_decl.locals in 
        let fbody = func_decl.body in 
        let fbody = List.map (fun s -> check_stmt env' s) fbody in
        let ffreturntype = check_valid_type func_decl.freturntype in
        let new_func_decl = { func_decl with  body = fbody; locals = flocals;
        formals = fformals; freturntype = ffreturntype; } in
        env.scope.functions <- new_func_decl::env.scope.functions ; new_func_decl 

let check_func_decls env func_decls =
    let func_decls = List.map (fun f -> check_func_decl env f) func_decls in 
    func_decls

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


let process_room_decl_body (scope: symbol_table) (rfa: Ast.stmt) = begin match rfa with
    Ast.Expr(roomAssign) -> begin match roomAssign with (* check that the expr is in the form of an assign*)
            Ast.Assign(fieldname, expr) ->
                if (List.exists (fun rdecl -> rdecl.rname = fieldname) scope.rooms) then
                        (*let (expr, typ) = check_expr scope expr in 
                        Ast.Assign(fieldname, )*)
                    rfa
                else raise (Failure "field name in room decl does not exist.") 
            | _ -> raise (Failure "room assignment not correct format.") end
    | _ -> raise (Failure "room assignment not correct format.") end
    (*warning being thrown here saying pattern moacthing not exhaustive not sure why *)


let process_room_decl (scope: symbol_table) (room: Ast.room_decl) = 
    let name = room.rname in 
    let body = room.rbody in (* body is a list of stmts*)
        try let _ = find_room scope name in raise(Failure ("Room with name " ^ name ^ " already exists."))
        with Not_found ->
        let checked_body = List.map ( fun unchecked -> process_room_decl_body scope unchecked) body in
        (* check that number of fields in room_def match with number of fields in room decl*)
        let num_stmts = List.length(checked_body) in
            if (num_stmts <> List.length(scope.room_def)) then
                raise (Failure "number of room decl fields do not match definition.")
            else 
                (* add the room_decls to the scope*)
                let checked_room_decl = { rname = name; rbody = checked_body } in 
                scope.rooms <- checked_room_decl::scope.rooms ;
                checked_room_decl (* return this *)

let check_room_decls (env: translation_environment) rooms = 
    try
        let checked_room_decls = List.map (fun unchecked -> process_room_decl env.scope unchecked) rooms in
        checked_room_decls
    with
    | _-> raise (Failure "room decls didn't check out")


(* fields in room_def are valid variable types *)
let check_room_def (env: translation_environment) (r: Ast.room_def) = 
    try
        let checked_fields = List.map ( fun room_field -> process_room_field room_field env.scope) r in
        checked_fields
    with
    | _ -> raise (Failure "room defs didn't check out")

let check_pred_stmt (env: translation_environment) pstmt =
    (* check that the expr is a boolean expr, check all var decls, check all
     * stmts in body *)
    let scope' = { parent = Some(env.scope); variables = []; functions =
        env.scope.functions; room_def = env.scope.room_def; pred_stmts =
            env.scope.pred_stmts; rooms = env.scope.rooms} in
    let env' = { env with scope = scope'; } in
    let (checked_pred, typ) = check_expr env pstmt.pred in
    if typ = Boolean then
        let checked_locals = check_var_decls env' pstmt.locals in
        let checked_body = List.map (fun s -> check_stmt env' s) pstmt.body in
        let new_pstmt = {pred = checked_pred; locals = checked_locals; body =
            checked_body;} in
        env.scope.pred_stmts <- new_pstmt::env.scope.pred_stmts ; new_pstmt
    else raise (Failure "Expression in predicate statement conditional must be
    of type Boolean")

(* do we need to store the pred_stmts so that we can prevent multiple pred_stmts
 * with the same condition? Would we compare the exprs in that case? Decompose
 * expr into all possible boolean exprs, compare operands and operator*)
let check_pred_stmts (env: translation_environment) pstmts = 
    let new_pstmts = List.map (fun s -> check_pred_stmt env s) pstmts in
    new_pstmts

let check_program (p : Ast.program) =
        (* at the start symbol table is empty *)
       let print_func = { freturntype = Void; fname = "print"; formals = [];
       locals = []; body = [];} in
       let symbol_table = { parent = None; variables = []; functions =
           [print_func]; room_def = []; pred_stmts = []; rooms = []} in
       let env = { scope = symbol_table; return_type =
           Ast.Int; current_func = None } in
        let (room_def, room_decls, adj_decls, start, npc_defs, npc_decls, item_defs,
             item_decls, var_decls, pred_stmts, funcs) = p in
       let checked_room_def = check_room_def env room_def in
       let checked_room_decls = check_room_decls env room_decls in
       let checked_var_decls = check_var_decls env var_decls in
       let checked_funcs = check_func_decls env funcs in
       let checked_pred_stmts = check_pred_stmts env pred_stmts in
    (checked_room_def, checked_room_decls, adj_decls, start, npc_defs, npc_decls, item_defs,
    item_decls, checked_var_decls, checked_pred_stmts, checked_funcs)
