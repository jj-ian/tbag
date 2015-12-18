open Ast

(*type room_table = var_decl list;*)

type symbol_table = {
    parent : symbol_table option;
    variables : var_decl list;
    functions : func_decl list;
} 

type translation_environment = {
    scope : symbol_table;
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
                        else 
                            if (t1 = String && t2 = String) then String
                            else 
                                raise (Failure "Types to + must both be Int or
                                both be String")
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
                  | _ -> raise (Failure "will fix this later") 

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
      (* TODO: Access operator for rooms, need to check that the thing is in the
       * room_decl, which will be stored in the environment *)

