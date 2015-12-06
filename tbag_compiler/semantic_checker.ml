open Ast
open Sast
(*open Lexing*)
open Map


(*Could implement as Stringmap or hash *)
type symbol_table = {
(*  MV's group had these be mutable *)
	mutable parent : symbol_table option;
	mutable variables : (string * checked_var_decl * variable_type) list;
	mutable functions : function_decl list;

	mutable return_found : bool;
	(*  add Rooms, Items, NPCs here*)
}

let rec check_id (scope: symbol_table) id = 
	try
		let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.variables in
		decl, t
	with Not_found ->
		match scope.parent with 
			Some(parent) -> check_id parent id
		| _ -> raise Not_found

(* provides context / where are we? *)
type translation_environment = {
(* not sure why MV's group had these be mutable *)
	mutable scope : symbol_table; (* symbol table for vars *)
	found_main: bool; 
}

(* adapted from edwards' notes*)
let rec check_expr (scope : symbol_table) (expr : Ast.expr) = match expr with
	(*Noexpr -> Sast.Noexpr, Void*)
	 IntLiteral(v) -> (Sast.IntLiteral(v), Int) (* An integer constant: convert and return Int type *)
	| StrLiteral(str) -> (Sast.StrLiteral(str), String)
	(* An identifier: verify it is scope and return its type *)
	| Id(vname) -> 
		(try 
			let (decl, t) = check_id scope vname in (Sast.Id(decl), t) 
		with Not_found -> raise (Failure ("Id named " ^ vname ^ " not found")))
	| BoolLiteral(b) -> Sast.BoolLiteral(b), Sast.Boolean
	| Assign(_, _) as a -> check_assign scope a
	| Binop(_, _, _) as b -> check_op scope b
        | Boolneg(op, expr) as u -> check_uni_op scope u 
	(*| Uniop(op, expr) as u -> check_uni_op scope u
	| Call(_, _) as c -> check_call scope c
	| Access(_, _) as a -> check_access scope a
        *)

(*MV's groups check_exp below
let rec check_expr (scope : symbol_table) (expr : Ast.expr) = match expr with
	Noexpr -> Sast.Noexpr, Void
	| Id(str) -> 
		(try 
			let (decl, t) = check_id scope str in Sast.Id(decl), t 
		with Not_found -> raise (Failure ("Id named " ^ str ^ " not found")))
	| Integer_literal(i) -> Sast.IntConst(i), Sast.Int
	| String_literal(str) -> Sast.StrConst(str), Sast.String
	| Boolean_literal(b) -> Sast.BoolConst(b), Sast.Boolean
	| Array_access(_, _) as a -> check_array_access scope a
	| Assign(_, _) as a -> check_assign scope a
	| Uniop(op, expr) as u -> check_uni_op scope u
	| Binop(_, _, _) as b -> check_op scope b
	| Call(_, _) as c -> check_call scope c
	| Access(_, _) as a -> check_access scope a
	| Struct_Member_Assign(_, _, _) as a -> check_struct_assignment scope a
	| Array_Member_Assign(_, _, _) as a -> check_array_assignment scope a
*)
and check_assign (scope : symbol_table) a = match a with
        Ast.Assign(id, expr) ->
                let (decl, t) = check_id scope id in
                let e = check_expr scope expr in
                let (_, t2) = e in
                if t <> t2 then raise (Failure "Incorrect type assignment.") else Sast.Assign(decl, e), t
        | _ -> raise (Failure "Not an assignment")

and check_op (scope : symbol_table) binop = match binop with
        Ast.Binop(xp1, op, xp2) ->
                let e1 = check_expr scope xp1 and e2 = check_expr scope xp2 in
                let (_, t1) = e1 and (_, t2) = e2 in
                let t = match op with
                        Add ->
                                if (t1 <> Int || t2 <> Int) then
                                        if (t1 <> String || t2 <> String) then raise (Failure "Incorrect types for +")
                                        else String
                                else Int
                        | Sub -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for - ") else Sast.Int
                        | Mult -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for * ") else Sast.Int
                        | Div -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for / ") else Sast.Int
                        | Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else Sast.Boolean
                        | Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else Sast.Boolean
                        | Less -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for < ") else Sast.Boolean
                        | Leq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <= ") else Sast.Boolean
                        | Greater -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for > ") else Sast.Boolean
                        | Geq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >= ") else Sast.Boolean
                        | Or -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for | ") else Sast.Boolean
                        | And -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for & ") else Sast.Boolean
                        | Not -> raise (Failure "! is a unary operator.")
                in Sast.Binop(e1, op, e2), t

and check_uni_op (scope : symbol_table) uniop = match uniop with
        Ast.Boolneg(op, expr) -> (
                match op with
                        Not ->
                                let e = check_expr scope expr in
                                let (_, t) = e in 
                                if (t <> Boolean) then raise (Failure "Incorrect
type for ! ") else Sast.Boolneg(op, e), Boolean
                        | _ -> raise (Failure "Not a unary operator")
                )
        | _ -> raise (Failure "Not a uniop") 

let rec check_var_type (scope : symbol_table) (v : Ast.variable_type) = match v with
	 Ast.Int -> Sast.Int
	| Ast.String -> Sast.String
	| Ast.Array(v, i) ->
		let v = check_var_type scope v in
		(*let expr = check_expr scope expr in
		let (_, t) = expr in
		if t <> Int then raise (Failure "Array size must have integer.")
		else*) Sast.Array(v, i) 

let process_var_decl (scope : symbol_table) (v : Ast.var_decl) =
	let triple = match v with
		Var(t, name) ->
			let t = check_var_type scope t in 
			(name, Sast.Variable(t, name), t) (*return this*)
		| VarInit(t, name, expr) ->
			let t = check_var_type scope t in
			let expr = check_expr scope expr in
			let (_, t2 ) = expr in
				if t <> t2 then raise (Failure "wrong type for variable initialization") 
				else (name, Sast.Variable_Initialization(t, name, expr), t) (*return this*)
			in
	let (_, decl, t) = triple in
	if t = Void then
		raise (Failure "Variable cannot be type void.")
	else 
		(scope.variables <- triple :: scope.variables;
		(decl, t))

let rec check_stmt (scope : symbol_table) (stmt : Ast.stmt) = match stmt with
        Block(sl) -> Sast.Block(List.fold_left ( fun a s -> (check_stmt scope s) :: a) [] sl)
        | Expr(e) -> Sast.Expr(check_expr scope e)
        | Return(e) -> Sast.Return(check_expr scope e)
        | If(expr, stmt1, stmt2) -> 
                let new_expr = check_expr scope expr in
                let (_, t) = new_expr in
                if t <> Sast.Boolean then
                        raise (Failure "If statement must have a boolean expression")
                else 
                        let new_stmt1 = check_stmt scope stmt1 in
                        let new_stmt2 = check_stmt scope stmt2 in
                        Sast.If(new_expr, new_stmt1, new_stmt2)
        | While(expr, stmt) ->
                let expr = check_expr scope expr in
                let (_, t) = expr in
                if t <> Sast.Boolean then
                        raise (Failure "While statement must have a boolean expression")
                else 
                        let stmt = check_stmt scope stmt in
                        Sast.While(expr, stmt)

let rec check_func_stmt (scope : symbol_table) (stml : Sast.stmt list) (ftype :
    Sast.variable_type) =
	List.iter (
		fun s -> match s with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "func return type is incorrect") else ()
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype
		| _ -> ()
	) stml

let process_func_stmt (scope : symbol_table) (stml : Ast.stmt list) (ftype :
    Sast.variable_type) = 
	List.fold_left (
		fun a s -> let stmt = check_stmt scope s in
		match stmt with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype; stmt :: a
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "while processing func statement, return type incorrect") else
			scope.return_found <- true; stmt :: a 
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype; stmt :: a
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| _ -> stmt :: a
	) [] stml

let check_func_decl (env : translation_environment) (f : Ast.func_decl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; return_found = false } in
	let t = check_var_type env.scope f.freturntype in
	
	let formals = List.fold_left ( (* formals = arguments*)
		fun a f -> match f with
		Ast.Argument(t, n) ->
			let t = check_var_type scope' t in 
			scope'.variables <- (n, Sast.Variable(t, n), t) ::
                            scope'.variables; (Sast.Variable(t, n), t) :: a
	) [] f.formals in
	(*local variables*)
	let locals = List.fold_left ( fun a l -> process_var_decl scope' l :: a ) [] f.locals in
	(* currently not handling statements, units, or return type *)
	let statements = process_func_stmt scope' f.body t in
	(*let units = List.fold_left ( fun a u -> process_func_units scope' u formals t :: a) [] f.units in*)
	(*if scope'.return_found then *)
		let f = { 	freturntype = t; 
					fname = f.fname; 
					checked_formals = formals; 
					checked_locals = locals; 
					checked_body = statements;
					(*checked_units = units*) } in
		env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *) f
	(*else (if f.ftype = Void then 
		let f = { 	ftype = t; 
					fname = f.fname; 
					checked_formals = formals; 
					checked_locals = locals; 
					checked_body = statements; 
					checked_units = units } in
		env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *) f*)
	(*else
		raise (Failure ("No return for function " ^ f.fname ^ " when return expected.")))*)


type function_table = {
	funcs : func_decl list
}

let find_func (l : function_decl list) f =
	List.find(fun c -> c.fname = f) l

let process_func_decl (env : translation_environment) (f : Ast.func_decl) =
	try
		let _ = find_func env.scope.functions f.fname in
			raise (Failure ("Function already declared with name " ^ f.fname))
	with Not_found ->
		if f.fname = "print" then raise (Failure "A function cannot be named 'print'")
		else
			check_func_decl env f
	
let check_basic_program (p : Ast.program) =
	let s = { parent = None; variables = []; functions = []; return_found = false } in
	let env = { scope = s; found_main = false } in
	let (room_defs, room_decls, adj_decls, npc_defs, npc_decls, item_defs,
        item_decls, var_decls, funcs, pred_stmt) = p in
	let funcs = 
		List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] (List.rev funcs) in
    funcs
