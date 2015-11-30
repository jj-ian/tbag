open Ast
open Sast
(*open Lexing*)
open Map


(*Could implement as Stringmap or hash *)
type symbol_table = {
(*  MV's group had these be mutable *)
	mutable parent : symbol_table option;
	mutable variables : (string * checked_var_decl * variable_type) list;
	mutable functions : func_decl list;

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

let rec check_var_type (scope : symbol_table) (v : Ast.variable_type) = match v with
	 Ast.Int -> Sast.Int
	| Ast.String -> Sast.String
	| Ast.Array(v, i) ->
		let v = check_var_type scope v in
		(*let expr = check_expr scope expr in
		let (_, t) = expr in
		if t <> Int then raise (Failure "Array size must have integer.")
		else*) Sast.Array(v, i) 

let check_func_decl (env : translation_environment) (f : Ast.func_decl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; return_found = false } in
	let t = check_var_type env.scope f.freturntype in
	
	let formals = List.fold_left ( (* formals = arguments*)
		fun a f -> match f with
		Ast.Argument(t, n) ->
			let t = check_var_type scope' t in 
			scope'.variables <- (n, Sast.Var(t, n), t) :: scope'.variables; (Sast.Var(t, n), t) :: a
	) [] f.formals in
	(*local variables*)
	let locals = List.fold_left ( fun a l -> process_var_decl scope' l :: a ) [] f.locals in
	(* currently not handling statements, units, or return type *)
	(*let statements = process_func_stmt scope' f.body t in *)
	(*let units = List.fold_left ( fun a u -> process_func_units scope' u formals t :: a) [] f.units in*)
	(*if scope'.return_found then *)
		let f = { 	freturntype = t; 
					fname = f.fname; 
					checked_formals = formals; 
					checked_locals = locals; 
					(*checked_body = statements; 
					checked_units = units*) } in
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

let process_var_decl (scope : symbol_table) (v : Ast.var_decl) =
	let triple = match v with
		Var(t, name) ->
			let t = check_var_type scope t in 
			(name, Sast.Var(t, name), t) (*return this*)
		| VarInit(t, name, expr) ->
			let t = check_var_type scope t in
			let expr = check_expr scope expr in
			let (_, t2 ) = expr in
				if t <> t2 then raise (Failure "wrong type for variable initialization") 
				else (name, Sast.VarInit(t, name, expr), t) (*return this*)
			in
	let (_, decl, t) = triple in
	if t = Void then
		raise (Failure "Variable cannot be type void.")
	else 
		(scope.variables <- triple :: scope.variables;
		(decl, t))

	
let check_basic_program (p : Ast.program) =
	let s = { parent = None; variables = []; functions = []; return_found = false } in
	let env = { scope = s; found_main = false } in
	let funcs = p in 	
	let funcs = 
		List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] (List.rev funcs) in
    funcs
