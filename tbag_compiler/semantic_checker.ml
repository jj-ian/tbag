open Ast
open Sast
open Lexing
open Map


(*Could implement as Stringmap or hash *)
type symbol_table = {
(* again MV's group had these be mutable *)
	mutable parent : symbol_table option;
	mutable variables : (string * checked_var_decl * var_types) list;
	mutable functions : function_decl list;

	mutable return_found : bool;
	(*  add Rooms, Items, NPCs here*)
}

let rec find_variable (scope: symbol_table) name = 
	try 
		List.find (fun (s, _, _, _ ) -> s = name ) scope.variables
	with Not_found ->
		match scope.parent with 
			Some(parent) -> find_variable parent name
		| _ -> raise Not_found

(* provides context / where are we? *)
type translation_environment = {
(* not sure why MV's group had these be mutable *)
	mutable scope : symbol_table; (* symbol table for vars *)
	found_main: bool; 
}

let rec expr env = function 

	(*) An integer constant: convert and return Int type *)
	Ast.IntLiteral(v) -> Sast.IntLiteral(v), variable_type.Int (* Or the last one should be just Int, not sure of the semantic notation.*)
	
	(* An identifier: verify it is scope and return its type *)
	| Ast.Id(vname) -> 
		let vdecl = 
			try find_variable env.scope vname (* locate a variable by name *)
		with Not_found -> 
			raise (Error("undeclared identifier " ^ vname))
		in let (_, typ) = vdecl in (* get the variable's type *)
		Sast.Id(vdecl), typ


type function_table = {
	funcs : func_decl list
}


let process_var_decl (scope : symbol_table) (v : Ast.var_decl) =
	let triple = match v with
		Variable(t, name) ->
			let t = check_var_type scope t in 
			(name, Sast.Variable(t, name), t)
		| Variable_Initialization(t, name, expr) ->
			let t = check_var_type scope t in
			let expr = check_expr scope expr in
			let (_, t2 ) = expr in
			if t <> t2 then raise (Failure "wrong type for variable initialization") else (name, Sast.Variable_Initialization(t, name, expr), t) 

	let (_, decl, t) = triple in
	if t = Void then
		raise (Failure "Variable cannot be type void.")
	else 
		(scope.variables <- triple :: scope.variables;
		(decl, t))

	
let check_simple_program (p : Ast.program) =
	let s = { parent = None; variables = []; functions = []; return_found = false } in
	let env = { scope = s; found_main = false } in
	let (vars, funcs) = p in 	
	let funcs = 
		List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] (List.rev funcs) in
    funcs
