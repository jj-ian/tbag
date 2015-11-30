open Ast

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int
and checked_var_decl =
	Variable of variable_type * string
	| Variable_Initialization of variable_type * string * expression
	| Array_Initialization of variable_type * string * expression list
and expr_detail =
        IntLiteral of int
        | StrLiteral of string
        | Id of checked_var_decl
        | Assign of checked_var_decl * expr
        | ArrayAssign of checked_var_decl * int * expression
        | ArrayAccess of checked_var_decl * int
        | Binop of expression * op * expression 
        | Call of func_decl * expression list
and expression = expr_detail * variable_type
and sast_var_decl = checked_var_decl * variable_type
and function_decl = {
	freturntype: variable_type;
	fname : string; (* Name of the function *)
	checked_formals : sast_var_decl list; (* Formal argument names *)
	checked_locals : sast_var_decl list; (* Locally defined variables *)
	checked_body : stmt list;
}



