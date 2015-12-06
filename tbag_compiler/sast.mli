open Ast

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int
        | Boolean
and checked_var_decl =
	Variable of variable_type * string
	| Variable_Initialization of variable_type * string * expression
	| Array_Initialization of variable_type * string * expression list
and expr_detail =
        IntLiteral of int
        | StrLiteral of string
        | BoolLiteral of bool
        | Id of checked_var_decl
        | Assign of checked_var_decl * expression
        | ArrayAssign of checked_var_decl * int * expression
        | ArrayAccess of checked_var_decl * int
        | Binop of expression * op * expression 
        | Boolneg of op * expression
        | Call of func_decl * expression list
and expression = expr_detail * variable_type
and sast_var_decl = checked_var_decl * variable_type
and stmt =
        Block of stmt list
        | Expr of expression
        | Return of expression
        | If of expression * stmt * stmt
        | While of expression * stmt
        | Goto of string
and function_decl = {
	freturntype: variable_type;
	fname : string; (* Name of the function *)
	checked_formals : sast_var_decl list; (* Formal argument names *)
	checked_locals : sast_var_decl list; (* Locally defined variables *)
	checked_body : stmt list; (* Body of the function *)
}



