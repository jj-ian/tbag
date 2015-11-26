open Ast

type expr =
        IntLiteral of int
        | StrLiteral of string
        | Id of string
        | Assign of string * expr
        | ArrayAssign of string * int * expr
        | ArrayAccess of string * int
        | Binop of expr * op * expr 
        | Call of string * expr list

type expression = expr * variable_type

and checked_var_decl =
	Variable of variable_type * string
	| Variable_Initialization of variable_type * string * expression
	| Array_Initialization of variable_type * string * expression list

