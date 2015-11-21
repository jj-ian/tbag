open Ast

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int

type expression = expr * variable_type

type expr =
        IntLiteral of int
        | StrLiteral of string
        | Id of string
        | Assign of string * expr
        | ArrayAssign of string * int * expr
        | ArrayAccess of string * int
        | Binop of expr * op * expr 
        | Call of string * expr list