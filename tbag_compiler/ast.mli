type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int

type argument_decl =
        Argument of variable_type * string

type expr =
        IntLiteral of int
        | StrLiteral of string
        | Id of string
        | Assign of string * expr
        | ArrayAssign of string * int * expr
        | ArrayAccess of string * int
        | Binop of expr * op * expr 
        | Call of string * expr list

type var_decl =
    Var of variable_type * string
    | VarInit of variable_type * string * expr

type stmt =
        Block of stmt list
        | Expr of expr
        | Return of expr
        | If of expr * stmt * stmt
        | While of expr * stmt

type room_def = var_decl list

type room_decl = {
        name: string;
        body: stmt list;
}

type adj_decl = string list
       
type func_decl = {
        returntype: variable_type;
        name : string;
        formals : argument_decl list; 
        locals: var_decl list;
        body : stmt list;
}

type npc_def = var_decl list

type npc_decl = {
        name: string;
        body: stmt list;
}

type item_def = var_decl list

type item_decl = {
        name: string;
        body: stmt list;
}

type basic_program = func_decl list

type simple_program = room_decl list * func_decl list

type program = room_decl list * adj_decl list * npc_decl list * item_decl list * func_decl list