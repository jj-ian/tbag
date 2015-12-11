open Ast

type argument_decl =
        Argument of variable_type * string

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int
        | Boolean
and checked_var_decl =
	Variable of variable_type * string
	| Variable_Initialization of variable_type * string * sast_expr
	| Array_Initialization of variable_type * string * sast_expr list
and expr_detail =
        IntLiteral of int
        | StrLiteral of string
        | BoolLiteral of bool
        | Id of checked_var_decl
        | Assign of checked_var_decl * sast_expr
        | ArrayAssign of checked_var_decl *  sast_expr
        | ArrayAccess of checked_var_decl * sast_expr
        | Binop of sast_expr * op * sast_expr 
        | Boolneg of op * sast_expr
        | Call of checked_func_decl * sast_expr list
and sast_expr = expr_detail * variable_type
and sast_var_decl = checked_var_decl * variable_type
and sast_stmt =
        Block of sast_stmt list
        | Expr of sast_expr
        | Return of sast_expr
        | If of sast_expr * sast_stmt * sast_stmt
        | While of sast_expr * sast_stmt
        | Goto of string
and checked_func_decl = {
	freturntype: variable_type;
	fname : string; (* Name of the function *)
	checked_formals : sast_var_decl list; (* Formal argument names *)
	checked_locals : sast_var_decl list; (* Locally defined variables *)
	checked_body : sast_stmt list; (* Body of the function *)
}

and pred_stmt = 
        {
                pred: sast_expr;
                locals: checked_var_decl list;
                body: sast_stmt list;
        }


type program =  room_def * 
                room_decl list * 
                adj_decl list * 
                start *                
                npc_def *
                npc_decl list * 
                item_def *
                item_decl list * 
                checked_var_decl list *
                checked_func_decl list *
                pred_stmt list

type room_def = checked_var_decl list

type room_decl = 
{
        rname: string;
        rbody: sast_stmt list;
}

type start = string

type adj_decl = string list

type npc_def = checked_var_decl list

type npc_decl = 
{
        nname: string;
        nbody: sast_stmt list;
}

type item_def = checked_var_decl list

type item_decl = 
{
        iname: string;
        ibody: sast_stmt list;
}

type basic_program = checked_func_decl list

type simple_program = room_decl list * 
                      checked_func_decl list

type room_program = room_def *
                    room_decl list * 
                    checked_func_decl list
