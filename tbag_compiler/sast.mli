open Ast

(*type sast_argument_decl =
        Argument of variable_type * string*)

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int
        | Boolean
and checked_var_decl =
	Variable of variable_type * string
	| Variable_Initialization of variable_type * string * sast_expr
	| Array_Decl of variable_type * string * sast_expr
and expr_detail =
        IntLiteral of int
        | StrLiteral of string
        | BoolLiteral of bool
        | Id of checked_var_decl
        | Assign of checked_var_decl * sast_expr
        | ArrayAssign of checked_var_decl * sast_expr * sast_expr
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
and sast_pred_stmt = 
        {
                pred: sast_expr;
                locals: checked_var_decl list;
                body: sast_stmt list;
        }
type sast_program =  
                room_def * 
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

type sast_room_def = checked_var_decl list

type sast_room_decl = 
{
        rname: string;
        rbody: sast_stmt list;
}

type start = string

type sast_adj_decl = string list

type sast_npc_def = checked_var_decl list

type sast_npc_decl = 
{
        nname: string;
        nbody: sast_stmt list;
}

type sast_item_def = checked_var_decl list

type sast_item_decl = 
{
        iname: string;
        ibody: sast_stmt list;
}

type sast_basic_program = checked_func_decl list

type sast_simple_program = sast_room_decl list * 
                      checked_func_decl list

type sast_room_program = sast_room_def *
                    sast_room_decl list * 
                    checked_func_decl list
