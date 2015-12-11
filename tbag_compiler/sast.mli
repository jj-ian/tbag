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
	| Variable_Initialization of variable_type * string * expression
	| Array_Initialization of variable_type * string * expression list
and expr_detail =
        IntLiteral of int
        | StrLiteral of string
        | BoolLiteral of bool
        | Id of checked_var_decl
        | Assign of checked_var_decl * expression
        | ArrayAssign of checked_var_decl *  expression
        | ArrayAccess of checked_var_decl * expression
        | Binop of expression * op * expression 
        | Boolneg of op * expression
        | Call of function_decl * expression list
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

and pred_stmt = 
        {
                pred: expression;
                locals: checked_var_decl list;
                body: stmt list;
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
                function_decl list *
                pred_stmt list

type room_def = checked_var_decl list

type room_decl = 
{
        rname: string;
        rbody: stmt list;
}

type start = string

type adj_decl = string list

type npc_def = checked_var_decl list

type npc_decl = 
{
        nname: string;
        nbody: stmt list;
}

type item_def = checked_var_decl list

type item_decl = 
{
        iname: string;
        ibody: stmt list;
}

type basic_program = function_decl list

type simple_program = room_decl list * 
                      function_decl list

type room_program = room_def *
                    room_decl list * 
                    function_decl list
