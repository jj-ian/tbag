type op = Add | Sub | Mult | Div | Equal | StrEqual | Neq | Less | Leq | Greater | Geq | And | Or | Not

type variable_type =
        Int
        | String
        | Void
        | Array of variable_type * int
        | Boolean

type argument_decl =
        Argument of variable_type * string

type expr =
        IntLiteral of int
        | NegIntLiteral of int
        | StrLiteral of string
        | BoolLiteral of bool
        | Id of string
        | Assign of string * expr
        | ArrayAssign of string * expr * expr
        | ArrayAccess of string * expr
        | Binop of expr * op * expr 
        | Boolneg of op * expr
        | Call of string * expr list
        | Access of string * string  (* foo.bar *)
        | End

type var_decl =
        Array_decl of variable_type * expr * string
        | Var of variable_type * string
        | VarInit of variable_type * string * expr

type stmt =
        Block of stmt list
        | Expr of expr
        | Return of expr
        | If of expr * stmt * stmt
        | While of expr * stmt
        | Goto of string

type room_def = var_decl list

type room_decl = 
{
        rname: string;
        rbody: stmt list;
}

type start = string

type adj_decl = string list

type pred_stmt = 
        {
                pred: expr;
                locals: var_decl list;
                body: stmt list;
        }
       
type func_decl = 
{
        freturntype: variable_type;
        fname : string;
        formals : argument_decl list; 
        locals: var_decl list;
        body : stmt list;
}

type npc_def = var_decl list

type npc_decl = 
{
        nname: string;
        nbody: stmt list;
}

type item_def = var_decl list

type item_decl = 
{
        iname: string;
        ibody: stmt list;
}

type basic_program = func_decl list

type simple_program = room_decl list * 
                      func_decl list

type room_program = room_def *
                    room_decl list * 
                    func_decl list

type program =  room_def * 
                room_decl list * 
                adj_decl list * 
                start *                
                npc_def *
                npc_decl list * 
                item_def *
                item_decl list * 
                var_decl list *
                pred_stmt list *
                func_decl list 
