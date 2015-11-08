type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type dollar = Dollar

type variable_type =
  Int
  | String
  | Array of variable_type * int

type argument_decl =
  Argument of variable_type * string

type expr =
    IntLiteral of int
  |  StrLiteral of string
  | Id of string
(*  | Binop of expr * op * expr *)
  | Assign of string * expr
  | ArrayAssign of string * int * expr
  | ArrayAccess of string * int
  | Binop of expr * op * expr
 (* | Call of string * expr list
  | Noexpr *)

type stmt =
   Block of stmt list
   | Expr of expr
   | Return of expr
   | If of expr * stmt * stmt
   | While of expr * stmt

type room_decl = {
        rname: string;
        body: stmt list;
}

type adj_decl = {
        body: string list;
}
       
type func_decl = {
		    freturntype: variable_type;
        fname : string;
        formals : argument_decl list; (* formal arguments *)
        locals: string list; (* locally defined variables *)
        body : stmt list;
}

(* let type_of_string = function
    | "int" -> Int
    | "string" -> String
 *)


type npc_decl = {
    nname: string;
    nbody: stmt list;
}

type item_decl = {
    iname: string;
    ibody: stmt list;
}

type simple_program = room_decl list * adj_decl list * func_decl list

type complex_program = room_decl list * adj_decl list * npc_decl list * 
item_decl list * func_decl list


(*
let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"
  *)

