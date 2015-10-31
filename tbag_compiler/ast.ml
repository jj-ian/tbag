type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater

type expr =
    Literal of int
  | Id of string
(*  | Binop of expr * op * expr *)
  | Assign of string * expr
  | Binop of expr * op * expr
 (* | Call of string * expr list
  | Noexpr *)

type stmt =
   Expr of expr

type room_decl = {
        rname: string;
}
       
type func_decl = {
		(*freturn: string;*)
        fname : string;
        formals : string list;
        (*locals: string list;*)
        body : stmt list;
}

(*
let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"
  *)

(*
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
*)





(*
type expr =
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr *)
