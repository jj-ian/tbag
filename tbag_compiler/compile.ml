open Ast

(*
module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
  }

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs
*)
(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)

(*
let translate (functions) =

  (* Allocate "addresses" for each global variable *)
  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in
  let env = { function_index = function_indexes} in

  let rec expr = function
    IntLiteral i -> "int " ^ string_of_int i 
    | StrLiteral i -> "String " ^ i

  in let rec stmt = function
    IntLiteral i -> "int " ^ string_of_int i
  in print_endline (string_of_int 1)

*)

let data_type = function
  String -> "String"
  | Int -> "int"
  | Void -> "void"

let operator = function
  Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"

let rec expression = function
  StrLiteral(str) -> str
  | IntLiteral(i) -> string_of_int i
  | Id(id) -> id
  | Binop(expr1, op, expr2) -> ((expression expr1) ^ (operator op) ^ (expression expr2))
  | Call(fname, arg) ->
      let rec expr_list = function
        [] -> ""
        | [solo] -> (expression solo)
        | hd::tl -> ((expression hd) ^ "," ^ (expr_list tl))
      in ((if fname = "print" 
                        then "System.out.println" 
                        else fname) ^ "(" ^ expr_list arg ^ ")")

let expression_with_semi (expr) = 
  ((expression expr) ^ ";\n")

let rec statement = function
  Expr(expr) -> (expression_with_semi expr)
  | Return(expr) -> ("return " ^ expression_with_semi expr)

let rec statement_list = function
  [] -> ""
  | hd::tl -> ((statement hd) ^ (statement_list tl))  

let formal = function
  Argument(datatype, id) -> ((data_type datatype) ^ " " ^ id)

let rec formals_list = function
  [] -> ""
  | [solo] -> formal solo
  | hd::tl -> ((formal hd) ^ "," ^ (formals_list tl)) 

let func_decl f =
  if f.fname = "main" then
    ("public static void main(String[] args) {\n"
      ^ (statement_list f.body)    
      ^ "\t}\n")
  else ("\tpublic static " ^ 
        (data_type f.freturntype) ^ 
        " " 
        ^ f.fname
        ^ "("
        ^ (formals_list f.formals) 
        ^ "){"
        ^ (statement_list f.body)
        ^ "}\n"
  )

let rec func_decl_list = function
  [] -> ""
  | hd::tl -> 
    ((func_decl hd) ^ (func_decl_list tl))

let print_java p =
  ("public class hello_world { \n\n\t" 
    ^ (func_decl_list p)
    ^ "\n}")

let translate (program) = 
  print_java program

