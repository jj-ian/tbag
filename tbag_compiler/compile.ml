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

let rec expression = function
  StrLiteral(str) -> str
  | IntLiteral(i) -> string_of_int i
  | Call(fname, arg) -> ((if fname = "print" 
                        then "System.out.println" 
                        else fname) ^ "(" ^ (expression arg) ^ ")")

let rec statements = function
  Expr(expr) -> ((expression expr) ^ ";\n")

let func_decl f =
  if f.fname = "main" then
    ("public static void main(String[] args) {\n"
      ^ (statements f.body)    
      ^ "}")

let print_java p =
  print_string ("public class Tbag { \n\n\t" 
                ^ (func_decl p)
                ^ "}")


let translate (program) = 
  print_java program

