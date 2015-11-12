open Ast
  
let data_type = function
  String -> "String"
  | Int -> "int"
  | Void -> "void"

let operator = function
  Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

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

