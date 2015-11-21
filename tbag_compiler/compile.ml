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
        in (
                (if fname = "print" then "System.out.println" else fname)
                ^ "(" ^ expr_list arg ^ ")")

let expression_with_semi (expr) = 
        ((expression expr) ^ ";\n")

let rec statement = function
        Expr(expr) -> (expression_with_semi expr)
        | Return(expr) -> ("return " ^ expression_with_semi expr)

let rec statement_list = function
        [] -> ""
        | hd::tl -> ("\t\t" ^ (statement hd) ^ (statement_list tl))  

let formal = function
        Argument(datatype, id) -> ((data_type datatype) ^ " " ^ id)

let rec formals_list = function
        [] -> ""
        | [solo] -> formal solo
        | hd::tl -> ((formal hd) ^ "," ^ (formals_list tl)) 

let room_decl r =
        ("Room " ^ r.name ^ " = new Room();\n")

let func_decl f =
        if f.name = "main" then
                ("public static void main(String[] args) {\n" ^ (statement_list f.body)    
                ^ "\t}\n")
        else
                ("public static " ^ (data_type f.returntype) ^ " " ^ f.name ^ "("
                ^ (formals_list f.formals) ^ "){\n" ^ (statement_list f.body) ^ "\n\t}\n")

let rec room_decl_list = function
        []              -> ""
        | hd::tl        -> ((room_decl hd) ^ "\n\t" ^ (room_decl_list tl))

let rec func_decl_list = function
        []              -> ""
        | hd::tl        -> ((func_decl hd) ^ "\n\t" ^ (func_decl_list tl))

let print_java (rooms, functions) =
        ("public class test_hello_world { \n\n\t" ^ "public class Room {\n\t\tString msg;\n\t}\n\n\t" ^
        (room_decl_list rooms)  ^ "\n\n\t" ^ (func_decl_list functions) ^ "\n}")

let translate (program) = 
        print_java program

