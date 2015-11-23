open Printf
open Jast
open Ast

let driver_file = "Driver.java"
let room_file = "Room.java"

let rec data_type = function
        String -> "String"
        | Int -> "int"
        | Void -> "void"
        | Array(var_type, size) -> data_type var_type ^ "[" ^ string_of_int size ^ "]"

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
        | Assign(id, expr) -> id ^ " = " ^ (expression expr)
        | ArrayAssign(id, loc, expr) ->  id ^ "[" ^ (string_of_int loc) ^ "] = " ^ (expression expr)
        | ArrayAccess(id, loc) -> id ^ "[" ^ (string_of_int loc) ^ "]"
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


let rec statement_list = function
        [] -> ""
        | hd::tl -> 
	        let rec statement = function
				Block(stmt_list) -> "{" ^ (statement_list stmt_list) ^ "}"
		        | Expr(expr) -> (expression_with_semi expr)
		        | Return(expr) -> ("return " ^ expression_with_semi expr)
		        | If(expr, stmt1, stmt2) -> "if (" ^ (expression expr) ^ ") " ^ (statement stmt1) ^ "else" ^ (statement stmt2)
		        | While(expr, stmt) -> "while (" ^ (expression expr) ^ ") " ^ (statement stmt)
    		in
				("\t\t" ^ (statement hd) ^ (statement_list tl))  

let formal = function
        Argument(datatype, id) -> ((data_type datatype) ^ " " ^ id)

let rec formals_list = function
        [] -> ""
        | [solo] -> formal solo
        | hd::tl -> ((formal hd) ^ "," ^ (formals_list tl)) 

let room_decl r =
        ("Room " ^ r.rname ^ " = new Room();")

let func_decl f =
		("public static " ^ (data_type f.freturntype) ^ " " ^ f.fname ^ "("
        ^ (formals_list f.formals) ^ "){\n" ^ (statement_list f.body) ^ "\t}\n")

let rec room_decl_list = function
        []              -> ""
        | hd::tl        -> "\t\t" ^ ((room_decl hd) ^ "\n" ^ (room_decl_list tl))

let rec func_decl_list = function
        []              -> ""
        | hd::tl        -> "\t" ^ ((func_decl hd) ^ "\t" ^ (func_decl_list tl)) ^ "\n"

let driver_code (driver_class) =
		let (main, fdecls) = driver_class in
        "public class Driver {\n\n\tpublic static void main(String[] args) {\n" ^
(*         room_decl_list main.rdecls ^
        adj_decl_list main.adecls ^ *)
        statement_list main.mmethod.body ^
        "\t}\n\n" ^
        func_decl_list fdecls ^ "}\n"

let room_code (room_def) =
        "public class Room {\n\n\t}\n"

let pretty_print (driver_class, room_def, npc_def, item_def) = 
        let oc = open_out driver_file in
        fprintf oc "%s" (driver_code driver_class);
        close_out oc;
        let oc = open_out room_file in
        fprintf oc "%s" (room_code room_def);
        close_out oc;

