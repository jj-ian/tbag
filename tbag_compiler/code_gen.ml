open Printf
open Jast
open Ast

let driver_file = "Driver.java"
let room_file = "Room.java"
let npc_file = "Npc.java"
let item_file = "Item.java"

let rec data_type = function
        String -> "String"
        | Int -> "int"
        | Void -> "void"
        | Array(var_type, size) -> data_type var_type ^ "[" ^ string_of_int size ^ "]"
        | Boolean -> "boolean"

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
        | And -> "&&"
        | Or -> "||"
        | Not -> "!"
        | StrEqual -> ".equals("

let check_str_eq = function 
        StrEqual -> true
        | _ -> false

let rec expression = function
        StrLiteral(str) -> str
        | IntLiteral(i) -> string_of_int i
        | NegIntLiteral(i) -> "-" ^ string_of_int i
        | BoolLiteral(boolean) -> string_of_bool boolean
        | Id(id) -> id
        | Access(id, field)     ->      id ^ "." ^ field
        | Assign(id, expr) -> id ^ " = " ^ (expression expr)
        | ArrayAssign(id, loc, expr) ->  id ^ "[" ^ (expression loc) ^ "] = " ^ (expression expr)
        | ArrayAccess(id, loc) -> id ^ "[" ^ (expression loc) ^ "]"
        | Binop(expr1, op, expr2) -> if check_str_eq op then ((expression expr1) ^ (operator op) ^ (expression expr2)) ^ ")"
                                     else ((expression expr1) ^ (operator op) ^ (expression expr2))
        | Boolneg(op, expr) -> ((operator op) ^ (expression expr))
        | Call(fname, arg) -> 
	        let rec expr_list = function
	        [] -> ""
	        | [solo] -> (expression solo)
	        | hd::tl -> ((expression hd) ^ "," ^ (expr_list tl))
	        in (
	                (if fname = "get_input_from_options" then "promptForInput(new String[]{" ^ expr_list arg ^ "})"
                     else (
                     if fname = "print" then "System.out.print" 
                     else fname)
	                ^ "(" ^ expr_list arg ^ ")")
                    )
        | End -> "break"

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
                        | Goto(str)     ->      "movePlayerToRoom(" ^ str ^ ");\n"
    		in
				((statement hd) ^ (statement_list tl))  

let formal = function
        Argument(datatype, id) -> ((data_type datatype) ^ " " ^ id)

let rec formals_list = function
        [] -> ""
        | [solo] -> formal solo
        | hd::tl -> ((formal hd) ^ "," ^ (formals_list tl)) 

let local = function
        Array_decl(var_type, expr, str) ->      ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]")
        | Var(var_type, str)      ->      ((data_type var_type) ^ " " ^ str)
        | VarInit(var_type, str, expr)    ->      ((data_type var_type) ^ " " ^
        str ^ " = " ^ (expression expr))

let rec locals_list = function
        []              ->      ""
        | hd::tl        ->      ((local hd) ^ ";\n" ^ (locals_list tl))

let vdecl = function
        Array_decl(var_type, expr, str) ->      ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]")
        | Var(vtype, id)                 ->      (data_type vtype) ^ " " ^ id ^ ";\n"
        | VarInit(vtype, id, expr)     ->      (data_type vtype) ^ " " ^ id ^ " = " ^ expression_with_semi expr

let rec vdecl_list  = function
        []              ->      ""
        | hd::tl        ->      "\t" ^ (vdecl hd) ^ (vdecl_list tl)

let global_vdecl = function
        Array_decl(var_type, expr, str) ->      ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]")
        | Var(vtype, id)                 ->      "public static " ^ (data_type vtype) ^ " " ^ id ^ ";\n"
        | VarInit(vtype, id, expr)     ->      "public static " ^ (data_type vtype) ^ " " ^ id ^ " = " ^ expression_with_semi expr

let rec global_vdecl_list  = function
        []              ->      ""
        | hd::tl        ->      "\t" ^ (global_vdecl hd) ^ (global_vdecl_list tl)

let func_decl f =
        ("public static " ^ (data_type f.freturntype) ^ " " ^ f.fname ^ "("
        ^ (formals_list f.formals) ^ "){\n" ^ (locals_list f.locals) ^ (statement_list f.body) ^ "\t}\n")

let rec func_decl_list = function
        []              -> ""
        | hd::tl        -> "\t" ^ ((func_decl hd) ^ "\t" ^ (func_decl_list tl)) ^ "\n"

let rec room_props_list proplist prefix = match proplist with 
        []              -> ""
        | hd::tl        -> prefix ^ "." ^ (statement_list [hd]) ^ (room_props_list tl prefix)

let room_decl r =
        "Room " ^ r.rname ^ " = new Room();\n\troomMap.put(\"" ^ r.rname ^"\", " ^ r.rname ^ ");\n" ^ (room_props_list r.rbody r.rname)

let rec room_decl_list = function
        []              -> ""
        | hd::tl        -> "\t\t" ^ ((room_decl hd) ^ "\n" ^ (room_decl_list tl))

let adj_decl = function
        []              -> ""
        | hd::tl        -> hd ^ ".setAdjacent(" ^ (List.hd tl) ^ ");"        

let rec adj_decl_list = function
        []              -> ""
        | hd::tl        -> "\t\t" ^ ((adj_decl hd) ^ "\t" ^ (adj_decl_list tl)) ^ "\n"

let start_decl s = 
    "\t\tcurrentRoom = " ^ s ^ ";\n"

let pred_stmt s = 
        "if(" ^ (expression s.pred) ^ "){\n" ^ vdecl_list s.locals ^ statement_list s.body ^ "}"

let rec pred_stmt_list = function 
        []              -> ""
        | hd::tl        -> "\t" ^ ((pred_stmt hd) ^ "\n\t" ^ (pred_stmt_list tl)) ^ "\n"

let rec npc_props_list proplist prefix = match proplist with 
        []              -> ""
        | hd::tl        -> prefix ^ "." ^ (statement_list [hd]) ^ (npc_props_list tl prefix)

let npc_decl n =
        "Npc " ^ n.nname ^ " = new Npc();\n\t\t" ^ (npc_props_list n.nbody n.nname)

let rec npc_decl_list = function
        []              -> ""
        | hd::tl        -> "\t\t" ^ ((npc_decl hd) ^ "\n" ^ (npc_decl_list tl))

let rec item_props_list proplist prefix = match proplist with 
        []              -> ""
        | hd::tl        -> prefix ^ "." ^ (statement_list [hd]) ^ (item_props_list tl prefix)

let item_decl i =
        "Item " ^ i.iname ^ " = new Item();\n\t\t" ^ (item_props_list i.ibody i.iname)

let rec item_decl_list = function
        []              -> ""
        | hd::tl        -> "\t\t" ^ ((item_decl hd) ^ "\n" ^ (item_decl_list tl))

let default_globals = 
    "
    public static Scanner scanner;
    public static Room currentRoom;
    public static String input = \"\";
    public static HashMap<String, Room> roomMap = new HashMap<String, Room>();
    "

let driver_code (driver_class) =
        let (vars, main, fdecls, lib_funcs) = driver_class in
        "import java.util.*;\n\npublic class Driver {\n\n\t " ^
        default_globals ^ 
        global_vdecl_list vars ^        
        "public static void main(String[] args) {\n\t" ^
        "scanner = new Scanner(System.in);\n\t" ^
        room_decl_list main.rdecls ^
        adj_decl_list main.adecls ^
        start_decl main.start ^ 
        npc_decl_list main.ndecls ^
        item_decl_list main.idecls ^
        "while (true) {\n" ^
        pred_stmt_list main.predicates ^
        "}\n\t" ^ "scanner.close();\n}\n\n" ^
        func_decl_list fdecls ^ 
        lib_funcs ^
        "}\n"

let room_constructor = "\n\tpublic Room(){\n\t\tadjRooms = new HashSet<Room>();\n\t}\n"

let room_adj_functions = "\tpublic void setAdjacent(Room room){\n\t\t" ^
                                "adjRooms.add(room);\n\t\troom.adjRooms.add(this);\n\t}\n\n" ^
                                "\tpublic boolean isAdjacent(Room room){\n\t\t" ^ 
                                "return adjRooms.contains(room);\n\t}\n\t\t"

let room_adj_field = "\tpublic HashSet<Room> adjRooms;"

let room_code (room_def) =
        "import java.util.*;\n\npublic class Room {\n\tString name;\n\t" ^ (vdecl_list room_def) ^ room_adj_field ^ "\n" ^ room_constructor ^ "\n" ^ room_adj_functions ^ "\n}\n"

let npc_code (npc_def) = 
        "public class Npc {\n\n" ^ (vdecl_list npc_def) ^ "\n}\n"

let item_code (item_def) = 
        "public class Item {\n\n" ^ (vdecl_list item_def) ^ "\n}\n"

let pretty_print (driver_class, room_def, npc_def, item_def) = 
        let oc = open_out driver_file in
        fprintf oc "%s" (driver_code driver_class);
        close_out oc;
        let oc = open_out room_file in
        fprintf oc "%s" (room_code room_def);
        close_out oc;
        let oc = open_out npc_file in
        fprintf oc "%s" (npc_code npc_def);
        close_out oc;
        let oc = open_out item_file in
        fprintf oc "%s" (item_code item_def);
        close_out oc;
