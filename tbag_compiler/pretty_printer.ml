open Printf
open Jast
open Ast
open Sast

let driver_file = "Driver.java"
let room_file = "Room.java"
let npc_file = "Npc.java"
let item_file = "Item.java"

let rec data_type = function
        String                  -> "String"
        | Int                   -> "int"
        | Void                  -> "void"
        | Array(var_type, size) -> data_type var_type ^ "[" ^ string_of_int size ^ "]"
        | Boolean               -> "boolean"

let operator = function
        Add             -> "+"
        | Sub           -> "-"
        | Mult          -> "*"
        | Div           -> "/"
        | Equal         -> "=="
        | Neq           -> "!="
        | Less          -> "<"
        | Leq           -> "<="
        | Greater       -> ">"
        | Geq           -> ">="
        | And           -> "&&"
        | Or            -> "||"
        | Not           -> "!"

let get_id = function
        Variable(_, str)                        -> str
        | Variable_Initialization(_, str, _)    -> str
        | Array_Decl(_, str, _)       -> str

let get_type = function
        Variable(aType, _)                      -> data_type aType
        | Variable_Initialization(aType, _, _)  -> data_type aType
        | Array_Decl(aType, _, _)     -> data_type aType

let rec expr_dtl = function
        StrLiteral(str)                 -> str
        | IntLiteral(i)                 -> string_of_int i
        | BoolLiteral(boolean)          -> string_of_bool boolean
        | Id(id)                        -> get_id id
(*        | Access(id, field)     -> get_id id ^ "." ^ field *)
        | Assign(id, expr)              -> get_id id ^ " = " ^ (expression expr)
        | ArrayAssign(id, loc, expr)    -> get_id id ^ "[" ^ (expression loc) ^ "] = " ^ (expression expr)
        | ArrayAccess(id, loc)          -> get_id id ^ "[" ^ (expression loc) ^ "]"
        | Binop(expr1, op, expr2)       -> ((expression expr1) ^ (operator op) ^ (expression expr2))
        | Boolneg(op, expr)             -> ((operator op) ^ (expression expr))
        | Call(fdecl, arg)              -> 
	        let rec expr_list = function
	        [] -> ""
	        | [solo] -> (expression solo)
	        | hd::tl -> ((expression hd) ^ "," ^ (expr_list tl))
	        in (
	                (if fdecl.fname = "get_input_from_options" then "promptForInput(new String[]{" ^ expr_list arg ^ "})"
                     else (
                     if fdecl.fname = "print" then "System.out.print" 
                     else fdecl.fname)
	                ^ "(" ^ expr_list arg ^ ")")
                    )
and expression e =
    let (expr_detail, _) = e in
    expr_dtl expr_detail

let expression_with_semi (expr) = ((expression expr) ^ ";\n")


let rec statement_list = function
        []              -> ""
        | hd::tl        -> 
	        let rec statement = function
			Block(stmt_list)        -> "{" ^ (statement_list stmt_list) ^ "}"
		        | Expr(expr)            -> (expression_with_semi expr)
		        | Return(expr)          -> ("return " ^ expression_with_semi expr)
		        | If(expr, stmt1, stmt2)        -> "if (" ^ (expression expr) ^ ") " ^ (statement stmt1) ^ "else" ^ (statement stmt2)
		        | While(expr, stmt)     -> "while (" ^ (expression expr) ^ ") " ^ (statement stmt)
                        | Goto(str)             ->      "movePlayerToRoom(" ^ str ^ ");\n"
    		in
	        ((statement hd) ^ (statement_list tl))   

let local = function
        Variable(var_type, str)            -> ((data_type var_type) ^ " " ^ str)
        | Variable_Initialization(var_type, str, expr)  ->      ((data_type var_type) ^ " " ^
        str ^ " = " ^ (expression expr))
        | Array_Decl(var_type, str, expr) -> ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]")

let local_sast_to_checked f = 
        let (checked_var_decl, _) = f in
        local checked_var_decl  

let rec locals_list = function
        []              ->      ""
        | hd::tl        ->      ((local_sast_to_checked hd) ^ ";\n" ^ (locals_list tl))

let vdecl = function
(*         Array_Decl(var_type, str, expr) ->      ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]") *)
        | Variable(vtype, id)                ->      (data_type vtype) ^ " " ^ id ^ ";\n"
        | Variable_Initialization(vtype, id, expr)      ->      (data_type vtype) ^ " " ^ id ^ " = " ^ expression_with_semi expr

let rec vdecl_list  = function
        []              ->      ""
        | hd::tl        ->      "\t" ^ (vdecl hd) ^ (vdecl_list tl)

let global_vdecl = function
(*         Array_Decl(var_type, str, expr) ->      ((data_type var_type) ^ "[] " ^
        str ^ "= new " ^ (data_type var_type) ^ "[" ^ (expression expr) ^ "]") *)
        | Variable(vtype, id)                ->      "public static " ^ (data_type vtype) ^ " " ^ id ^ ";\n"
        | Variable_Initialization(vtype, id, expr)      ->      "public static " ^ (data_type vtype) ^ " " ^ id ^ " = " ^ expression_with_semi expr

let rec global_vdecl_list  = function
        []              ->      ""
        | hd::tl        ->      "\t" ^ (global_vdecl hd) ^ (global_vdecl_list tl)

let formal f = (get_type f) ^ " " ^ (get_id f)

let formal_sast_to_checked f = 
        let (checked_var_decl, _) = f in
        formal checked_var_decl

let rec formals_list = function
        []              -> ""
        | [solo]        -> formal_sast_to_checked solo
        | hd::tl        -> ((formal_sast_to_checked hd) ^ "," ^ (formals_list tl)) 

let func_decl f =
        ("public static " ^ (data_type f.freturntype) ^ " " ^ f.fname ^ "("
        ^ (formals_list f.checked_formals) ^ "){\n" ^ (locals_list
        f.checked_locals) ^ (statement_list f.checked_body) ^ "\t}\n")

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

let default_globals = 
    "
    public static Scanner scanner;
    public static Room currentRoom;
    public static String input;
    public static HashMap<String, Room> roomMap = new HashMap<String, Room>();
    "

let driver_code (driver_class) =
        let (vars, main, fdecls, lib_funcs) = driver_class in
        "import java.util.*;\n\npublic class Driver {\n\n\t " ^
        default_globals ^ 
(*         global_vdecl_list vars ^        
 *)        "public static void main(String[] args) {\n\t" ^
        "scanner = new Scanner(System.in);\n\t" ^
(*         room_decl_list main.rdecls ^
 *)     adj_decl_list main.adecls ^
        start_decl main.start ^ 
        "while (true) {\n" ^
(*         pred_stmt_list main.predicates ^
 *)        "}\n\t" ^ "scanner.close();\n}\n\n" ^
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
        "import java.util.*;\n\npublic class Room {\n\n" ^ (vdecl_list room_def) ^ room_adj_field ^ "\n" ^ room_constructor ^ "\n" ^ room_adj_functions ^ "\n}\n"

let npc_code (npc_def) = 
        "public class Npc {\n\n" ^ (vdecl_list npc_def) ^ "\n}\n"

let item_code (item_def) = 
        "public class Item {\n\n" ^ (vdecl_list item_def) ^ "\n}\n"

let pretty_print (driver_class, room_def, npc_def, item_def) = 
        let oc = open_out driver_file in
        fprintf oc "%s" (driver_code driver_class);
        close_out oc;
(*         let oc = open_out room_file in
        fprintf oc "%s" (room_code room_def);
        close_out oc;
        let oc = open_out npc_file in
        fprintf oc "%s" (npc_code npc_def);
        close_out oc;
        let oc = open_out item_file in
        fprintf oc "%s" (item_code item_def);
        close_out oc; *)
