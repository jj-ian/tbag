open Printf

let driver_file = "Driver.java"

let driver_code (driver_class) =
        "public class Driver {\n\tpublic static void main(String[] args) {
        }\n}\n"

let pretty_print (driver_class, room_def, npc_def, item_def) = 
        let oc = open_out driver_file in
        fprintf oc "%s" (driver_code driver_class);
        close_out oc;
