open Printf

let file = "Tbag.java"

let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.basic_program Scanner.token lexbuf in
        let program_text = Compile.translate program in
		let oc = open_out file in    (* create or truncate file, return channel *)
		fprintf oc "%s" program_text;   (* write something *)   
		close_out oc;         
        (*let listing = JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing*)
