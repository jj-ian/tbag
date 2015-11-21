open Printf

let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let jast_program = Java_builder.rearrange program in
        let test2 = print_string "jast completed!\n" in
                Pretty_printer.pretty_print jast_program;
                (*
		printf "%s" program_text;   (* write something *)   
                flush stdout;
                *)
        (*let listing = JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing*)
