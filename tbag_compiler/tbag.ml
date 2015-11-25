open Printf

let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let cast_program = C_builder.rearrange program in
        Pretty_printer.pretty_print cast_program;
                (*
		printf "%s" program_text;   (* write something *)   
                flush stdout;
                *)
        (*let listing = JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing*)
