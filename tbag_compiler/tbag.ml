open Printf

let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let program_text = Java_builder.rearrange program in
		printf "%s" program_text;   (* write something *)   
                flush stdout;
        (*let listing = JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing*)
