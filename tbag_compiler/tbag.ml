open Printf


let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.basic_program Scanner.token lexbuf in
        let program_text = Compile.translate program in
		printf "%s" program_text;   (* write something *)   
                flush stdout;
        (*let listing = JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing*)
