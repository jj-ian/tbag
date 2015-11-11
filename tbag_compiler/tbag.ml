let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.simple_program Scanner.token lexbuf in
        JavaCode.string_of_prog (Compile.translate program) in
        print_endline listing
