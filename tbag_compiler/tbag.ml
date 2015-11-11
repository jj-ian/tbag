let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        Execute.execute_prog (Compile.translate program)
