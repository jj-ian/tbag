open Printf

let _ = 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let checked_program = Semantic_checker.check_program program in
        let jast_program = Java_builder.rearrange program in
        Code_gen.pretty_print jast_program;
