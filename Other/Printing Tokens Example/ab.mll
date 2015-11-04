rule token = parse
    [' ' '\t'] { token lexbuf }
  | '\n'       { 1 }
  | '+'        { 2 }
  | _          { 3 }
{
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            ignore (token lexbuf)
        done
    with _ -> exit 0
}