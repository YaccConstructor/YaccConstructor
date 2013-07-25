module literals

open FParsec.Primitives
let rec public s   = Lexer.literal "aaaa" |>> fun (res) -> (res)   