module ANTLR_expand_mlc

open FParsec.Primitives
open FParsec.CharParsers

let rec public start   = many ( attempt((attempt (multiline_comment  |>> fun (_ as _1)   -> (_1 ))) )) |>> fun (_ as _1)   -> (_1 )

 and public multiline_comment = pstring "/*" >>= fun (_ ) -> (attempt(many ( attempt(Lexer.pCHAR))) >>= fun (_ ) -> (attempt(pstring "*/") |>> fun (_) -> (printf "comment") ))  