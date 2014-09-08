module AbstractParser.Tokens

open YC.FST.AbstractLexing.Interpreter

type Token =
    | NUMBER of (string*array<Position<string>>)
    | MINUS of (string*array<Position<string>>)
    | LBRACE of (string*array<Position<string>>)
    | RBRACE of (string*array<Position<string>>)
    | DIV of (string*array<Position<string>>)
    | PLUS of (string*array<Position<string>>)
    | POW of (string*array<Position<string>>)
    | MULT of (string*array<Position<string>>)
    | LITERAL of (string*array<Position<string>>)
    | RNGLR_EOF of (string*array<Position<string>>)