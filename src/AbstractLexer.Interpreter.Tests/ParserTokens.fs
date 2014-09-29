module AbstractParser.Tokens

open YC.FST.AbstractLexing.Interpreter

type Token =
    | NUMBER of GraphTokenValue<string>
    | MINUS of GraphTokenValue<string>
    | LBRACE of GraphTokenValue<string>
    | RBRACE of GraphTokenValue<string>
    | DIV of GraphTokenValue<string>
    | PLUS of GraphTokenValue<string>
    | POW of GraphTokenValue<string>
    | MULT of GraphTokenValue<string>
    | LITERAL of GraphTokenValue<string>
    | RNGLR_EOF of GraphTokenValue<string>