module AbstractParser.Tokens

open QuickGraph.FSA.GraphBasedFsa
open YC.Utils.StructClass

type Token =
    | NUMBER of FSA<char*Position<string>>
    | MINUS of FSA<char*Position<string>>
    | LBRACE of FSA<char*Position<string>>
    | RBRACE of FSA<char*Position<string>>
    | DIV of FSA<char*Position<string>>
    | PLUS of FSA<char*Position<string>>
    | POW of FSA<char*Position<string>>
    | MULT of FSA<char*Position<string>>
    | LITERAL of FSA<char*Position<string>>
    | RNGLR_EOF of FSA<char*Position<string>>