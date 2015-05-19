module TSQLParserToken

open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation

type Token =
    | DEC_NUMBER of FSA<char*Position<string>>
    | L_comma_ of FSA<char*Position<string>>
    | L_equal_ of FSA<char*Position<string>>
    | L_more_ of FSA<char*Position<string>>
    | L_less_ of FSA<char*Position<string>>
    | L_colon_ of FSA<char*Position<string>>
    | L_left_bracket_ of FSA<char*Position<string>>
    | L_right_bracket_ of FSA<char*Position<string>>
    | L_plus_ of FSA<char*Position<string>>
    | L_minus_ of FSA<char*Position<string>>
    | L_star_ of FSA<char*Position<string>>
    | L_select of FSA<char*Position<string>>
    | L_from of FSA<char*Position<string>>
    | L_where of FSA<char*Position<string>>
    | L_and_ of FSA<char*Position<string>>
    | L_or_ of FSA<char*Position<string>>
    | IDENT of FSA<char*Position<string>>
    | RNGLR_EOF of FSA<char*Position<string>>