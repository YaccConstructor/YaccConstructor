//// MADE FROM H22_H23.yrd
module internal GrammarCombinator.Examples.Bio16s
open GrammarCombinator.Combinators
open GrammarCombinator.GrammarGenerator

let (^<|) = (<|)

let any =
    "AUGC".ToCharArray()
    |> List.ofArray
    |> List.map (fun x -> tok <| x.ToString())
    |> List.reduce (<|>)
//    tok "A" <|> tok "U" <|> tok "G" <|> tok "C"

let stem s =
    <@
        let rec stem() =
            tok "A" + stem() + tok "U"
            <|> tok "U" + stem() + tok "A"
            <|> tok "C" + stem() + tok "G"
            <|> tok "G" + stem() + tok "C"
            <|> tok "G" + stem() + tok "U"
            <|> tok "U" + stem() + tok "G"
            <|> tok "G" + stem() + tok "A"
            <|> tok "A" + stem() + tok "G"
            <|> %s
        stem()
    @>

let gstem s =
    <@
//        tok "A" + %(stem s) + tok "U"
//        <|> tok "U" + %(stem s) + tok "A"
//        <|> tok "C" + %(stem s) + tok "G"
//        <|> tok "G" + %(stem s) + tok "C"
//        <|> tok "G" + %(stem s) + tok "U"
//        <|> tok "U" + %(stem s) + tok "G"
//        <|> tok "G" + %(stem s) + tok "A"
//        <|> tok "A" + %(stem s) + tok "G"
//        <|> %s
        %(stem s)
        <|> tok "G" + %(stem s) + tok "G"
    @>

let stem_e1 s = stem <@ !? any + %(stem s) + !? any @>

let stem_e2 s = stem <@ !? any + %(stem_e1 s) + !? any @>

let any_1_2 = any % (1, 2)
let any_1_3 = any % (1, 3)
let any_2_3 = any + any_1_2
let any_2_4 = any % (2, 4)
let any_3_4 = any % (3, 4)
let any_3_5 = any + any_2_4
let any_5_7 = any + any + any_3_5
let any_4_6 = any + any_3_5
let any_6_8 = any + any_5_7
let any_9_11 = any % (9, 11)
let any_4 = any + any + any + any
let s2 = stem <@ any % (4, 8) @>
let s1 = stem <@ any % (1, 4) + %s2 + any % (2, 7) @>
let h23 = stem <@ any_1_3 + %s1 + any_1_3 @>
let h24 = gstem <@ any_1_3 + %(stem <@ tok "G" + tok "A" + tok "A" + tok "G" @>) @>
let s5 = stem <@ %h23 + any % (3, 7) + %h24 @>
let s4 = stem <@ any_1_3 + %s5 + any_1_3 @>
let h22 = stem <@ %s4 + any @>
let s8 = stem_e2 <@ !? any + tok "C" + tok "U" + !? any + tok "A" + tok "A" + !? any @>
let s7 = stem <@ any % (2, 6) + %s8 + any_2_4 @>
let s6 = stem <@ any_2_3 + %s7 + any_3_4 @>
let h21 = stem <@ !? any + %s6 + !? any @>
let root = stem <@ any_3_4 + %(stem <@ any_1_2 + %h21 + any_2_4 + %h22 + any_2_4 @>) + any_3_4 @>
let h26 = stem <@ any_1_2 + %(stem <@ any_4_6 @>) + any_3_5 + %(stem <@ any_4 @>) + any_3_5 @>
let h25 = stem_e2 <@ any_2_3 + %(stem <@ any % (8, 10) @>) + any_2_3 @>
let h27 = stem <@ any_5_7 + %(stem <@ any_4 @>) + any_3_5 @>
let h19 = stem <@ any_5_7 + %root + any_2_4 + %h25 + any % (9, 12) + %h26 + any_1_2 @>
let h8 = stem <@ any_3_5 + %(stem <@ any_4 @>) + any_3_5 @>
let h9 = stem <@ any_4 @>
let h10 = stem_e2 <@ any_4 @>
let h6 = stem_e2 <| stem_e1 ^<| stem_e2 ^<| stem_e1 <@ any_4 @>
let h7 = stem_e2 <@ any_2_4 + %(stem <@ any_1_2 + %h8 + any_4_6 + %h9 + any_3_5 + %h10 + any_1_2 @>) + any_1_3 @>
let h11 = stem_e1 <@ !? any + %(stem_e2 <@ any_6_8 @>) + any_3_5 @>
let h12 = stem <@ !? any + %(stem <@ any_4 @>) + any_2_4 @>
let h13 = stem <@ any_9_11 @>
let h14 = stem <@ tok "U" + tok "A" + tok "C" + tok "G" @>
let h5 =
    <@
        any_5_7
        + %(stem <|
            <@
                    any_1_3
                    + %h6
                    + any_5_7
                    + %(stem <@ any_5_7 + %h7 + !? any + %h11 + any_1_3 + %h12 + !? any @>)
                    + any_1_2
                    + %h13
                    + any_1_2
                    + %h14
                    + any_2_4
            @>)
        + any_3_5
    @>
let h15 = stem_e1 <@ any_2_4 + %(stem <@ any_4 @>) + !? any @>
let h16 = stem <@ any_5_7 + %(stem <@ any_4 @>) + any_4_6 @>
let h17 = stem_e2 <@ any_2_4 + %(stem <@ any_6_8 @>) + any_3_5 @>
let h18 = stem <@ any_5_7 + %(stem <@ any_4_6 + %(stem <@ any + tok "G" + tok "C" + any @>) + any_2_3 + tok "A" + tok "A" + any_1_2 @>) @>
let h4 = stem_e1 <@ %h5 + %h15 + !? any @>
let h3 = stem_e1 <@ !? any + %h4 + any_1_3 + %h16 + !? any + %h17 + any_2_4 + %h18 @>
let root2 = <@ %h19 + any_1_2 + %h27 @>
let folded = root2
let full = <@ %folded + !? any @>
let h37 = stem <@ any_5_7 @>
let h36 = stem <@ any_4 @>
let h35 = stem <@ %h36 + any_2_3 + %h37 + any_2_3 @>
let h39 = stem <@ any_2_4 + %(stem <@ any_1_3 + %(stem <@ any_4_6 @>) @>) + any_2_4 @>
let h40 = stem <@ any_4 @>
let h38 = stem <@ any_1_2 + %h39 + any_1_3 + %h40 + any_4_6 @>
let h34 = stem_e1 <@ any_1_2 + %(stem <@ %(stem_e2 <@ any_2_4 + %h35 + any_4_6 + %h38 + any_3_5 @>) + any_2_4 @>) @>
let h33 = stem <@ any_1_3 + %(stem <@ any_4 @>) + any_1_3 + %(stem <@ any_4 @>) + any_1_3 @>
let h32 = stem <@ any_4_6 + %h33 + any_1_2 + %h34 + any_3_5 @>
let h31 = stem <@ any % (7, 9) @>
let h30 = stem_e1 <@ any_3_5 + %h31 + any % (7, 9) + %h32 + any_2_4 @>
let h41 = stem <@ any_4_6 + %(stem <@ any_1_3 + %(stem <@ any_2_4 + %(stem <@ any_4 @>) + any_2_4 @>) + any_3_5 @>) + any_4_6 @>
let h42 = stem <@ any_3_4 + %(stem <@ any % (7, 9) @>) + any_3_4 @>
let h29 = stem <@ %h30 + any_2_4 + %h41 + any_5_7 + %h42 + any_4_6 @>
let h43 = stem <@ any % (7, 9) @>
let h28_a = stem <@ any_1_3 + %h29 + any_4_6 + %h43 + any_4_6 @>
let h28 = stem_e2 <@ any + %h28_a + any_2_4 @>
let h44 =
    stem <@
            any_1_3
            + %(stem <@
                        any_2_4
                        + %(stem <@
                                    any_1_3
                                    + %(stem <@
                                                any_3_5
                                                + %(stem_e1 <@
                                                            any_1_3 + %(stem <@ any_4 @>)
                                                @>)
                                                + any_2_4
                                    @>)
                                    + any_1_3
                        @>)
                        + any_3_5
            @>)
            + any_2_3
    @>
let h45 = stem <@ any_4 @>
let root3 = <@ %h28 + any_3_5 + %h44 + any_3_5 + %h44 @>
let full_size_root = <@ %h3 + any_9_11 + %h19 + any_1_2 + %h27 + any % (7, 9) + %root3 @>
