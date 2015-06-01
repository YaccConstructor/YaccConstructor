
# 2 "If.yrd.fs"
module RNGLR.ParseIf
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | D of (int)
    | E of (int)
    | ELSE of (int)
    | ENDIF of (int)
    | F of (int)
    | G of (int)
    | H of (int)
    | IF of (int)
    | RNGLR_EOF of (int)
    | SEMICOLON of (int)
    | THEN of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | D x -> box x
    | E x -> box x
    | ELSE x -> box x
    | ENDIF x -> box x
    | F x -> box x
    | G x -> box x
    | H x -> box x
    | IF x -> box x
    | RNGLR_EOF x -> box x
    | SEMICOLON x -> box x
    | THEN x -> box x

let numToString = function
    | 0 -> "cond"
    | 1 -> "error"
    | 2 -> "if_statement"
    | 3 -> "simple_statement"
    | 4 -> "start"
    | 5 -> "statement"
    | 6 -> "yard_opt_1"
    | 7 -> "yard_some_1"
    | 8 -> "yard_some_2"
    | 9 -> "yard_some_3"
    | 10 -> "yard_start_rule"
    | 11 -> "A"
    | 12 -> "B"
    | 13 -> "C"
    | 14 -> "D"
    | 15 -> "E"
    | 16 -> "ELSE"
    | 17 -> "ENDIF"
    | 18 -> "F"
    | 19 -> "G"
    | 20 -> "H"
    | 21 -> "IF"
    | 22 -> "RNGLR_EOF"
    | 23 -> "SEMICOLON"
    | 24 -> "THEN"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 11
    | B _ -> 12
    | C _ -> 13
    | D _ -> 14
    | E _ -> 15
    | ELSE _ -> 16
    | ENDIF _ -> 17
    | F _ -> 18
    | G _ -> 19
    | H _ -> 20
    | IF _ -> 21
    | RNGLR_EOF _ -> 22
    | SEMICOLON _ -> 23
    | THEN _ -> 24

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | D _ -> false
    | E _ -> false
    | ELSE _ -> false
    | ENDIF _ -> false
    | F _ -> false
    | G _ -> false
    | H _ -> false
    | IF _ -> false
    | RNGLR_EOF _ -> false
    | SEMICOLON _ -> false
    | THEN _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|4; 10; 7; 7; 5; 5; 2; 8; 8; 9; 9; 6; 6; 3; 0; 0; 0; 0; 0; 0; 0; 0|]
let private rules = [|7; 4; 5; 5; 7; 2; 3; 21; 0; 24; 8; 6; 17; 5; 5; 8; 5; 5; 9; 16; 9; 0; 23; 11; 12; 13; 14; 15; 18; 19; 20|]
let private rulesStart = [|0; 1; 2; 3; 5; 6; 7; 13; 14; 16; 17; 19; 19; 21; 23; 24; 25; 26; 27; 28; 29; 30; 31|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 3; 4; 5; 6; 28; 8; 9; 10; 11; 12; 13; 14; 15; 16; 2; 7; 17; 18; 19; 21; 20; 22; 24; 23; 25; 27; 26|]
let private small_gotos =
        [|15; 0; 131073; 196610; 262147; 327684; 458757; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270; 65537; 1507343; 393230; 0; 131073; 196610; 327684; 458768; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270; 1048585; 17; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1114113; 1572882; 1179662; 0; 131073; 196610; 327699; 524308; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270; 1245198; 0; 131073; 196610; 327699; 524309; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270; 1376258; 393238; 1048599; 1441793; 1114136; 1572878; 0; 131073; 196610; 327705; 589850; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270; 1638414; 0; 131073; 196610; 327705; 589851; 720902; 786439; 851976; 917513; 983050; 1179659; 1245196; 1310733; 1376270|]
let gotos = Array.zeroCreate 29
for i = 0 to 28 do
        gotos.[i] <- Array.zeroCreate 25
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|13,2|]; [|4,1|]; [|5,1|]; [|2,1|]; [|3,2|]; [|14,1|]; [|15,1|]; [|16,1|]; [|17,1|]; [|18,1|]; [|19,1|]; [|20,1|]; [|21,1|]; [|7,1|]; [|8,2|]; [|6,6|]; [|9,1|]; [|10,2|]; [|12,2|]; [|0,1|]|]
let private small_reduces =
        [|131084; 720896; 786432; 851968; 917504; 983040; 1048576; 1114112; 1179648; 1245184; 1310720; 1376256; 1441792; 196620; 720897; 786433; 851969; 917505; 983041; 1048577; 1114113; 1179649; 1245185; 1310721; 1376257; 1441793; 262156; 720898; 786434; 851970; 917506; 983042; 1048578; 1114114; 1179650; 1245186; 1310722; 1376258; 1441794; 393217; 1441795; 458753; 1441796; 524290; 1507333; 1572869; 589826; 1507334; 1572870; 655362; 1507335; 1572871; 720898; 1507336; 1572872; 786434; 1507337; 1572873; 851970; 1507338; 1572874; 917506; 1507339; 1572875; 983042; 1507340; 1572876; 1245186; 1048589; 1114125; 1310722; 1048590; 1114126; 1507340; 720911; 786447; 851983; 917519; 983055; 1048591; 1114127; 1179663; 1245199; 1310735; 1376271; 1441807; 1638401; 1114128; 1703937; 1114129; 1769473; 1114130; 1835009; 1441811|]
let reduces = Array.zeroCreate 29
for i = 0 to 28 do
        reduces.[i] <- Array.zeroCreate 25
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|11|]|]
let private small_zeroReduces =
        [|1376257; 1114112|]
let zeroReduces = Array.zeroCreate 29
for i = 0 to 28 do
        zeroReduces.[i] <- Array.zeroCreate 25
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [5]
let private accStates = Array.zeroCreate 29
for i = 0 to 28 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 22
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


