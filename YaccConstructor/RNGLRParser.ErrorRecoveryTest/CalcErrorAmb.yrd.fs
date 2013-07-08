
# 2 "CalcErrorAmb.yrd.fs"
module RNGLR.ParseCalcErrorAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | ADD of (int)
    | LBRACE of (int)
    | RBRACE of (int)
    | RNGLR_EOF of (int)
    | SEMICOLON of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | ADD x -> box x
    | LBRACE x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMICOLON x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "list"
    | 3 -> "yard_exp_brackets_1"
    | 4 -> "yard_exp_brackets_2"
    | 5 -> "yard_exp_brackets_3"
    | 6 -> "yard_opt_1"
    | 7 -> "yard_start_rule"
    | 8 -> "A"
    | 9 -> "ADD"
    | 10 -> "LBRACE"
    | 11 -> "RBRACE"
    | 12 -> "RNGLR_EOF"
    | 13 -> "SEMICOLON"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 8
    | ADD _ -> 9
    | LBRACE _ -> 10
    | RBRACE _ -> 11
    | RNGLR_EOF _ -> 12
    | SEMICOLON _ -> 13

let isLiteral = function
    | A _ -> false
    | ADD _ -> false
    | LBRACE _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMICOLON _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 7; 6; 6; 1; 1; 1; 3; 3; 4; 5; 5|]
let private rules = [|3; 6; 2; 4; 8; 10; 5; 11; 1; 9; 1; 0; 1; 13; 2; 0; 1|]
let private rulesStart = [|0; 2; 3; 4; 4; 5; 8; 11; 12; 13; 15; 16; 17|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 11; 12; 5; 6; 3; 4; 7; 8; 9; 10; 13; 14; 15; 16|]
let private small_gotos =
        [|6; 0; 65537; 131074; 196611; 524292; 655365; 131073; 589830; 196611; 65543; 524292; 655365; 262145; 589830; 393221; 8; 65545; 327690; 524292; 655365; 524289; 589830; 589825; 720907; 786435; 262156; 393229; 851982; 983046; 0; 65537; 131087; 196611; 524292; 655365|]
let gotos = Array.zeroCreate 17
for i = 0 to 16 do
        gotos.[i] <- Array.zeroCreate 14
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
let private lists_reduces = [|[|7,1|]; [|8,1|]; [|6,3|]; [|4,1|]; [|10,1|]; [|11,1|]; [|5,3|]; [|0,1|]; [|2,1|]; [|0,2|]; [|9,2|]|]
let private small_reduces =
        [|65538; 786432; 851968; 131074; 786433; 851969; 262148; 589826; 720898; 786434; 851970; 327684; 589827; 720899; 786435; 851971; 458753; 720900; 524289; 720901; 655364; 589830; 720902; 786438; 851974; 786433; 786439; 851969; 786440; 917505; 786441; 1048577; 786442|]
let reduces = Array.zeroCreate 17
for i = 0 to 16 do
        reduces.[i] <- Array.zeroCreate 14
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
let private lists_zeroReduces = [|[|3|]|]
let private small_zeroReduces =
        [|786433; 786432|]
let zeroReduces = Array.zeroCreate 17
for i = 0 to 16 do
        zeroReduces.[i] <- Array.zeroCreate 14
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
let private small_acc = [11]
let private accStates = Array.zeroCreate 17
for i = 0 to 16 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 12
let errorIndex = 0
let errorRulesExists = true
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


