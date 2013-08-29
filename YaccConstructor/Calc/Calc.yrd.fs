
# 2 "Calc.yrd.fs"
module Calc.AbstractParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "Calc.yrd"

open AbstractLexer.Core

# 13 "Calc.yrd.fs"
type Token =
    | DIV of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | ERROR of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | LBRACE of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | MINUS of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | MULT of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | NUMBER of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | PLUS of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | POW of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | RBRACE of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | RNGLR_EOF of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | DIV x -> box x
    | ERROR x -> box x
    | LBRACE x -> box x
    | MINUS x -> box x
    | MULT x -> box x
    | NUMBER x -> box x
    | PLUS x -> box x
    | POW x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "factor"
    | 3 -> "factorOp"
    | 4 -> "powExpr"
    | 5 -> "powOp"
    | 6 -> "term"
    | 7 -> "termOp"
    | 8 -> "yard_rule_binExpr_1"
    | 9 -> "yard_rule_binExpr_3"
    | 10 -> "yard_rule_binExpr_5"
    | 11 -> "yard_rule_yard_many_1_2"
    | 12 -> "yard_rule_yard_many_1_4"
    | 13 -> "yard_rule_yard_many_1_6"
    | 14 -> "yard_start_rule"
    | 15 -> "DIV"
    | 16 -> "ERROR"
    | 17 -> "LBRACE"
    | 18 -> "MINUS"
    | 19 -> "MULT"
    | 20 -> "NUMBER"
    | 21 -> "PLUS"
    | 22 -> "POW"
    | 23 -> "RBRACE"
    | 24 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 15
    | ERROR _ -> 16
    | LBRACE _ -> 17
    | MINUS _ -> 18
    | MULT _ -> 19
    | NUMBER _ -> 20
    | PLUS _ -> 21
    | POW _ -> 22
    | RBRACE _ -> 23
    | RNGLR_EOF _ -> 24

let isLiteral = function
    | DIV _ -> false
    | ERROR _ -> false
    | LBRACE _ -> false
    | MINUS _ -> false
    | MULT _ -> false
    | NUMBER _ -> false
    | PLUS _ -> false
    | POW _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 1; 14; 8; 11; 11; 7; 7; 6; 6; 9; 12; 12; 3; 3; 2; 2; 10; 13; 13; 5; 4; 4; 4|]
let private rules = [|8; 16; 1; 6; 11; 7; 6; 11; 21; 18; 9; 16; 2; 12; 3; 2; 12; 19; 15; 10; 16; 4; 13; 5; 4; 13; 22; 20; 17; 1; 23; 16|]
let private rulesStart = [|0; 1; 2; 3; 5; 5; 8; 9; 10; 11; 12; 14; 14; 17; 18; 19; 20; 21; 23; 23; 26; 27; 28; 31; 32|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 22; 31; 28; 19; 32; 14; 17; 3; 21; 6; 7; 4; 20; 5; 9; 18; 12; 10; 13; 11; 15; 16; 23; 30; 26; 27; 24; 29; 25|]
let private small_gotos =
        [|10; 65536; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1114120; 1310729; 131076; 196618; 786443; 983052; 1245197; 196614; 131086; 262146; 655366; 1048591; 1114120; 1310729; 262148; 196618; 786448; 983052; 1245197; 524291; 327697; 851986; 1441811; 589828; 262164; 1048597; 1114120; 1310729; 655363; 327697; 851990; 1441811; 917514; 65559; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1114120; 1310729; 983041; 1507352; 1441796; 458777; 720922; 1179675; 1376284; 1507336; 131073; 262146; 393245; 589829; 655366; 1048606; 1114120; 1310729; 1572868; 458777; 720927; 1179675; 1376284|]
let gotos = Array.zeroCreate 33
for i = 0 to 32 do
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
let private lists_reduces = [|[|10,1|]; [|12,2|]; [|12,3|]; [|14,1|]; [|13,1|]; [|17,1|]; [|19,2|]; [|19,3|]; [|20,1|]; [|23,1|]; [|22,3|]; [|21,1|]; [|17,2|]; [|15,1|]; [|23,1; 16,1|]; [|10,2|]; [|3,1|]; [|5,2|]; [|5,3|]; [|7,1|]; [|6,1|]; [|8,1|]; [|23,1; 16,1; 9,1|]; [|3,2|]; [|0,1|]; [|23,1; 16,1; 9,1; 1,1|]|]
let private small_reduces =
        [|131076; 1179648; 1376256; 1507328; 1572864; 262148; 1179649; 1376257; 1507329; 1572865; 327684; 1179650; 1376258; 1507330; 1572866; 393219; 1048579; 1114115; 1310723; 458755; 1048580; 1114116; 1310724; 524294; 983045; 1179653; 1245189; 1376261; 1507333; 1572869; 655366; 983046; 1179654; 1245190; 1376262; 1507334; 1572870; 720902; 983047; 1179655; 1245191; 1376263; 1507335; 1572871; 786435; 1048584; 1114120; 1310728; 851975; 983049; 1179657; 1245193; 1376265; 1441801; 1507337; 1572873; 1048583; 983050; 1179658; 1245194; 1376266; 1441802; 1507338; 1572874; 1114119; 983051; 1179659; 1245195; 1376267; 1441803; 1507339; 1572875; 1179654; 983052; 1179660; 1245196; 1376268; 1507340; 1572876; 1245190; 983053; 1179661; 1245197; 1376269; 1507341; 1572877; 1310727; 983054; 1179662; 1245198; 1376270; 1441801; 1507342; 1572878; 1376260; 1179663; 1376271; 1507343; 1572879; 1441794; 1507344; 1572880; 1572866; 1507345; 1572881; 1638402; 1507346; 1572882; 1703939; 1048595; 1114131; 1310739; 1769475; 1048596; 1114132; 1310740; 1835012; 1179669; 1376277; 1507349; 1572885; 1900551; 983054; 1179670; 1245198; 1376278; 1441801; 1507350; 1572886; 1966082; 1507351; 1572887; 2031618; 1507352; 1572888; 2097159; 983054; 1179670; 1245198; 1376278; 1441801; 1507353; 1572889|]
let reduces = Array.zeroCreate 33
for i = 0 to 32 do
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
let private lists_zeroReduces = [|[|11|]; [|18|]; [|4|]|]
let private small_zeroReduces =
        [|131076; 1179648; 1376256; 1507328; 1572864; 262148; 1179648; 1376256; 1507328; 1572864; 524294; 983041; 1179649; 1245185; 1376257; 1507329; 1572865; 655366; 983041; 1179649; 1245185; 1376257; 1507329; 1572865; 1441794; 1507330; 1572866; 1572866; 1507330; 1572866|]
let zeroReduces = Array.zeroCreate 33
for i = 0 to 32 do
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 33
for i = 0 to 32 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 24
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


