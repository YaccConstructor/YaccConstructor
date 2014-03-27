
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
    | x -> failwithf "Literal %%s undefined" x
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
    | 8 -> "yard_exp_brackets_4"
    | 9 -> "yard_exp_brackets_5"
    | 10 -> "yard_exp_brackets_6"
    | 11 -> "yard_many_1"
    | 12 -> "yard_many_2"
    | 13 -> "yard_many_3"
    | 14 -> "yard_rule_binExpr_1"
    | 15 -> "yard_rule_binExpr_2"
    | 16 -> "yard_rule_binExpr_3"
    | 17 -> "yard_start_rule"
    | 18 -> "DIV"
    | 19 -> "ERROR"
    | 20 -> "LBRACE"
    | 21 -> "MINUS"
    | 22 -> "MULT"
    | 23 -> "NUMBER"
    | 24 -> "PLUS"
    | 25 -> "POW"
    | 26 -> "RBRACE"
    | 27 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 18
    | ERROR _ -> 19
    | LBRACE _ -> 20
    | MINUS _ -> 21
    | MULT _ -> 22
    | NUMBER _ -> 23
    | PLUS _ -> 24
    | POW _ -> 25
    | RBRACE _ -> 26
    | RNGLR_EOF _ -> 27

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
let leftSide = [|10; 9; 8; 4; 4; 4; 5; 13; 13; 16; 2; 2; 3; 3; 12; 12; 15; 6; 6; 7; 7; 11; 11; 14; 1; 1; 17|]
let private rules = [|5; 4; 3; 2; 7; 6; 23; 20; 1; 26; 19; 25; 10; 13; 4; 13; 16; 19; 22; 18; 9; 12; 2; 12; 15; 19; 24; 21; 8; 11; 6; 11; 14; 19; 1|]
let private rulesStart = [|0; 2; 4; 6; 7; 10; 11; 12; 12; 14; 16; 17; 18; 19; 20; 20; 22; 24; 25; 26; 27; 28; 28; 30; 32; 33; 34; 35|]
let startRule = 26

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 5; 24; 34; 27; 17; 35; 9; 12; 3; 19; 23; 21; 22; 4; 18; 6; 13; 16; 15; 7; 8; 10; 11; 14; 20; 25; 29; 33; 31; 32; 26; 28; 30|]
let private small_gotos =
        [|10; 65536; 131073; 262146; 393219; 917508; 983045; 1048582; 1245191; 1310728; 1507337; 131077; 196618; 589835; 786444; 1179661; 1441806; 196614; 131087; 262146; 1048582; 1245200; 1310728; 1507337; 327684; 327697; 655378; 851987; 1638420; 393220; 262165; 1245206; 1310728; 1507337; 589834; 65559; 131073; 262146; 393219; 917508; 983045; 1048582; 1245191; 1310728; 1507337; 655361; 1703960; 851972; 327697; 655378; 851993; 1638420; 1245189; 196618; 589835; 786458; 1179661; 1441806; 1572869; 458779; 524316; 720925; 1376286; 1572895; 1638408; 131073; 262146; 393248; 983045; 1048582; 1245217; 1310728; 1507337; 1900549; 458779; 524316; 720930; 1376286; 1572895|]
let gotos = Array.zeroCreate 36
for i = 0 to 35 do
        gotos.[i] <- Array.zeroCreate 28
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
let private lists_reduces = [|[|16,1|]; [|1,2|]; [|9,1|]; [|0,2|]; [|5,1|]; [|4,3|]; [|3,1|]; [|8,1|]; [|8,2|]; [|6,1|]; [|9,2|]; [|10,1|]; [|11,1; 5,1|]; [|15,1|]; [|15,2|]; [|13,1|]; [|12,1|]; [|16,2|]; [|23,1|]; [|2,2|]; [|17,1|]; [|18,1; 11,1; 5,1|]; [|22,1|]; [|22,2|]; [|20,1|]; [|19,1|]; [|23,2|]; [|24,1|]; [|25,1; 18,1; 11,1; 5,1|]|]
let private small_reduces =
        [|131076; 1376256; 1572864; 1703936; 1769472; 262150; 1179649; 1376257; 1441793; 1572865; 1703937; 1769473; 327686; 1179650; 1376258; 1441794; 1572866; 1703938; 1769474; 458759; 1179651; 1376259; 1441795; 1572867; 1638403; 1703939; 1769475; 524295; 1179652; 1376260; 1441796; 1572868; 1638404; 1703940; 1769476; 720903; 1179653; 1376261; 1441797; 1572869; 1638405; 1703941; 1769477; 786439; 1179654; 1376262; 1441798; 1572870; 1638406; 1703942; 1769478; 851974; 1179655; 1376263; 1441799; 1572871; 1703943; 1769479; 917510; 1179656; 1376264; 1441800; 1572872; 1703944; 1769480; 983043; 1245193; 1310729; 1507337; 1048582; 1179658; 1376266; 1441802; 1572874; 1703946; 1769482; 1114118; 1179659; 1376267; 1441803; 1572875; 1703947; 1769483; 1179655; 1179660; 1376268; 1441804; 1572876; 1638404; 1703948; 1769484; 1245188; 1376269; 1572877; 1703949; 1769485; 1310724; 1376270; 1572878; 1703950; 1769486; 1376259; 1245199; 1310735; 1507343; 1441795; 1245200; 1310736; 1507344; 1507332; 1376273; 1572881; 1703953; 1769489; 1572866; 1703954; 1769490; 1703940; 1376275; 1572883; 1703955; 1769491; 1769476; 1376276; 1572884; 1703956; 1769492; 1835015; 1179660; 1376277; 1441804; 1572885; 1638404; 1703957; 1769493; 1900546; 1703958; 1769494; 1966082; 1703959; 1769495; 2031619; 1245208; 1310744; 1507352; 2097155; 1245209; 1310745; 1507353; 2162690; 1703962; 1769498; 2228226; 1703963; 1769499; 2293767; 1179660; 1376277; 1441804; 1572885; 1638404; 1703964; 1769500|]
let reduces = Array.zeroCreate 36
for i = 0 to 35 do
        reduces.[i] <- Array.zeroCreate 28
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
let private lists_zeroReduces = [|[|14|]; [|7|]; [|21|]|]
let private small_zeroReduces =
        [|131076; 1376256; 1572864; 1703936; 1769472; 327686; 1179649; 1376257; 1441793; 1572865; 1703937; 1769473; 851974; 1179649; 1376257; 1441793; 1572865; 1703937; 1769473; 1245188; 1376256; 1572864; 1703936; 1769472; 1572866; 1703938; 1769474; 1900546; 1703938; 1769474|]
let zeroReduces = Array.zeroCreate 36
for i = 0 to 35 do
        zeroReduces.[i] <- Array.zeroCreate 28
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
let private accStates = Array.zeroCreate 36
for i = 0 to 35 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 27
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


