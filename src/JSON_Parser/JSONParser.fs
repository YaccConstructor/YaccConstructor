
# 2 "JSONParser.fs"
module JSON.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "JSON.yrd"

open AbstractLexer.Core

# 13 "JSONParser.fs"
type Token =
    | NUMBER of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | RNGLR_EOF of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | STRING1 of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_comma_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_semi_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_left_square_bracket_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_right_square_bracket_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_false of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_null of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_true of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_left_figure_bracket_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | L_right_figure_bracket_ of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)

let genLiteral (str : string) (data : string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>) =
    match str.ToLower() with
    | "," -> Some (L_comma_ data)
    | ":" -> Some (L_semi_ data)
    | "[" -> Some (L_left_square_bracket_ data)
    | "]" -> Some (L_right_square_bracket_ data)
    | "false" -> Some (L_false data)
    | "null" -> Some (L_null data)
    | "true" -> Some (L_true data)
    | "{" -> Some (L_left_figure_bracket_ data)
    | "}" -> Some (L_right_figure_bracket_ data)
    | x -> None
let tokenData = function
    | NUMBER x -> box x
    | RNGLR_EOF x -> box x
    | STRING1 x -> box x
    | L_comma_ x -> box x
    | L_semi_ x -> box x
    | L_left_square_bracket_ x -> box x
    | L_right_square_bracket_ x -> box x
    | L_false x -> box x
    | L_null x -> box x
    | L_true x -> box x
    | L_left_figure_bracket_ x -> box x
    | L_right_figure_bracket_ x -> box x

let numToString = function
    | 0 -> "array1"
    | 1 -> "error"
    | 2 -> "objects"
    | 3 -> "pair"
    | 4 -> "value"
    | 5 -> "yard_exp_brackets_1603"
    | 6 -> "yard_exp_brackets_1604"
    | 7 -> "yard_many_177"
    | 8 -> "yard_many_178"
    | 9 -> "yard_rule_list_1601"
    | 10 -> "yard_rule_list_1602"
    | 11 -> "yard_start_rule"
    | 12 -> "NUMBER"
    | 13 -> "RNGLR_EOF"
    | 14 -> "STRING1"
    | 15 -> "','"
    | 16 -> "':'"
    | 17 -> "'['"
    | 18 -> "']'"
    | 19 -> "'false'"
    | 20 -> "'null'"
    | 21 -> "'true'"
    | 22 -> "'{'"
    | 23 -> "'}'"
    | _ -> ""

let tokenToNumber = function
    | NUMBER _ -> 12
    | RNGLR_EOF _ -> 13
    | STRING1 _ -> 14
    | L_comma_ _ -> 15
    | L_semi_ _ -> 16
    | L_left_square_bracket_ _ -> 17
    | L_right_square_bracket_ _ -> 18
    | L_false _ -> 19
    | L_null _ -> 20
    | L_true _ -> 21
    | L_left_figure_bracket_ _ -> 22
    | L_right_figure_bracket_ _ -> 23

let isLiteral = function
    | NUMBER _ -> false
    | RNGLR_EOF _ -> false
    | STRING1 _ -> false
    | L_comma_ _ -> true
    | L_semi_ _ -> true
    | L_left_square_bracket_ _ -> true
    | L_right_square_bracket_ _ -> true
    | L_false _ -> true
    | L_null _ -> true
    | L_true _ -> true
    | L_left_figure_bracket_ _ -> true
    | L_right_figure_bracket_ _ -> true

let getLiteralNames = [",";":";"[";"]";"false";"null";"true";"{";"}";]
let mutable private cur = 0
let leftSide = [|6; 5; 8; 8; 10; 10; 2; 3; 7; 7; 9; 9; 0; 4; 4; 4; 4; 4; 4; 4; 11|]
let private rules = [|15; 3; 15; 4; 6; 8; 3; 8; 22; 10; 23; 14; 16; 4; 5; 7; 4; 7; 17; 9; 18; 14; 12; 2; 0; 21; 19; 20; 4|]
let private rulesStart = [|0; 2; 4; 4; 6; 6; 8; 11; 14; 14; 16; 16; 18; 21; 22; 23; 24; 25; 26; 27; 28; 29|]
let startRule = 20

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 6; 14; 15; 16; 17; 7; 12; 8; 29; 10; 9; 11; 13; 18; 26; 23; 19; 28; 21; 20; 22; 24; 25; 27|]
let private small_gotos =
        [|10; 0; 131073; 262146; 786435; 917508; 1114117; 1245190; 1310727; 1376264; 1441801; 393227; 0; 131073; 262154; 589835; 786435; 917508; 1114117; 1245190; 1310727; 1376264; 1441801; 458755; 327692; 458765; 983054; 524291; 327692; 458767; 983054; 655370; 0; 131073; 262160; 786435; 917508; 1114117; 1245190; 1310727; 1376264; 1441801; 786433; 1179665; 1114115; 196626; 655379; 917524; 1179651; 393237; 524310; 983063; 1245187; 393237; 524312; 983063; 1376258; 196633; 917524; 1507329; 1048602; 1572874; 0; 131073; 262171; 786435; 917508; 1114117; 1245190; 1310727; 1376264; 1441801; 1703937; 1507356|]
let gotos = Array.zeroCreate 30
for i = 0 to 29 do
        gotos.[i] <- Array.zeroCreate 24
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
let private lists_reduces = [|[|16,1|]; [|15,1|]; [|14,1|]; [|13,1|]; [|11,1|]; [|9,1|]; [|9,2|]; [|1,2|]; [|12,3|]; [|18,1|]; [|19,1|]; [|17,1|]; [|5,1|]; [|3,1|]; [|3,2|]; [|0,2|]; [|7,3|]; [|6,3|]; [|5,2|]; [|11,2|]|]
let private small_reduces =
        [|65540; 851968; 983040; 1179648; 1507328; 131076; 851969; 983041; 1179649; 1507329; 262148; 851970; 983042; 1179650; 1507330; 327684; 851971; 983043; 1179651; 1507331; 458753; 1179652; 524289; 1179653; 589825; 1179654; 720898; 983047; 1179655; 851972; 851976; 983048; 1179656; 1507336; 917508; 851977; 983049; 1179657; 1507337; 983044; 851978; 983050; 1179658; 1507338; 1048580; 851979; 983051; 1179659; 1507339; 1179649; 1507340; 1245185; 1507341; 1310721; 1507342; 1441794; 983055; 1507343; 1638402; 983056; 1507344; 1769476; 851985; 983057; 1179665; 1507345; 1835009; 1507346; 1900545; 1179667|]
let reduces = Array.zeroCreate 30
for i = 0 to 29 do
        reduces.[i] <- Array.zeroCreate 24
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
let private lists_zeroReduces = [|[|10|]; [|8|]; [|4|]; [|2|]|]
let private small_zeroReduces =
        [|393217; 1179648; 458753; 1179649; 524289; 1179649; 1114113; 1507330; 1179649; 1507331; 1245185; 1507331|]
let zeroReduces = Array.zeroCreate 30
for i = 0 to 29 do
        zeroReduces.[i] <- Array.zeroCreate 24
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
let private small_acc = [3]
let private accStates = Array.zeroCreate 30
for i = 0 to 29 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 13
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


