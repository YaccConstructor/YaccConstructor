
# 2 "ExtendedCalc.yrd.fs"
module RNGLR.ParseExtendedCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.ARNGLR.Parser
open AbstractAnalysis.Common
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
type Token =
    | DIV of (int)
    | EIGHT of (int)
    | EQ of (int)
    | ERROR of (int)
    | FIVE of (int)
    | FOUR of (int)
    | LBRACE of (int)
    | MINUS of (int)
    | MULT of (int)
    | NINE of (int)
    | ONE of (int)
    | PLUS of (int)
    | POW of (int)
    | RBRACE of (int)
    | RNGLR_EOF of (int)
    | SEMICOLON of (int)
    | SEVEN of (int)
    | SIX of (int)
    | THREE of (int)
    | TWO of (int)
    | X of (int)
    | Y of (int)
    | Z of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | DIV x -> box x
    | EIGHT x -> box x
    | EQ x -> box x
    | ERROR x -> box x
    | FIVE x -> box x
    | FOUR x -> box x
    | LBRACE x -> box x
    | MINUS x -> box x
    | MULT x -> box x
    | NINE x -> box x
    | ONE x -> box x
    | PLUS x -> box x
    | POW x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMICOLON x -> box x
    | SEVEN x -> box x
    | SIX x -> box x
    | THREE x -> box x
    | TWO x -> box x
    | X x -> box x
    | Y x -> box x
    | Z x -> box x

let numToString = function
    | 0 -> "assign"
    | 1 -> "calcExpr"
    | 2 -> "error"
    | 3 -> "expr"
    | 4 -> "factor"
    | 5 -> "factorOp"
    | 6 -> "id"
    | 7 -> "number"
    | 8 -> "powExpr"
    | 9 -> "powOp"
    | 10 -> "start"
    | 11 -> "term"
    | 12 -> "termOp"
    | 13 -> "yard_rule_binExpr_1"
    | 14 -> "yard_rule_binExpr_3"
    | 15 -> "yard_rule_binExpr_5"
    | 16 -> "yard_rule_yard_many_1_2"
    | 17 -> "yard_rule_yard_many_1_4"
    | 18 -> "yard_rule_yard_many_1_6"
    | 19 -> "yard_some_1"
    | 20 -> "yard_start_rule"
    | 21 -> "DIV"
    | 22 -> "EIGHT"
    | 23 -> "EQ"
    | 24 -> "ERROR"
    | 25 -> "FIVE"
    | 26 -> "FOUR"
    | 27 -> "LBRACE"
    | 28 -> "MINUS"
    | 29 -> "MULT"
    | 30 -> "NINE"
    | 31 -> "ONE"
    | 32 -> "PLUS"
    | 33 -> "POW"
    | 34 -> "RBRACE"
    | 35 -> "RNGLR_EOF"
    | 36 -> "SEMICOLON"
    | 37 -> "SEVEN"
    | 38 -> "SIX"
    | 39 -> "THREE"
    | 40 -> "TWO"
    | 41 -> "X"
    | 42 -> "Y"
    | 43 -> "Z"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 21
    | EIGHT _ -> 22
    | EQ _ -> 23
    | ERROR _ -> 24
    | FIVE _ -> 25
    | FOUR _ -> 26
    | LBRACE _ -> 27
    | MINUS _ -> 28
    | MULT _ -> 29
    | NINE _ -> 30
    | ONE _ -> 31
    | PLUS _ -> 32
    | POW _ -> 33
    | RBRACE _ -> 34
    | RNGLR_EOF _ -> 35
    | SEMICOLON _ -> 36
    | SEVEN _ -> 37
    | SIX _ -> 38
    | THREE _ -> 39
    | TWO _ -> 40
    | X _ -> 41
    | Y _ -> 42
    | Z _ -> 43

let isLiteral = function
    | DIV _ -> false
    | EIGHT _ -> false
    | EQ _ -> false
    | ERROR _ -> false
    | FIVE _ -> false
    | FOUR _ -> false
    | LBRACE _ -> false
    | MINUS _ -> false
    | MULT _ -> false
    | NINE _ -> false
    | ONE _ -> false
    | PLUS _ -> false
    | POW _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMICOLON _ -> false
    | SEVEN _ -> false
    | SIX _ -> false
    | THREE _ -> false
    | TWO _ -> false
    | X _ -> false
    | Y _ -> false
    | Z _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|10; 20; 19; 19; 0; 1; 3; 3; 13; 16; 16; 12; 12; 11; 11; 14; 17; 17; 5; 5; 4; 4; 15; 18; 18; 9; 8; 8; 8; 8; 6; 6; 6; 7; 7; 7; 7; 7; 7; 7; 7; 7|]
let private rules = [|19; 10; 0; 0; 19; 6; 23; 3; 36; 3; 13; 24; 11; 16; 12; 11; 16; 32; 28; 14; 24; 4; 17; 5; 4; 17; 29; 21; 15; 24; 8; 18; 9; 8; 18; 33; 7; 6; 27; 3; 34; 24; 41; 42; 43; 31; 40; 39; 26; 25; 38; 37; 22; 30|]
let private rulesStart = [|0; 1; 2; 3; 5; 9; 10; 11; 12; 14; 14; 17; 18; 19; 20; 21; 23; 23; 26; 27; 28; 29; 30; 32; 32; 35; 36; 37; 38; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber (Some tokenData) leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 51; 52; 32; 33; 34; 50; 3; 4; 6; 12; 13; 14; 39; 48; 45; 36; 19; 49; 21; 22; 23; 26; 27; 28; 29; 30; 31; 5; 7; 38; 10; 11; 8; 37; 9; 15; 35; 18; 16; 20; 17; 24; 25; 40; 47; 43; 44; 41; 46; 42|]
let private small_gotos =
        [|7; 0; 393217; 655362; 1245187; 2686980; 2752517; 2818054; 65542; 0; 393217; 1245191; 2686980; 2752517; 2818054; 131073; 1507336; 196631; 196617; 262154; 393227; 458764; 524301; 720910; 851983; 917520; 983057; 1441810; 1572883; 1638420; 1703957; 1769494; 1966103; 2031640; 2424857; 2490394; 2555931; 2621468; 2686980; 2752517; 2818054; 262145; 2359325; 393220; 327710; 1114143; 1376288; 1900577; 458771; 262178; 393227; 458764; 524301; 983057; 1441810; 1572899; 1638420; 1703957; 1769494; 1966103; 2031640; 2424857; 2490394; 2555931; 2621468; 2686980; 2752517; 2818054; 524292; 327710; 1114148; 1376288; 1900577; 917507; 589861; 1179686; 2162727; 983057; 393227; 458764; 524328; 1441810; 1572905; 1638420; 1703957; 1769494; 1966103; 2031640; 2424857; 2490394; 2555931; 2621468; 2686980; 2752517; 2818054; 1048579; 589861; 1179690; 2162727; 1507351; 196651; 262154; 393227; 458764; 524301; 720910; 851983; 917520; 983057; 1441810; 1572883; 1638420; 1703957; 1769494; 1966103; 2031640; 2424857; 2490394; 2555931; 2621468; 2686980; 2752517; 2818054; 1572865; 2228268; 2555908; 786477; 1048622; 1835055; 2097200; 2621461; 262154; 393227; 458764; 524301; 720945; 917520; 983057; 1441810; 1572914; 1638420; 1703957; 1769494; 1966103; 2031640; 2424857; 2490394; 2555931; 2621468; 2686980; 2752517; 2818054; 2686980; 786477; 1048627; 1835055; 2097200|]
let gotos = Array.zeroCreate 53
for i = 0 to 52 do
        gotos.[i] <- Array.zeroCreate 44
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
let private lists_reduces = [|[|2,1|]; [|4,4|]; [|15,1|]; [|17,2|]; [|17,3|]; [|19,1|]; [|18,1|]; [|27,1|]; [|26,1|]; [|22,1|]; [|24,2|]; [|24,3|]; [|25,1|]; [|40,1|]; [|29,1|]; [|37,1|]; [|36,1|]; [|28,3|]; [|41,1|]; [|33,1|]; [|39,1|]; [|38,1|]; [|35,1|]; [|34,1|]; [|30,1|]; [|31,1|]; [|32,1|]; [|22,2|]; [|20,1|]; [|29,1; 21,1|]; [|15,2|]; [|8,1|]; [|10,2|]; [|10,3|]; [|12,1|]; [|11,1|]; [|13,1|]; [|29,1; 21,1; 14,1|]; [|8,2|]; [|6,1|]; [|29,1; 21,1; 14,1; 7,1|]; [|3,2|]; [|0,1|]|]
let private small_reduces =
        [|65537; 2293760; 327684; 2293761; 2686977; 2752513; 2818049; 393220; 1835010; 2097154; 2228226; 2359298; 524292; 1835011; 2097155; 2228227; 2359299; 589828; 1835012; 2097156; 2228228; 2359300; 655374; 1441797; 1572869; 1638405; 1703941; 1769477; 1966085; 2031621; 2424837; 2490373; 2555909; 2621445; 2686981; 2752517; 2818053; 720910; 1441798; 1572870; 1638406; 1703942; 1769478; 1966086; 2031622; 2424838; 2490374; 2555910; 2621446; 2686982; 2752518; 2818054; 786439; 1376263; 1835015; 1900551; 2097159; 2162695; 2228231; 2359303; 851975; 1376264; 1835016; 1900552; 2097160; 2162696; 2228232; 2359304; 917510; 1376265; 1835017; 1900553; 2097161; 2228233; 2359305; 1048582; 1376266; 1835018; 1900554; 2097162; 2228234; 2359306; 1114118; 1376267; 1835019; 1900555; 2097163; 2228235; 2359307; 1179662; 1441804; 1572876; 1638412; 1703948; 1769484; 1966092; 2031628; 2424844; 2490380; 2555916; 2621452; 2686988; 2752524; 2818060; 1245191; 1376269; 1835021; 1900557; 2097165; 2162701; 2228237; 2359309; 1310727; 1376270; 1835022; 1900558; 2097166; 2162702; 2228238; 2359310; 1376263; 1376271; 1835023; 1900559; 2097167; 2162703; 2228239; 2359311; 1441799; 1376272; 1835024; 1900560; 2097168; 2162704; 2228240; 2359312; 1638407; 1376273; 1835025; 1900561; 2097169; 2162705; 2228241; 2359313; 1703943; 1376274; 1835026; 1900562; 2097170; 2162706; 2228242; 2359314; 1769479; 1376275; 1835027; 1900563; 2097171; 2162707; 2228243; 2359315; 1835015; 1376276; 1835028; 1900564; 2097172; 2162708; 2228244; 2359316; 1900551; 1376277; 1835029; 1900565; 2097173; 2162709; 2228245; 2359317; 1966087; 1376278; 1835030; 1900566; 2097174; 2162710; 2228246; 2359318; 2031623; 1376279; 1835031; 1900567; 2097175; 2162711; 2228247; 2359319; 2097160; 1376280; 1507352; 1835032; 1900568; 2097176; 2162712; 2228248; 2359320; 2162696; 1376281; 1507353; 1835033; 1900569; 2097177; 2162713; 2228249; 2359321; 2228232; 1376282; 1507354; 1835034; 1900570; 2097178; 2162714; 2228250; 2359322; 2293766; 1376283; 1835035; 1900571; 2097179; 2228251; 2359323; 2359302; 1376284; 1835036; 1900572; 2097180; 2228252; 2359324; 2424839; 1376285; 1835037; 1900573; 2097181; 2162702; 2228253; 2359325; 2490372; 1835038; 2097182; 2228254; 2359326; 2555906; 2228255; 2359327; 2686978; 2228256; 2359328; 2752514; 2228257; 2359329; 2818062; 1441826; 1572898; 1638434; 1703970; 1769506; 1966114; 2031650; 2424866; 2490402; 2555938; 2621474; 2687010; 2752546; 2818082; 2883598; 1441827; 1572899; 1638435; 1703971; 1769507; 1966115; 2031651; 2424867; 2490403; 2555939; 2621475; 2687011; 2752547; 2818083; 2949124; 1835044; 2097188; 2228260; 2359332; 3014663; 1376285; 1835045; 1900573; 2097189; 2162702; 2228261; 2359333; 3080194; 2228262; 2359334; 3145730; 2228263; 2359335; 3211271; 1376285; 1835045; 1900573; 2097189; 2162702; 2228264; 2359336; 3276801; 2293801; 3407873; 2293802|]
let reduces = Array.zeroCreate 53
for i = 0 to 52 do
        reduces.[i] <- Array.zeroCreate 44
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
let private lists_zeroReduces = [|[|16|]; [|23|]; [|9|]|]
let private small_zeroReduces =
        [|393220; 1835008; 2097152; 2228224; 2359296; 524292; 1835008; 2097152; 2228224; 2359296; 917510; 1376257; 1835009; 1900545; 2097153; 2228225; 2359297; 1048582; 1376257; 1835009; 1900545; 2097153; 2228225; 2359297; 2555906; 2228226; 2359298; 2686978; 2228226; 2359298|]
let zeroReduces = Array.zeroCreate 53
for i = 0 to 52 do
        zeroReduces.[i] <- Array.zeroCreate 44
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
let private small_acc = [51]
let private accStates = Array.zeroCreate 53
for i = 0 to 52 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 35
let errorIndex = 2
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists, tokenData)
let buildAstAbstract : (ParserInputGraph<'TokenType> -> Yard.Generators.ARNGLR.Parser.ParseResult<Token>) = 
    buildAstAbstract<Token> parserSource


