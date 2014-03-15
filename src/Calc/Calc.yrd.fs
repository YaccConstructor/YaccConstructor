
# 2 "Calc.yrd.fs"
module Calc.AbstractParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Highlighting.Core
open CalcHighlighting

# 1 "calc.yrd"

open AbstractLexer.Core

# 15 "Calc.yrd.fs"
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

let genLiteral (str : string) (data : string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>) =
    match str.ToLower() with
    | x -> None
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
    | 0 -> "Highlight_DIV"
    | 1 -> "Highlight_ERROR"
    | 2 -> "Highlight_LBRACE"
    | 3 -> "Highlight_MINUS"
    | 4 -> "Highlight_MULT"
    | 5 -> "Highlight_NUMBER"
    | 6 -> "Highlight_PLUS"
    | 7 -> "Highlight_POW"
    | 8 -> "Highlight_RBRACE"
    | 9 -> "error"
    | 10 -> "expr"
    | 11 -> "factor"
    | 12 -> "factorOp"
    | 13 -> "powExpr"
    | 14 -> "powOp"
    | 15 -> "term"
    | 16 -> "termOp"
    | 17 -> "yard_rule_binExpr_1"
    | 18 -> "yard_rule_binExpr_3"
    | 19 -> "yard_rule_binExpr_5"
    | 20 -> "yard_rule_yard_many_1_2"
    | 21 -> "yard_rule_yard_many_1_4"
    | 22 -> "yard_rule_yard_many_1_6"
    | 23 -> "yard_start_rule"
    | 24 -> "DIV"
    | 25 -> "ERROR"
    | 26 -> "LBRACE"
    | 27 -> "MINUS"
    | 28 -> "MULT"
    | 29 -> "NUMBER"
    | 30 -> "PLUS"
    | 31 -> "POW"
    | 32 -> "RBRACE"
    | 33 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 24
    | ERROR _ -> 25
    | LBRACE _ -> 26
    | MINUS _ -> 27
    | MULT _ -> 28
    | NUMBER _ -> 29
    | PLUS _ -> 30
    | POW _ -> 31
    | RBRACE _ -> 32
    | RNGLR_EOF _ -> 33

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
let leftSide = [|10; 10; 23; 17; 20; 20; 16; 16; 15; 15; 18; 21; 21; 12; 12; 11; 11; 19; 22; 22; 14; 13; 13; 13; 1; 6; 3; 4; 0; 7; 5; 2; 8|]
let private rules = [|17; 1; 10; 15; 20; 16; 15; 20; 6; 3; 18; 1; 11; 21; 12; 11; 21; 4; 0; 19; 1; 13; 22; 14; 13; 22; 7; 5; 2; 10; 8; 1; 25; 30; 27; 28; 24; 31; 29; 26; 32|]
let private rulesStart = [|0; 1; 2; 3; 5; 5; 8; 9; 10; 11; 12; 14; 14; 17; 18; 19; 20; 21; 23; 23; 26; 27; 28; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 46; 47; 48; 53; 63; 70; 68; 61; 58; 26; 59; 3; 4; 5; 43; 9; 18; 31; 42; 40; 29; 25; 27; 6; 7; 8; 10; 11; 12; 30; 16; 17; 13; 14; 15; 19; 20; 28; 24; 21; 22; 23; 32; 33; 34; 41; 38; 39; 35; 36; 37; 44; 45; 49; 62; 50; 51; 52; 54; 60; 55; 56; 57; 64; 69; 65; 66; 67|]
let private small_gotos =
        [|13; 65536; 131073; 327682; 655363; 720900; 851973; 983046; 1114119; 1179656; 1245193; 1638410; 1703947; 1900556; 131085; 65549; 131086; 327695; 655376; 720913; 851986; 983059; 1114132; 1179669; 1245206; 1638423; 1703947; 1900568; 262157; 65549; 131086; 327695; 655385; 720913; 851986; 983059; 1114132; 1179669; 1245206; 1638423; 1703947; 1900568; 393218; 524314; 2097179; 589830; 28; 262173; 786462; 1376287; 1572896; 1835041; 786441; 65570; 131086; 327695; 720931; 851986; 1245206; 1638423; 1703947; 1900568; 917510; 28; 262173; 786462; 1376292; 1572896; 1835041; 1179652; 458789; 917542; 1441831; 2031656; 1310727; 65577; 131086; 327695; 852010; 1638423; 1703947; 1900568; 1441796; 458789; 917542; 1441835; 2031656; 2031622; 196652; 393261; 1048622; 1310767; 1769520; 1966129; 2228235; 65586; 131086; 327695; 720913; 851986; 983091; 1179669; 1245206; 1638423; 1703947; 1900568; 2359302; 196652; 393261; 1048622; 1310772; 1769520; 1966129; 2818050; 524341; 2097206; 3145734; 28; 262173; 786487; 1376312; 1572896; 1835041; 3211273; 65593; 131073; 327682; 720954; 851973; 1245193; 1638410; 1703947; 1900556; 3342342; 28; 262173; 786487; 1376315; 1572896; 1835041; 3473412; 458789; 917564; 1441853; 2031656; 3538951; 65598; 131073; 327682; 852031; 1638410; 1703947; 1900556; 3670020; 458789; 917564; 1441856; 2031656; 4128774; 196652; 393261; 1048641; 1310786; 1769520; 1966129; 4194315; 65603; 131073; 327682; 720900; 851973; 983108; 1179656; 1245193; 1638410; 1703947; 1900556; 4325382; 196652; 393261; 1048641; 1310789; 1769520; 1966129|]
let gotos = Array.zeroCreate 71
for i = 0 to 70 do
        gotos.[i] <- Array.zeroCreate 34
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
let private lists_reduces = [|[|23,1; 16,1|]; [|23,1; 16,1; 9,1|]; [|23,1|]; [|23,1; 16,1; 9,1; 1,1|]; [|21,1|]; [|22,3|]; [|32,1|]; [|10,1|]; [|14,1|]; [|13,1|]; [|12,2|]; [|12,3|]; [|28,1|]; [|27,1|]; [|17,1|]; [|20,1|]; [|19,2|]; [|19,3|]; [|29,1|]; [|24,1|]; [|31,1|]; [|30,1|]; [|17,2|]; [|15,1|]; [|10,2|]; [|3,1|]; [|7,1|]; [|6,1|]; [|5,2|]; [|5,3|]; [|26,1|]; [|25,1|]; [|8,1|]; [|3,2|]; [|0,1|]|]
let private small_reduces =
        [|65542; 1572864; 1769473; 1835008; 1966081; 2031618; 2162691; 196614; 1572864; 1769473; 1835008; 1966081; 2031618; 2097155; 327686; 1572868; 1769476; 1835012; 1966084; 2031620; 2097156; 458758; 1572869; 1769477; 1835013; 1966085; 2031621; 2097157; 524294; 1572870; 1769478; 1835014; 1966086; 2031622; 2097158; 589827; 1769479; 1966087; 2097159; 655363; 1638408; 1703944; 1900552; 720899; 1638409; 1703945; 1900553; 851974; 1572864; 1769472; 1835008; 1966080; 2031618; 2097152; 917507; 1769482; 1966090; 2097162; 983043; 1769483; 1966091; 2097163; 1048579; 1638412; 1703948; 1900556; 1114115; 1638413; 1703949; 1900557; 1179653; 1572878; 1769486; 1835022; 1966094; 2097166; 1245187; 1638415; 1703951; 1900559; 1376262; 1572866; 1769474; 1835010; 1966082; 2031618; 2097154; 1441797; 1572880; 1769488; 1835024; 1966096; 2097168; 1507333; 1572881; 1769489; 1835025; 1966097; 2097169; 1572867; 1638418; 1703954; 1900562; 1638406; 1572883; 1769491; 1835027; 1966099; 2031635; 2097171; 1703939; 1638420; 1703956; 1900564; 1769478; 1572885; 1769493; 1835029; 1966101; 2031637; 2097173; 1835013; 1572886; 1769494; 1835030; 1966102; 2097174; 1900549; 1572887; 1769495; 1835031; 1966103; 2097175; 1966083; 1769496; 1966104; 2097176; 2031617; 2097177; 2097155; 1638426; 1703962; 1900570; 2162691; 1638427; 1703963; 1900571; 2293766; 1572864; 1769473; 1835008; 1966081; 2031618; 2097153; 2359297; 2097180; 2424833; 2097181; 2490371; 1638430; 1703966; 1900574; 2555907; 1638431; 1703967; 1900575; 2621443; 1769504; 1966112; 2097184; 2686977; 2097185; 2752513; 2097186; 2883590; 1572869; 1769477; 1835013; 1966085; 2031621; 2162693; 2949126; 1572870; 1769478; 1835014; 1966086; 2031622; 2162694; 3014662; 1572868; 1769476; 1835012; 1966084; 2031620; 2162692; 3145731; 1769479; 1966087; 2162695; 3276806; 1572864; 1769472; 1835008; 1966080; 2031618; 2162688; 3342339; 1769482; 1966090; 2162698; 3407875; 1769483; 1966091; 2162699; 3473413; 1572878; 1769486; 1835022; 1966094; 2162702; 3604486; 1572866; 1769474; 1835010; 1966082; 2031618; 2162690; 3670021; 1572880; 1769488; 1835024; 1966096; 2162704; 3735557; 1572881; 1769489; 1835025; 1966097; 2162705; 3801094; 1572883; 1769491; 1835027; 1966099; 2031635; 2162707; 3866630; 1572885; 1769493; 1835029; 1966101; 2031637; 2162709; 3932165; 1572886; 1769494; 1835030; 1966102; 2162710; 3997701; 1572887; 1769495; 1835031; 1966103; 2162711; 4063235; 1769496; 1966104; 2162712; 4128769; 2162713; 4259846; 1572864; 1769473; 1835008; 1966081; 2031618; 2162689; 4325377; 2162716; 4390913; 2162717; 4456451; 1769504; 1966112; 2162720; 4521985; 2162721; 4587521; 2162722|]
let reduces = Array.zeroCreate 71
for i = 0 to 70 do
        reduces.[i] <- Array.zeroCreate 34
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
        [|589827; 1769472; 1966080; 2097152; 917507; 1769472; 1966080; 2097152; 1179653; 1572865; 1769473; 1835009; 1966081; 2097153; 1441797; 1572865; 1769473; 1835009; 1966081; 2097153; 2031617; 2097154; 2359297; 2097154; 3145731; 1769472; 1966080; 2162688; 3342339; 1769472; 1966080; 2162688; 3473413; 1572865; 1769473; 1835009; 1966081; 2162689; 3670021; 1572865; 1769473; 1835009; 1966081; 2162689; 4128769; 2162690; 4325377; 2162690|]
let zeroReduces = Array.zeroCreate 71
for i = 0 to 70 do
        zeroReduces.[i] <- Array.zeroCreate 34
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
let private small_acc = [47]
let private accStates = Array.zeroCreate 71
for i = 0 to 70 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 33
let errorIndex = 9
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let addSemantic (parent : IAbstractTreeNode) (children : IAbstractTreeNode list) = 
    let mutable prev = null
    let mutable curr = null
    for child in children do
        prev <- curr
        curr <- child
        curr.SetParent(parent)
        if prev = null
        then parent.SetFirstChild(curr)
        else
            prev.SetNextSibling(curr)
            curr.SetNextSibling(prev)
    parent.SetLastChild(curr)
    parent

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_Highlight_DIV * '_rnglr_type_Highlight_ERROR * '_rnglr_type_Highlight_LBRACE * '_rnglr_type_Highlight_MINUS * '_rnglr_type_Highlight_MULT * '_rnglr_type_Highlight_NUMBER * '_rnglr_type_Highlight_PLUS * '_rnglr_type_Highlight_POW * '_rnglr_type_Highlight_RBRACE * '_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_rule_binExpr_5 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_rule_yard_many_1_6 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_1) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ExprNonTermNode("expr")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "calc.yrd"
               : '_rnglr_type_expr) 
# 220 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_ERROR) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ExprNonTermNode("expr")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "calc.yrd"
               : '_rnglr_type_expr) 
# 243 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 22 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 253 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_1NonTermNode("yard_rule_binExpr_1")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_1) 
# 278 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              let parent = new Yard_rule_yard_many_1_2NonTermNode("yard_rule_yard_many_1_2")
              let children : IAbstractTreeNode list = []
              addSemantic parent children
              
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 299 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_termOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_term) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 22 "calc.yrd"
                                       new AbstractTreeNode("I NEED THINK ABOUT IT")
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_2) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_yard_many_1_2NonTermNode("yard_rule_yard_many_1_2")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 335 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_PLUS) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermOpNonTermNode("termOp")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_termOp) 
# 358 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_MINUS) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermOpNonTermNode("termOp")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_termOp) 
# 381 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_3) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermNonTermNode("term")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_term) 
# 404 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_ERROR) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermNonTermNode("term")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_term) 
# 427 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_3NonTermNode("yard_rule_binExpr_3")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_3) 
# 452 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              let parent = new Yard_rule_yard_many_1_4NonTermNode("yard_rule_yard_many_1_4")
              let children : IAbstractTreeNode list = []
              addSemantic parent children
              
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 473 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_factorOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_factor) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 27 "calc.yrd"
                                         new AbstractTreeNode("I NEED THINK ABOUT IT")
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_4) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_yard_many_1_4NonTermNode("yard_rule_yard_many_1_4")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 509 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_MULT) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorOpNonTermNode("factorOp")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 532 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_DIV) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorOpNonTermNode("factorOp")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 555 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_5) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorNonTermNode("factor")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_factor) 
# 578 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_ERROR) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorNonTermNode("factor")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_factor) 
# 601 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_6) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_5NonTermNode("yard_rule_binExpr_5")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_5) 
# 626 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              let parent = new Yard_rule_yard_many_1_6NonTermNode("yard_rule_yard_many_1_6")
              let children : IAbstractTreeNode list = []
              addSemantic parent children
              
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 647 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_powOp) 
               |> List.iter (fun (S1) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_powExpr) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 31 "calc.yrd"
                                            new AbstractTreeNode("I NEED THINK ABOUT IT")
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_6) 
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_yard_many_1_6NonTermNode("yard_rule_yard_many_1_6")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 683 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_POW) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowOpNonTermNode("powOp")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 34 "calc.yrd"
               : '_rnglr_type_powOp) 
# 706 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_NUMBER) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowExprNonTermNode("powExpr")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 729 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_LBRACE) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_expr) 
               |> List.iter (fun (H1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_Highlight_RBRACE) 
                 |> List.iter (fun (H2) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new PowExprNonTermNode("powExpr")
                    let children : IAbstractTreeNode list = [H0; H1; H2]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 756 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_ERROR) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowExprNonTermNode("powExpr")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 779 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ERROR _rnglr_val -> [_rnglr_val] | a -> failwith "ERROR expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new ERRORTermNode("ERROR")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_ERROR) 
# 800 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new PLUSTermNode("PLUS")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_PLUS) 
# 821 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwith "MINUS expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new MINUSTermNode("MINUS")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_MINUS) 
# 842 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MULT _rnglr_val -> [_rnglr_val] | a -> failwith "MULT expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new MULTTermNode("MULT")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_MULT) 
# 863 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIV _rnglr_val -> [_rnglr_val] | a -> failwith "DIV expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new DIVTermNode("DIV")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_DIV) 
# 884 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with POW _rnglr_val -> [_rnglr_val] | a -> failwith "POW expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new POWTermNode("POW")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_POW) 
# 905 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwith "NUMBER expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new NUMBERTermNode("NUMBER")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_NUMBER) 
# 926 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new LBRACETermNode("LBRACE")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_LBRACE) 
# 947 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new RBRACETermNode("RBRACE")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_RBRACE) 
# 968 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 986 "Calc.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_DIV)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_ERROR)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_LBRACE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_MINUS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_MULT)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_NUMBER)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_PLUS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_POW)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_RBRACE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factor)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factorOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_term)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_termOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_5)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_4)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_6)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
