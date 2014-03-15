
# 2 "JSONParser.fs"
module JSON.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Highlighting.Core
open JSONHighlighting

# 1 "JSON.yrd"

open AbstractLexer.Core

# 16 "JSONParser.fs"
type Token =
    | EMPTY of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | KW_COLON of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | KW_FALSE of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | KW_NULL of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | KW_TRUE of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | NUMBER of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | RNGLR_EOF of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)
    | STRING1 of (string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)

let genLiteral (str : string) (data : string*array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | EMPTY x -> box x
    | KW_COLON x -> box x
    | KW_FALSE x -> box x
    | KW_NULL x -> box x
    | KW_TRUE x -> box x
    | NUMBER x -> box x
    | RNGLR_EOF x -> box x
    | STRING1 x -> box x

let numToString = function
    | 0 -> "Highlight_EMPTY"
    | 1 -> "Highlight_KW_COLON"
    | 2 -> "Highlight_KW_FALSE"
    | 3 -> "Highlight_KW_NULL"
    | 4 -> "Highlight_KW_TRUE"
    | 5 -> "Highlight_NUMBER"
    | 6 -> "Highlight_STRING1"
    | 7 -> "array1"
    | 8 -> "error"
    | 9 -> "objects"
    | 10 -> "pair"
    | 11 -> "value"
    | 12 -> "yard_rule_list_1"
    | 13 -> "yard_rule_list_3"
    | 14 -> "yard_rule_yard_many_1_2"
    | 15 -> "yard_rule_yard_many_1_4"
    | 16 -> "yard_start_rule"
    | 17 -> "EMPTY"
    | 18 -> "KW_COLON"
    | 19 -> "KW_FALSE"
    | 20 -> "KW_NULL"
    | 21 -> "KW_TRUE"
    | 22 -> "NUMBER"
    | 23 -> "RNGLR_EOF"
    | 24 -> "STRING1"
    | _ -> ""

let tokenToNumber = function
    | EMPTY _ -> 17
    | KW_COLON _ -> 18
    | KW_FALSE _ -> 19
    | KW_NULL _ -> 20
    | KW_TRUE _ -> 21
    | NUMBER _ -> 22
    | RNGLR_EOF _ -> 23
    | STRING1 _ -> 24

let isLiteral = function
    | EMPTY _ -> false
    | KW_COLON _ -> false
    | KW_FALSE _ -> false
    | KW_NULL _ -> false
    | KW_TRUE _ -> false
    | NUMBER _ -> false
    | RNGLR_EOF _ -> false
    | STRING1 _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|11; 11; 11; 11; 11; 11; 11; 16; 7; 12; 12; 14; 14; 10; 9; 13; 13; 15; 15; 6; 5; 4; 2; 3; 0; 1|]
let private rules = [|6; 5; 9; 7; 4; 2; 3; 11; 0; 12; 0; 11; 14; 17; 11; 14; 6; 1; 11; 0; 13; 0; 10; 15; 17; 10; 15; 24; 22; 21; 19; 20; 17; 18|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 11; 11; 13; 13; 16; 19; 22; 22; 24; 24; 27; 28; 29; 30; 31; 32; 33; 34|]
let startRule = 7

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 43; 44; 45; 46; 47; 48; 49; 50; 13; 51; 52; 53; 54; 55; 2; 3; 4; 5; 6; 7; 10; 11; 20; 27; 38; 41; 14; 15; 16; 17; 37; 32; 35; 8; 19; 9; 12; 18; 21; 22; 23; 24; 26; 25; 28; 29; 30; 31; 33; 34; 36; 39; 40; 42|]
let private small_gotos =
        [|15; 0; 131073; 196610; 262147; 327684; 393221; 458758; 589831; 720904; 1114121; 1245194; 1310731; 1376268; 1441805; 1572878; 65554; 15; 131088; 196625; 262162; 327699; 393236; 458773; 589846; 655383; 720920; 786457; 851994; 1114121; 1245211; 1310748; 1376285; 1441822; 1572895; 131090; 15; 131088; 196625; 262162; 327699; 393236; 458773; 589846; 655383; 720920; 786464; 852001; 1114121; 1245211; 1310748; 1376285; 1441822; 1572895; 458754; 65570; 1179683; 524303; 15; 131088; 196625; 262162; 327699; 393252; 458773; 589846; 720933; 1114121; 1245211; 1310748; 1376285; 1441822; 1572902; 1310722; 983079; 1114152; 1441795; 393257; 655402; 1572907; 1507330; 65570; 1179683; 1572866; 983084; 1114152; 1769474; 917549; 1114158; 1900559; 15; 131088; 196625; 262162; 327699; 393252; 458773; 589846; 720943; 1114121; 1245211; 1310748; 1376285; 1441822; 1572902; 1966082; 917552; 1114158; 2097154; 49; 1114162; 2293762; 51; 1114162; 2490370; 52; 1114165; 2686978; 54; 1114165|]
let gotos = Array.zeroCreate 56
for i = 0 to 55 do
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
let private lists_reduces = [|[|5,1|]; [|6,1|]; [|4,1|]; [|1,1|]; [|0,1|]; [|3,1|]; [|2,1|]; [|13,3|]; [|24,1|]; [|22,1|]; [|23,1|]; [|21,1|]; [|20,1|]; [|19,1|]; [|25,1|]; [|16,1|]; [|16,2|]; [|18,2|]; [|18,3|]; [|10,1|]; [|10,2|]; [|12,2|]; [|12,3|]; [|8,3|]; [|14,3|]|]
let private small_reduces =
        [|196609; 1114112; 262145; 1114113; 327681; 1114114; 393217; 1114115; 458753; 1114116; 589825; 1114116; 655361; 1114117; 720897; 1114118; 786433; 1114119; 851974; 1114120; 1245192; 1310728; 1376264; 1441800; 1572872; 917505; 1114121; 983041; 1114122; 1048577; 1114123; 1114113; 1114124; 1179649; 1114125; 1245190; 1114126; 1245198; 1310734; 1376270; 1441806; 1572878; 1310721; 1114127; 1376257; 1114128; 1572865; 1114129; 1638401; 1114130; 1703937; 1179661; 1769473; 1114131; 1835009; 1114132; 1966081; 1114133; 2031617; 1114134; 2162689; 1114135; 2228225; 1114120; 2359297; 1114136; 2424834; 1114125; 1179661; 2555905; 1507351; 2621441; 1507336; 2752513; 1507352; 2818049; 1507328; 2883585; 1507329; 2949121; 1507330; 3014657; 1507331; 3080193; 1507332; 3145729; 1507333; 3211265; 1507334; 3342337; 1507337; 3407873; 1507338; 3473409; 1507339; 3538945; 1507340; 3604481; 1507341|]
let reduces = Array.zeroCreate 56
for i = 0 to 55 do
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
let private lists_zeroReduces = [|[|15; 9|]; [|17|]; [|11|]|]
let private small_zeroReduces =
        [|65537; 1114112; 131073; 1114112; 1310721; 1114113; 1572865; 1114113; 1769473; 1114114; 1966081; 1114114|]
let zeroReduces = Array.zeroCreate 56
for i = 0 to 55 do
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
let private small_acc = [50]
let private accStates = Array.zeroCreate 56
for i = 0 to 55 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 23
let errorIndex = 8
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

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_Highlight_EMPTY * '_rnglr_type_Highlight_KW_COLON * '_rnglr_type_Highlight_KW_FALSE * '_rnglr_type_Highlight_KW_NULL * '_rnglr_type_Highlight_KW_TRUE * '_rnglr_type_Highlight_NUMBER * '_rnglr_type_Highlight_STRING1 * '_rnglr_type_array1 * '_rnglr_type_error * '_rnglr_type_objects * '_rnglr_type_pair * '_rnglr_type_value * '_rnglr_type_yard_rule_list_1 * '_rnglr_type_yard_rule_list_3 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_STRING1) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 204 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_NUMBER) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 227 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_objects) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 250 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_array1) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 273 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_KW_TRUE) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 296 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_KW_FALSE) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 319 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_KW_NULL) 
             |> List.iter (fun (H0) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H0]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 342 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_value) 
            )
# 23 "JSON.yrd"
               : '_rnglr_type_yard_start_rule) 
# 352 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_EMPTY) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_list_1) 
               |> List.iter (fun (H1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_Highlight_EMPTY) 
                 |> List.iter (fun (H2) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Array1NonTermNode("array1")
                    let children : IAbstractTreeNode list = [H0; H1; H2]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 25 "JSON.yrd"
               : '_rnglr_type_array1) 
# 379 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              let parent = new Yard_rule_list_1NonTermNode("yard_rule_list_1")
              let children : IAbstractTreeNode list = []
              addSemantic parent children
              
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_1) 
# 400 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_value) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2)
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_list_1NonTermNode("yard_rule_list_1")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_1) 
# 425 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
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
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 446 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              (match ((unbox _rnglr_children.[0]) : Token) with EMPTY _rnglr_val -> [_rnglr_val] | a -> failwith "EMPTY expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_value) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 25 "JSON.yrd"
                                            S2
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
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 482 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_STRING1) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_Highlight_KW_COLON) 
               |> List.iter (fun (H1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_value) 
                 |> List.iter (fun (H2) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new PairNonTermNode("pair")
                    let children : IAbstractTreeNode list = [H0; H1; H2]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 27 "JSON.yrd"
               : '_rnglr_type_pair) 
# 509 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_Highlight_EMPTY) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_list_3) 
               |> List.iter (fun (H1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_Highlight_EMPTY) 
                 |> List.iter (fun (H2) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new ObjectsNonTermNode("objects")
                    let children : IAbstractTreeNode list = [H0; H1; H2]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 29 "JSON.yrd"
               : '_rnglr_type_objects) 
# 536 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              let parent = new Yard_rule_list_3NonTermNode("yard_rule_list_3")
              let children : IAbstractTreeNode list = []
              addSemantic parent children
              
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_3) 
# 557 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_pair) 
             |> List.iter (fun (H0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4)
               |> List.iter (fun (H1) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_list_3NonTermNode("yard_rule_list_3")
                  let children : IAbstractTreeNode list = [H0; H1]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_3) 
# 582 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
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
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 603 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              (match ((unbox _rnglr_children.[0]) : Token) with EMPTY _rnglr_val -> [_rnglr_val] | a -> failwith "EMPTY expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_pair) 
                 |> List.iter (fun (S2) -> 
                  _rnglr_cycle_res := (
                    
# 25 "JSON.yrd"
                                            S2
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
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 639 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRING1 _rnglr_val -> [_rnglr_val] | a -> failwith "STRING1 expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new STRING1TermNode("STRING1")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_STRING1) 
# 660 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
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
# 681 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with KW_TRUE _rnglr_val -> [_rnglr_val] | a -> failwith "KW_TRUE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new KW_TRUETermNode("KW_TRUE")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_KW_TRUE) 
# 702 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with KW_FALSE _rnglr_val -> [_rnglr_val] | a -> failwith "KW_FALSE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new KW_FALSETermNode("KW_FALSE")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_KW_FALSE) 
# 723 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with KW_NULL _rnglr_val -> [_rnglr_val] | a -> failwith "KW_NULL expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new KW_NULLTermNode("KW_NULL")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_KW_NULL) 
# 744 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EMPTY _rnglr_val -> [_rnglr_val] | a -> failwith "EMPTY expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new EMPTYTermNode("EMPTY")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_EMPTY) 
# 765 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with KW_COLON _rnglr_val -> [_rnglr_val] | a -> failwith "KW_COLON expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                new KW_COLONTermNode("KW_COLON")
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_Highlight_KW_COLON) 
# 786 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
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
# 804 "JSONParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_EMPTY)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_KW_COLON)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_KW_FALSE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_KW_NULL)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_KW_TRUE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_NUMBER)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_Highlight_STRING1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_array1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_objects)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_pair)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_value)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_list_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_list_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_4)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
