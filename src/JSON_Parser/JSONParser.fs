
# 2 "JSONParser.fs"
module JSON.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open JetBrains.ReSharper.Psi.Tree
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
    | 0 -> "array1"
    | 1 -> "error"
    | 2 -> "highlight_EMPTY"
    | 3 -> "highlight_KW_COLON"
    | 4 -> "highlight_KW_FALSE"
    | 5 -> "highlight_KW_NULL"
    | 6 -> "highlight_KW_TRUE"
    | 7 -> "highlight_NUMBER"
    | 8 -> "highlight_STRING1"
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
let leftSide = [|11; 11; 11; 11; 11; 11; 11; 16; 0; 12; 12; 14; 14; 10; 9; 13; 13; 15; 15; 8; 7; 6; 4; 5; 2; 3|]
let private rules = [|8; 7; 9; 0; 6; 4; 5; 11; 2; 12; 2; 11; 14; 2; 11; 14; 8; 3; 11; 2; 13; 2; 10; 15; 2; 10; 15; 24; 22; 21; 19; 20; 17; 18|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 11; 11; 13; 13; 16; 19; 22; 22; 24; 24; 27; 28; 29; 30; 31; 32; 33; 34|]
let startRule = 7

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 45; 46; 47; 48; 49; 50; 51; 14; 52; 53; 54; 55; 56; 3; 4; 5; 6; 7; 8; 9; 12; 21; 29; 40; 43; 15; 16; 17; 18; 39; 34; 37; 10; 20; 11; 13; 19; 22; 28; 26; 23; 24; 27; 25; 30; 33; 31; 32; 35; 36; 38; 41; 42; 44|]
let private small_gotos =
        [|15; 0; 131073; 262146; 327683; 393220; 458757; 524294; 589831; 720904; 1114121; 1245194; 1310731; 1376268; 1441805; 1572878; 131090; 15; 131088; 262161; 327698; 393235; 458772; 524309; 589846; 655383; 720920; 786457; 851994; 1114121; 1245211; 1310748; 1376285; 1441822; 1572895; 262162; 15; 131088; 262161; 327698; 393235; 458772; 524309; 589846; 655383; 720920; 786464; 852001; 1114121; 1245211; 1310748; 1376285; 1441822; 1572895; 589826; 196642; 1179683; 655375; 15; 131088; 262161; 327698; 393235; 458772; 524324; 589846; 720933; 1114121; 1245211; 1310748; 1376285; 1441822; 1572902; 1376259; 131111; 983080; 1114153; 1441795; 524330; 655403; 1572908; 1507330; 196642; 1179683; 1572867; 131111; 983085; 1114153; 1900547; 131118; 917551; 1114121; 1966095; 15; 131088; 262161; 327698; 393235; 458772; 524324; 589846; 720944; 1114121; 1245211; 1310748; 1376285; 1441822; 1572902; 2031619; 131118; 917553; 1114121; 2228226; 131122; 1114163; 2424834; 131124; 1114163; 2621442; 131125; 1114166; 2818050; 131127; 1114166|]
let gotos = Array.zeroCreate 57
for i = 0 to 56 do
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
let private lists_reduces = [|[|3,1|]; [|5,1|]; [|6,1|]; [|4,1|]; [|1,1|]; [|0,1|]; [|2,1|]; [|13,3|]; [|24,1|]; [|22,1|]; [|23,1|]; [|21,1|]; [|20,1|]; [|19,1|]; [|25,1|]; [|16,1|]; [|18,2|]; [|18,3|]; [|16,2|]; [|10,1|]; [|12,2|]; [|12,3|]; [|10,2|]; [|8,3|]; [|14,3|]|]
let private small_reduces =
        [|65537; 1507328; 196609; 1114112; 327681; 1114113; 393217; 1114114; 458753; 1114115; 524289; 1114116; 589825; 1114117; 720897; 1114117; 786433; 1114118; 851969; 1114119; 917510; 1114120; 1245192; 1310728; 1376264; 1441800; 1572872; 983041; 1114121; 1048577; 1114122; 1114113; 1114123; 1179649; 1114124; 1245185; 1114125; 1310726; 1114126; 1245198; 1310734; 1376270; 1441806; 1572878; 1376257; 1114127; 1572865; 1114128; 1638401; 1114129; 1703937; 1572872; 1769473; 1179661; 1835009; 1114130; 1900545; 1114131; 2031617; 1114132; 2097153; 1114133; 2162689; 1114134; 2293761; 1114135; 2359297; 1114120; 2490369; 1114136; 2555906; 1114125; 1179661; 2686977; 1507351; 2752513; 1507336; 2883585; 1507352; 2949121; 1507329; 3014657; 1507330; 3080193; 1507331; 3145729; 1507332; 3211265; 1507333; 3276801; 1507334; 3407873; 1507337; 3473409; 1507338; 3538945; 1507339; 3604481; 1507340; 3670017; 1507341|]
let reduces = Array.zeroCreate 57
for i = 0 to 56 do
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
        [|131073; 1114112; 262145; 1114112; 1376257; 1114113; 1572865; 1114113; 1900545; 1114114; 2031617; 1114114|]
let zeroReduces = Array.zeroCreate 57
for i = 0 to 56 do
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
let private small_acc = [51]
let private accStates = Array.zeroCreate 57
for i = 0 to 56 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 23
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let xmlPath = "JSONHighlighting.xml" 

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
            curr.SetPrevSibling(prev)
    parent.SetLastChild(curr)
    parent

let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
    let ranges = 
        brs |> Seq.groupBy (fun x -> x.back_ref)
        |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
        |> Seq.map(fun brs ->
            try
                let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                let lengthTok = pos.Length
                let beginPosTok = pos.[0] + 1
                let endPosTok = pos.[lengthTok-1] + 2
                let endPos = 
                    brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok
                    - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset
                brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
            with
            | e -> brs.[0].back_ref.GetDocumentRange())
    ranges

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_array1 * '_rnglr_type_error * '_rnglr_type_highlight_EMPTY * '_rnglr_type_highlight_KW_COLON * '_rnglr_type_highlight_KW_FALSE * '_rnglr_type_highlight_KW_NULL * '_rnglr_type_highlight_KW_TRUE * '_rnglr_type_highlight_NUMBER * '_rnglr_type_highlight_STRING1 * '_rnglr_type_objects * '_rnglr_type_pair * '_rnglr_type_value * '_rnglr_type_yard_rule_list_1 * '_rnglr_type_yard_rule_list_3 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_STRING1) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 224 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_NUMBER) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 247 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_objects) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 270 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_array1) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 293 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_KW_TRUE) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 316 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_KW_FALSE) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 339 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_KW_NULL) 
             |> List.iter (fun (H1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ValueNonTermNode("value")
                let children : IAbstractTreeNode list = [H1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "JSON.yrd"
               : '_rnglr_type_value) 
# 362 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_value) 
            )
# 23 "JSON.yrd"
               : '_rnglr_type_yard_start_rule) 
# 372 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_EMPTY) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_list_1) 
               |> List.iter (fun (H2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_highlight_EMPTY) 
                 |> List.iter (fun (H3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Array1NonTermNode("array1")
                    let children : IAbstractTreeNode list = [H1; H2; H3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 25 "JSON.yrd"
               : '_rnglr_type_array1) 
# 399 "JSONParser.fs"
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
# 420 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_value) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2) 
               |> List.iter (fun (H2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_list_1NonTermNode("yard_rule_list_1")
                  let children : IAbstractTreeNode list = [H1; H2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_1) 
# 445 "JSONParser.fs"
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
# 466 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_EMPTY) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_value) 
               |> List.iter (fun (H2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_2) 
                 |> List.iter (fun (H3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Yard_rule_yard_many_1_2NonTermNode("yard_rule_yard_many_1_2")
                    let children : IAbstractTreeNode list = [H1; H2; H3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 493 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_STRING1) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_highlight_KW_COLON) 
               |> List.iter (fun (H2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_value) 
                 |> List.iter (fun (H3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new PairNonTermNode("pair")
                    let children : IAbstractTreeNode list = [H1; H2; H3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 27 "JSON.yrd"
               : '_rnglr_type_pair) 
# 520 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_EMPTY) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_list_3) 
               |> List.iter (fun (H2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_highlight_EMPTY) 
                 |> List.iter (fun (H3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new ObjectsNonTermNode("objects")
                    let children : IAbstractTreeNode list = [H1; H2; H3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 29 "JSON.yrd"
               : '_rnglr_type_objects) 
# 547 "JSONParser.fs"
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
# 568 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_pair) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4) 
               |> List.iter (fun (H2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_list_3NonTermNode("yard_rule_list_3")
                  let children : IAbstractTreeNode list = [H1; H2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Common.yrd"
               : '_rnglr_type_yard_rule_list_3) 
# 593 "JSONParser.fs"
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
# 614 "JSONParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> * array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_EMPTY) 
             |> List.iter (fun (H1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_pair) 
               |> List.iter (fun (H2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_4) 
                 |> List.iter (fun (H3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Yard_rule_yard_many_1_4NonTermNode("yard_rule_yard_many_1_4")
                    let children : IAbstractTreeNode list = [H1; H2; H3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "Common.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 641 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new STRING1TermNode("STRING1", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_STRING1) 
# 664 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new NUMBERTermNode("NUMBER", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_NUMBER) 
# 687 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new KW_TRUETermNode("KW_TRUE", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_KW_TRUE) 
# 710 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new KW_FALSETermNode("KW_FALSE", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_KW_FALSE) 
# 733 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new KW_NULLTermNode("KW_NULL", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_KW_NULL) 
# 756 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new EMPTYTermNode("EMPTY", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_EMPTY) 
# 779 "JSONParser.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new KW_COLONTermNode("KW_COLON", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_KW_COLON) 
# 802 "JSONParser.fs"
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
# 820 "JSONParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_array1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_EMPTY)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_KW_COLON)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_KW_FALSE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_KW_NULL)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_KW_TRUE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_NUMBER)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_STRING1)   ) |> List.concat));
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
