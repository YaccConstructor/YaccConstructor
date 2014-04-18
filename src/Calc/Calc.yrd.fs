
# 2 "Calc.yrd.fs"
module Calc.AbstractParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open JetBrains.ReSharper.Psi.Tree
open Highlighting.Core
open CalcHighlighting

# 1 "calc.yrd"

open AbstractLexer.Core

# 16 "Calc.yrd.fs"
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
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "factor"
    | 3 -> "factorOp"
    | 4 -> "highlight_DIV"
    | 5 -> "highlight_ERROR"
    | 6 -> "highlight_LBRACE"
    | 7 -> "highlight_MINUS"
    | 8 -> "highlight_MULT"
    | 9 -> "highlight_NUMBER"
    | 10 -> "highlight_PLUS"
    | 11 -> "highlight_POW"
    | 12 -> "highlight_RBRACE"
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
let leftSide = [|1; 1; 23; 17; 20; 20; 16; 16; 15; 15; 18; 21; 21; 3; 3; 2; 2; 19; 22; 22; 14; 13; 13; 13; 5; 10; 7; 8; 4; 11; 9; 6; 12|]
let private rules = [|17; 5; 1; 15; 20; 16; 15; 20; 10; 7; 18; 5; 2; 21; 3; 2; 21; 8; 4; 19; 5; 13; 22; 14; 13; 22; 11; 9; 6; 1; 12; 5; 25; 30; 27; 28; 24; 31; 29; 26; 32|]
let private rulesStart = [|0; 1; 2; 3; 5; 5; 8; 9; 10; 11; 12; 14; 14; 17; 18; 19; 20; 21; 23; 23; 26; 27; 28; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 29; 11; 15; 16; 30; 41; 39; 27; 23; 24; 25; 3; 5; 6; 28; 8; 9; 4; 10; 7; 12; 13; 14; 17; 18; 26; 22; 19; 20; 21; 31; 32; 33; 40; 37; 38; 34; 35; 36|]
let private small_gotos =
        [|13; 65536; 131073; 327682; 393219; 589828; 851973; 983046; 1114119; 1179656; 1245193; 1638410; 1703947; 1900556; 131078; 196621; 262158; 524303; 1376272; 1572881; 1835026; 196617; 131091; 327700; 393219; 589828; 851973; 1245193; 1638410; 1703947; 1900556; 262150; 196621; 262158; 524303; 1376277; 1572881; 1835026; 720909; 65558; 131073; 327682; 393219; 589828; 851973; 983046; 1114119; 1179656; 1245193; 1638410; 1703947; 1900556; 786434; 786455; 2097176; 1048580; 720921; 917530; 1441819; 2031644; 1179655; 327709; 393219; 589828; 851998; 1638410; 1703947; 1900556; 1310724; 720921; 917530; 1441823; 2031644; 1966086; 458784; 655393; 1048610; 1310755; 1769508; 1966117; 2162699; 131073; 327718; 393219; 589828; 851973; 983079; 1179656; 1245193; 1638410; 1703947; 1900556; 2293766; 458784; 655393; 1048610; 1310760; 1769508; 1966117|]
let gotos = Array.zeroCreate 42
for i = 0 to 41 do
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
let private lists_reduces = [|[|10,1|]; [|12,2|]; [|14,1|]; [|13,1|]; [|12,3|]; [|28,1|]; [|27,1|]; [|23,1; 16,1|]; [|23,1|]; [|22,3|]; [|32,1|]; [|21,1|]; [|17,1|]; [|20,1|]; [|19,2|]; [|19,3|]; [|29,1|]; [|24,1|]; [|31,1|]; [|30,1|]; [|17,2|]; [|15,1|]; [|10,2|]; [|23,1; 16,1; 9,1|]; [|23,1; 16,1; 9,1; 1,1|]; [|3,1|]; [|7,1|]; [|6,1|]; [|5,2|]; [|5,3|]; [|26,1|]; [|25,1|]; [|8,1|]; [|3,2|]; [|0,1|]|]
let private small_reduces =
        [|131076; 1769472; 1966080; 2097152; 2162688; 262148; 1769473; 1966081; 2097153; 2162689; 327683; 1638402; 1703938; 1900546; 393219; 1638403; 1703939; 1900547; 458756; 1769476; 1966084; 2097156; 2162692; 524291; 1638405; 1703941; 1900549; 589827; 1638406; 1703942; 1900550; 655367; 1572871; 1769479; 1835015; 1966087; 2031624; 2097159; 2162695; 851975; 1572873; 1769481; 1835017; 1966089; 2031625; 2097161; 2162697; 917511; 1572874; 1769482; 1835018; 1966090; 2031626; 2097162; 2162698; 983047; 1572875; 1769483; 1835019; 1966091; 2031627; 2097163; 2162699; 1048582; 1572876; 1769484; 1835020; 1966092; 2097164; 2162700; 1114115; 1638413; 1703949; 1900557; 1245191; 1572872; 1769480; 1835016; 1966088; 2031624; 2097160; 2162696; 1310726; 1572878; 1769486; 1835022; 1966094; 2097166; 2162702; 1376262; 1572879; 1769487; 1835023; 1966095; 2097167; 2162703; 1441795; 1638416; 1703952; 1900560; 1507335; 1572881; 1769489; 1835025; 1966097; 2031633; 2097169; 2162705; 1572867; 1638418; 1703954; 1900562; 1638407; 1572883; 1769491; 1835027; 1966099; 2031635; 2097171; 2162707; 1703942; 1572884; 1769492; 1835028; 1966100; 2097172; 2162708; 1769478; 1572885; 1769493; 1835029; 1966101; 2097173; 2162709; 1835012; 1769494; 1966102; 2097174; 2162710; 1900551; 1572871; 1769495; 1835015; 1966103; 2031624; 2097176; 2162712; 1966082; 2097177; 2162713; 2031619; 1638426; 1703962; 1900570; 2097155; 1638427; 1703963; 1900571; 2228231; 1572871; 1769495; 1835015; 1966103; 2031624; 2097175; 2162711; 2293762; 2097180; 2162716; 2359298; 2097181; 2162717; 2424835; 1638430; 1703966; 1900574; 2490371; 1638431; 1703967; 1900575; 2555908; 1769504; 1966112; 2097184; 2162720; 2621442; 2097185; 2162721; 2686978; 2097186; 2162722|]
let reduces = Array.zeroCreate 42
for i = 0 to 41 do
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
        [|131076; 1769472; 1966080; 2097152; 2162688; 262148; 1769472; 1966080; 2097152; 2162688; 1048582; 1572865; 1769473; 1835009; 1966081; 2097153; 2162689; 1310726; 1572865; 1769473; 1835009; 1966081; 2097153; 2162689; 1966082; 2097154; 2162690; 2293762; 2097154; 2162690|]
let zeroReduces = Array.zeroCreate 42
for i = 0 to 41 do
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 42
for i = 0 to 41 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 33
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let xmlPath = "CalcHighlighting.xml" 

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

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_highlight_DIV * '_rnglr_type_highlight_ERROR * '_rnglr_type_highlight_LBRACE * '_rnglr_type_highlight_MINUS * '_rnglr_type_highlight_MULT * '_rnglr_type_highlight_NUMBER * '_rnglr_type_highlight_PLUS * '_rnglr_type_highlight_POW * '_rnglr_type_highlight_RBRACE * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_rule_binExpr_5 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_rule_yard_many_1_6 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_1) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ExprNonTermNode("expr")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "calc.yrd"
               : '_rnglr_type_expr) 
# 241 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_ERROR) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ExprNonTermNode("expr")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "calc.yrd"
               : '_rnglr_type_expr) 
# 264 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 22 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 274 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2) 
               |> List.iter (fun (h2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_1NonTermNode("yard_rule_binExpr_1")
                  let children : IAbstractTreeNode list = [h1; h2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_1) 
# 299 "Calc.yrd.fs"
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
# 320 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_termOp) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_term) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_2) 
                 |> List.iter (fun (h3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Yard_rule_yard_many_1_2NonTermNode("yard_rule_yard_many_1_2")
                    let children : IAbstractTreeNode list = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 347 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_PLUS) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermOpNonTermNode("termOp")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_termOp) 
# 370 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_MINUS) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermOpNonTermNode("termOp")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_termOp) 
# 393 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_3) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermNonTermNode("term")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_term) 
# 416 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_ERROR) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new TermNonTermNode("term")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_term) 
# 439 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4) 
               |> List.iter (fun (h2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_3NonTermNode("yard_rule_binExpr_3")
                  let children : IAbstractTreeNode list = [h1; h2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_3) 
# 464 "Calc.yrd.fs"
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
# 485 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factorOp) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_factor) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_4) 
                 |> List.iter (fun (h3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Yard_rule_yard_many_1_4NonTermNode("yard_rule_yard_many_1_4")
                    let children : IAbstractTreeNode list = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 512 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_MULT) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorOpNonTermNode("factorOp")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 535 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_DIV) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorOpNonTermNode("factorOp")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 558 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_5) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorNonTermNode("factor")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_factor) 
# 581 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_ERROR) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new FactorNonTermNode("factor")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_factor) 
# 604 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_6) 
               |> List.iter (fun (h2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_rule_binExpr_5NonTermNode("yard_rule_binExpr_5")
                  let children : IAbstractTreeNode list = [h1; h2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_5) 
# 629 "Calc.yrd.fs"
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
# 650 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powOp) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_powExpr) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_6) 
                 |> List.iter (fun (h3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new Yard_rule_yard_many_1_6NonTermNode("yard_rule_yard_many_1_6")
                    let children : IAbstractTreeNode list = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 677 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_POW) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowOpNonTermNode("powOp")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 34 "calc.yrd"
               : '_rnglr_type_powOp) 
# 700 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_NUMBER) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowExprNonTermNode("powExpr")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 723 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_LBRACE) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_expr) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_highlight_RBRACE) 
                 |> List.iter (fun (h3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new PowExprNonTermNode("powExpr")
                    let children : IAbstractTreeNode list = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 750 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_ERROR) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new PowExprNonTermNode("powExpr")
                let children : IAbstractTreeNode list = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 36 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 773 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new ERRORTermNode("ERROR", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_ERROR) 
# 796 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new PLUSTermNode("PLUS", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_PLUS) 
# 819 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new MINUSTermNode("MINUS", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_MINUS) 
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new MULTTermNode("MULT", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_MULT) 
# 865 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new DIVTermNode("DIV", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_DIV) 
# 888 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new POWTermNode("POW", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_POW) 
# 911 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new NUMBERTermNode("NUMBER", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_NUMBER) 
# 934 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new LBRACETermNode("LBRACE", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_LBRACE) 
# 957 "Calc.yrd.fs"
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
                

                let temp : array<Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> = snd <| _rnglr_var_0
                let pos = calculatePos temp
                new RBRACETermNode("RBRACE", pos)
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_RBRACE) 
# 980 "Calc.yrd.fs"
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
# 998 "Calc.yrd.fs"
      );
  |] , [|
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_DIV)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_ERROR)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_LBRACE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_MINUS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_MULT)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_NUMBER)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_PLUS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_POW)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_RBRACE)   ) |> List.concat));
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
