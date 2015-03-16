
# 2 "ExtCalc.yrd.fs"
module ExtCalc.AbstractParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open YC.SDK.ReSharper.Helper
open JetBrains.ReSharper.Psi.Tree
open ExtCalcHighlighting

# 1 "extCalc.yrd"

open YC.FST.AbstractLexing.Interpreter

# 16 "ExtCalc.yrd.fs"
type Token =
    | DIV of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | EQ of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | ERROR of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | LBRACE of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | MINUS of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | MUL of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | NUMBER of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | PLUS of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | RBRACE of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | RNGLR_EOF of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | SEMI of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)
    | VARIABLE of (GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>)

let genLiteral (str : string) (data : GraphTokenValue<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | DIV x -> box x
    | EQ x -> box x
    | ERROR x -> box x
    | LBRACE x -> box x
    | MINUS x -> box x
    | MUL x -> box x
    | NUMBER x -> box x
    | PLUS x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | VARIABLE x -> box x

let numToString = function
    | 0 -> "assign"
    | 1 -> "error"
    | 2 -> "expr"
    | 3 -> "highlight_DIV"
    | 4 -> "highlight_EQ"
    | 5 -> "highlight_ERROR"
    | 6 -> "highlight_LBRACE"
    | 7 -> "highlight_MINUS"
    | 8 -> "highlight_MUL"
    | 9 -> "highlight_NUMBER"
    | 10 -> "highlight_PLUS"
    | 11 -> "highlight_RBRACE"
    | 12 -> "highlight_SEMI"
    | 13 -> "highlight_VARIABLE"
    | 14 -> "start"
    | 15 -> "yard_exp_brackets_1"
    | 16 -> "yard_some_1"
    | 17 -> "yard_start_rule"
    | 18 -> "DIV"
    | 19 -> "EQ"
    | 20 -> "ERROR"
    | 21 -> "LBRACE"
    | 22 -> "MINUS"
    | 23 -> "MUL"
    | 24 -> "NUMBER"
    | 25 -> "PLUS"
    | 26 -> "RBRACE"
    | 27 -> "RNGLR_EOF"
    | 28 -> "SEMI"
    | 29 -> "VARIABLE"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 18
    | EQ _ -> 19
    | ERROR _ -> 20
    | LBRACE _ -> 21
    | MINUS _ -> 22
    | MUL _ -> 23
    | NUMBER _ -> 24
    | PLUS _ -> 25
    | RBRACE _ -> 26
    | RNGLR_EOF _ -> 27
    | SEMI _ -> 28
    | VARIABLE _ -> 29

let isLiteral = function
    | DIV _ -> false
    | EQ _ -> false
    | ERROR _ -> false
    | LBRACE _ -> false
    | MINUS _ -> false
    | MUL _ -> false
    | NUMBER _ -> false
    | PLUS _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | VARIABLE _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|14; 17; 16; 16; 0; 2; 2; 2; 2; 2; 15; 15; 15; 15; 13; 4; 12; 9; 6; 11; 5; 10; 7; 3; 8|]
let private rules = [|16; 14; 0; 0; 16; 13; 4; 2; 12; 2; 15; 2; 9; 13; 6; 2; 11; 5; 10; 7; 3; 8; 29; 19; 28; 24; 21; 26; 20; 25; 22; 18; 23|]
let private rulesStart = [|0; 1; 2; 3; 5; 9; 12; 13; 14; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 30; 31; 25; 29; 3; 28; 4; 16; 17; 21; 22; 23; 26; 24; 5; 6; 7; 8; 9; 10; 12; 13; 14; 15; 27; 11; 18; 19; 20|]
let private small_gotos =
        [|5; 0; 851969; 917506; 1048579; 1900548; 65540; 0; 851969; 1048581; 1900548; 131074; 262150; 1245191; 196617; 131080; 327689; 393226; 589835; 851980; 1310733; 1376270; 1572879; 1900548; 262155; 196624; 458769; 524306; 655379; 786452; 983061; 1179670; 1441815; 1507352; 1638425; 1835034; 655369; 131099; 327689; 393226; 589835; 851980; 1310733; 1376270; 1572879; 1900548; 720905; 196624; 458769; 524306; 655379; 983061; 1179670; 1441815; 1507352; 1638425; 1114121; 131100; 327689; 393226; 589835; 851980; 1310733; 1376270; 1572879; 1900548; 1179659; 196624; 458769; 524306; 655379; 720925; 983061; 1179670; 1441815; 1507352; 1638425; 1703966|]
let gotos = Array.zeroCreate 32
for i = 0 to 31 do
        gotos.[i] <- Array.zeroCreate 30
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
let private lists_reduces = [|[|2,1|]; [|12,1|]; [|11,1|]; [|13,1|]; [|10,1|]; [|4,4|]; [|5,3|]; [|23,1|]; [|22,1|]; [|24,1|]; [|21,1|]; [|9,1|]; [|8,3|]; [|19,1|]; [|6,1|]; [|7,1|]; [|20,1|]; [|17,1|]; [|14,1|]; [|18,1|]; [|16,1|]; [|15,1|]; [|3,2|]; [|0,1|]|]
let private small_reduces =
        [|65537; 1769472; 327684; 1310721; 1376257; 1572865; 1900545; 393220; 1310722; 1376258; 1572866; 1900546; 458756; 1310723; 1376259; 1572867; 1900547; 524292; 1310724; 1376260; 1572868; 1900548; 589826; 1769477; 1900549; 720902; 1179654; 1441798; 1507334; 1638406; 1703942; 1835014; 786436; 1310727; 1376263; 1572871; 1900551; 851972; 1310728; 1376264; 1572872; 1900552; 917508; 1310729; 1376265; 1572873; 1900553; 983044; 1310730; 1376266; 1572874; 1900554; 1048582; 1179659; 1441803; 1507339; 1638411; 1703947; 1835019; 1245190; 1179660; 1441804; 1507340; 1638412; 1703948; 1835020; 1310726; 1179661; 1441805; 1507341; 1638413; 1703949; 1835021; 1376262; 1179662; 1441806; 1507342; 1638414; 1703950; 1835022; 1441798; 1179663; 1441807; 1507343; 1638415; 1703951; 1835023; 1507334; 1179664; 1441808; 1507344; 1638416; 1703952; 1835024; 1572870; 1179665; 1441809; 1507345; 1638417; 1703953; 1835025; 1638407; 1179666; 1245202; 1441810; 1507346; 1638418; 1703954; 1835026; 1703940; 1310739; 1376275; 1572883; 1900563; 1769474; 1769492; 1900564; 1835012; 1310741; 1376277; 1572885; 1900565; 1900545; 1769494; 2031617; 1769495|]
let reduces = Array.zeroCreate 32
for i = 0 to 31 do
        reduces.[i] <- Array.zeroCreate 30
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 32
for i = 0 to 31 do
        zeroReduces.[i] <- Array.zeroCreate 30
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
let private small_acc = [30]
let private accStates = Array.zeroCreate 32
for i = 0 to 31 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 27
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let xmlPath = "ExtCalcHighlighting.xml" 

let tokenToTreeNode token = 
    match token with
    | DIV data -> 
        let ranges = calculatePos data
        new DIVTermNode(ranges) :> ITreeNode
    | EQ data -> 
        let ranges = calculatePos data
        new EQTermNode(ranges) :> ITreeNode
    | ERROR data -> 
        let ranges = calculatePos data
        new ERRORTermNode(ranges) :> ITreeNode
    | LBRACE data -> 
        let ranges = calculatePos data
        new LBRACETermNode(ranges) :> ITreeNode
    | MINUS data -> 
        let ranges = calculatePos data
        new MINUSTermNode(ranges) :> ITreeNode
    | MUL data -> 
        let ranges = calculatePos data
        new MULTermNode(ranges) :> ITreeNode
    | NUMBER data -> 
        let ranges = calculatePos data
        new NUMBERTermNode(ranges) :> ITreeNode
    | PLUS data -> 
        let ranges = calculatePos data
        new PLUSTermNode(ranges) :> ITreeNode
    | RBRACE data -> 
        let ranges = calculatePos data
        new RBRACETermNode(ranges) :> ITreeNode
    | RNGLR_EOF data -> 
        let ranges = calculatePos data
        new RNGLR_EOFTermNode(ranges) :> ITreeNode
    | SEMI data -> 
        let ranges = calculatePos data
        new SEMITermNode(ranges) :> ITreeNode
    | VARIABLE data -> 
        let ranges = calculatePos data
        new VARIABLETermNode(ranges) :> ITreeNode


let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; null; null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_assign * '_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_highlight_DIV * '_rnglr_type_highlight_EQ * '_rnglr_type_highlight_ERROR * '_rnglr_type_highlight_LBRACE * '_rnglr_type_highlight_MINUS * '_rnglr_type_highlight_MUL * '_rnglr_type_highlight_NUMBER * '_rnglr_type_highlight_PLUS * '_rnglr_type_highlight_RBRACE * '_rnglr_type_highlight_SEMI * '_rnglr_type_highlight_VARIABLE * '_rnglr_type_start * '_rnglr_type_yard_exp_brackets_1 * '_rnglr_type_yard_some_1 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_some_1) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new StartNonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 19 "extCalc.yrd"
               : '_rnglr_type_start) 
# 255 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_start) 
            )
# 19 "extCalc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 265 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_assign) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new Yard_some_1NonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 19 "extCalc.yrd"
               : '_rnglr_type_yard_some_1) 
# 288 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_assign) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_some_1) 
               |> List.iter (fun (h2) -> 
                _rnglr_cycle_res := (
                  

                  let parent = new Yard_some_1NonTermNode()
                  let children = [h1; h2]
                  addSemantic parent children
                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "extCalc.yrd"
               : '_rnglr_type_yard_some_1) 
# 313 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_VARIABLE) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_highlight_EQ) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (h3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_highlight_SEMI) 
                   |> List.iter (fun (h4) -> 
                    _rnglr_cycle_res := (
                      

                      let parent = new AssignNonTermNode()
                      let children = [h1; h2; h3; h4]
                      addSemantic parent children
                      
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 21 "extCalc.yrd"
               : '_rnglr_type_assign) 
# 342 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
             |> List.iter (fun (h1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_1) 
               |> List.iter (fun (h2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (h3) -> 
                  _rnglr_cycle_res := (
                    

                    let parent = new ExprNonTermNode()
                    let children = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 23 "extCalc.yrd"
               : '_rnglr_type_expr) 
# 369 "ExtCalc.yrd.fs"
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
                

                let parent = new ExprNonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "extCalc.yrd"
               : '_rnglr_type_expr) 
# 392 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_VARIABLE) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new ExprNonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "extCalc.yrd"
               : '_rnglr_type_expr) 
# 415 "ExtCalc.yrd.fs"
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
                    

                    let parent = new ExprNonTermNode()
                    let children = [h1; h2; h3]
                    addSemantic parent children
                    
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 23 "extCalc.yrd"
               : '_rnglr_type_expr) 
# 442 "ExtCalc.yrd.fs"
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
                

                let parent = new ExprNonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "extCalc.yrd"
               : '_rnglr_type_expr) 
# 465 "ExtCalc.yrd.fs"
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
                

                let parent = new Yard_exp_brackets_1NonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 488 "ExtCalc.yrd.fs"
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
                

                let parent = new Yard_exp_brackets_1NonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 511 "ExtCalc.yrd.fs"
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
                

                let parent = new Yard_exp_brackets_1NonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 534 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_highlight_MUL) 
             |> List.iter (fun (h1) -> 
              _rnglr_cycle_res := (
                

                let parent = new Yard_exp_brackets_1NonTermNode()
                let children = [h1]
                addSemantic parent children
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_1) 
# 557 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with VARIABLE _rnglr_val -> [_rnglr_val] | a -> failwith "VARIABLE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new VARIABLETermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_VARIABLE) 
# 580 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EQ _rnglr_val -> [_rnglr_val] | a -> failwith "EQ expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new EQTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_EQ) 
# 603 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new SEMITermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_SEMI) 
# 626 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new NUMBERTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_NUMBER) 
# 649 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new LBRACETermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_LBRACE) 
# 672 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new RBRACETermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_RBRACE) 
# 695 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new ERRORTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_ERROR) 
# 718 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new PLUSTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_PLUS) 
# 741 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new MINUSTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_MINUS) 
# 764 "ExtCalc.yrd.fs"
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
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new DIVTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_DIV) 
# 787 "ExtCalc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MUL _rnglr_val -> [_rnglr_val] | a -> failwith "MUL expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                

                let pos = _rnglr_var_0
                let ranges = calculatePos pos
                new MULTermNode(ranges) :> ITreeNode
                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_highlight_MUL) 
# 810 "ExtCalc.yrd.fs"
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
# 828 "ExtCalc.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_assign)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_DIV)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_EQ)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_MUL)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_NUMBER)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_PLUS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_RBRACE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_SEMI)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_highlight_VARIABLE)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_start)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_some_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
