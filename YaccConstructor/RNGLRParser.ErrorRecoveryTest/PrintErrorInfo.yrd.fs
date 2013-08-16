
# 2 "PrintErrorInfo.yrd.fs"
module RNGLR.ParsePrintErrorInfo
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "a"
    | 1 -> "e1"
    | 2 -> "e2"
    | 3 -> "error"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "B"
    | 7 -> "C"
    | 8 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | B _ -> 6
    | C _ -> 7
    | RNGLR_EOF _ -> 8

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|0; 4; 1; 1; 2; 2|]
let private rules = [|5; 1; 6; 2; 7; 0; 3; 3|]
let private rulesStart = [|0; 5; 6; 7; 7; 8; 8|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 8; 4; 5; 7; 6|]
let private small_gotos =
        [|2; 0; 327681; 131074; 65538; 196611; 196609; 393220; 262146; 131077; 196614; 327681; 458759|]
let gotos = Array.zeroCreate 9
for i = 0 to 8 do
        gotos.[i] <- Array.zeroCreate 9
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
let private lists_reduces = [|[|0,5|]; [|4,1|]; [|2,1|]|]
let private small_reduces =
        [|393217; 524288; 458753; 458753; 524289; 393218|]
let reduces = Array.zeroCreate 9
for i = 0 to 8 do
        reduces.[i] <- Array.zeroCreate 9
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
let private lists_zeroReduces = [|[|3; 2|]; [|5; 4|]|]
let private small_zeroReduces =
        [|131073; 393216; 262145; 458753|]
let zeroReduces = Array.zeroCreate 9
for i = 0 to 8 do
        zeroReduces.[i] <- Array.zeroCreate 9
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
let private accStates = Array.zeroCreate 9
for i = 0 to 8 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 8
let errorIndex = 3
let errorRulesExists = true
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null))|])), [|new Family(3, new Nodes([||]))|])), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null))|])), [|new Family(5, new Nodes([||]))|])), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null))|])), [|new Family(3, new Nodes([||]))|])), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null))|])), [|new Family(5, new Nodes([||]))|])), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_a * '_rnglr_type_e1 * '_rnglr_type_e2 * '_rnglr_type_error * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_e1) 
               |> List.iter (fun (e1) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_e2) 
                   |> List.iter (fun (e2) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with C _rnglr_val -> [_rnglr_val] | a -> failwith "C expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 2 "PrintErrorInfo.yrd"
                                              e1 + e2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 2 "PrintErrorInfo.yrd"
               : '_rnglr_type_a) 
# 150 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_a) 
            )
# 2 "PrintErrorInfo.yrd"
               : '_rnglr_type_yard_start_rule) 
# 160 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : list<ErrorNode>)
             |> List.iter (fun (e) -> 
              _rnglr_cycle_res := (
                
# 3 "PrintErrorInfo.yrd"
                             printfn "e1: skipped %A " e.tokens; -1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "PrintErrorInfo.yrd"
               : '_rnglr_type_e1) 
# 180 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 3 "PrintErrorInfo.yrd"
                                                                      0
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 3 "PrintErrorInfo.yrd"
               : '_rnglr_type_e1) 
# 198 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : list<ErrorNode>)
             |> List.iter (fun (e) -> 
              _rnglr_cycle_res := (
                
# 4 "PrintErrorInfo.yrd"
                             printfn "e2: error on %A " e.errorOn; -2
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 4 "PrintErrorInfo.yrd"
               : '_rnglr_type_e2) 
# 218 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 4 "PrintErrorInfo.yrd"
                                                                        0
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 4 "PrintErrorInfo.yrd"
               : '_rnglr_type_e2) 
# 236 "PrintErrorInfo.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
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
# 254 "PrintErrorInfo.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_a)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
