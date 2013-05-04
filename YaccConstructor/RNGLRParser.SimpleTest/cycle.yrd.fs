
# 2 "Cycle.yrd.fs"
module RNGLR.ParseCycle
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function
    | 0 -> "s"
    | 1 -> "start"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5

let mutable private cur = 0
let leftSide = [|1; 2; 0; 0|]
let private rules = [|0; 4; 1; 0; 3|]
let private rulesStart = [|0; 2; 3; 4; 5|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 3; 4; 2|]
let private small_gotos =
        [|3; 0; 65537; 196610; 65537; 262147|]
let gotos = Array.zeroCreate 5
for i = 0 to 4 do
        gotos.[i] <- Array.zeroCreate 6
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
let private lists_reduces = [|[|2,1|]; [|0,2|]; [|3,1|]|]
let private small_reduces =
        [|65537; 262144; 131073; 327681; 262145; 262146|]
let reduces = Array.zeroCreate 5
for i = 0 to 4 do
        reduces.[i] <- Array.zeroCreate 6
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
let zeroReduces = Array.zeroCreate 5
for i = 0 to 4 do
        zeroReduces.[i] <- Array.zeroCreate 6
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
let private accStates = Array.zeroCreate 5
for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_start * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (f) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 2 "Cycle.yrd"
                                 f
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 2 "Cycle.yrd"
               : '_rnglr_type_start) 
# 120 "Cycle.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_start) 
            )
# 2 "Cycle.yrd"
               : '_rnglr_type_yard_start_rule) 
# 130 "Cycle.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (v) -> 
              _rnglr_cycle_res := (
                
# 3 "Cycle.yrd"
                                  v+1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Cycle.yrd"
               : '_rnglr_type_s) 
# 150 "Cycle.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 3 "Cycle.yrd"
                        0
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Cycle.yrd"
               : '_rnglr_type_s) 
# 170 "Cycle.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_start)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
