
# 2 "Order.yrd.fs"
module RNGLR.ParseOrder
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "Order.yrd"

let res = ref []

# 13 "Order.yrd.fs"
type Token =
    | A of int
    | EOF of int

let numToString = function
    | 0 -> "e1"
    | 1 -> "e2"
    | 2 -> "e3"
    | 3 -> "s"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 5
    | EOF _ -> 6

let mutable private cur = 0
let leftSide = [|3; 4; 0; 1; 2|]
let private rules = [|0; 0; 3; 1; 1; 2; 2; 5|]
let private rulesStart = [|0; 2; 3; 5; 7; 8|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 11; 9; 13; 8; 2; 3; 4; 5; 6; 7; 10; 12|]
let private small_gotos =
        [|5; 0; 65537; 131074; 196611; 327684; 65540; 5; 65542; 131074; 327684; 196611; 65543; 131080; 327684; 327682; 131081; 327690; 589826; 131083; 327684; 720899; 65548; 131074; 327684|]
let gotos = Array.zeroCreate 14
for i = 0 to 13 do
        gotos.[i] <- Array.zeroCreate 7
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
let private lists_reduces = [|[|0,2|]; [|2,2|]; [|3,2|]; [|4,1|]|]
let private small_reduces =
        [|131073; 393216; 262145; 393217; 393217; 393218; 458753; 393219; 524289; 327683; 655361; 327682; 786433; 327681|]
let reduces = Array.zeroCreate 14
for i = 0 to 13 do
        reduces.[i] <- Array.zeroCreate 7
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
let zeroReduces = Array.zeroCreate 14
for i = 0 to 13 do
        zeroReduces.[i] <- Array.zeroCreate 7
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
let private small_acc = [13]
let private accStates = Array.zeroCreate 14
for i = 0 to 13 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 6
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_e1 * '_rnglr_type_e2 * '_rnglr_type_e3 * '_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_e1) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_e1) 
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 4 "Order.yrd"
                              ()
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Order.yrd"
               : '_rnglr_type_s) 
# 125 "Order.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 4 "Order.yrd"
               : '_rnglr_type_yard_start_rule) 
# 135 "Order.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_e2) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_e2) 
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 5 "Order.yrd"
                              ()
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 5 "Order.yrd"
               : '_rnglr_type_e1) 
# 157 "Order.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_e3) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_e3) 
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 6 "Order.yrd"
                              ()
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "Order.yrd"
               : '_rnglr_type_e2) 
# 179 "Order.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (a) -> 
              _rnglr_cycle_res := (
                
# 7 "Order.yrd"
                           res := a :: !res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 7 "Order.yrd"
               : '_rnglr_type_e3) 
# 199 "Order.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
