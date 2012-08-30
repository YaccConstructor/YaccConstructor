module RNGLR.ParseAttrs
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | EOF of int

let numToString = function
    | 0 -> "s"
    | 1 -> "yard_start_rule"
    | 2 -> "A"
    | 3 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 2
    | EOF _ -> 3

let mutable private cur = 0
let leftSide = [|0; 0; 1|]
let private rules = [|2; 0; 2; 0|]
let private rulesStart = [|0; 1; 3; 4|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 3; 2|]
let private small_gotos =
        [|2; 0; 131073; 65537; 131074|]
let gotos = Array.zeroCreate 4
for i = 0 to 3 do
        gotos.[i] <- Array.zeroCreate 4
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
let private lists_reduces = [|[|1,2|]; [|0,1|]|]
let private small_reduces =
        [|131074; 131072; 196608; 196610; 131073; 196609|]
let reduces = Array.zeroCreate 4
for i = 0 to 3 do
        reduces.[i] <- Array.zeroCreate 4
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
let zeroReduces = Array.zeroCreate 4
for i = 0 to 3 do
        zeroReduces.[i] <- Array.zeroCreate 4
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
let private accStates = Array.zeroCreate 4
for i = 0 to 3 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 3
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
let _rnglr_epsilons : Tree<Token>[] = [|null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( fun arg ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                # 35 "Attrs.yrd"
                arg
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( fun arg ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) (arg * 2)
             |> List.iter (fun (r) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_1) -> 
                _rnglr_cycle_res := (
                  # 28 "Attrs.yrd"
                  r
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( fun arg ->
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) arg
           ) : '_rnglr_type_yard_start_rule)
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( fun arg ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)  arg ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun arg ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)  arg ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
