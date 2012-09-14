module RNGLR.ParseCond
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | ELSE of int
    | EOF of int
    | IF of int

let numToString = function
    | 0 -> "good"
    | 1 -> "if"
    | 2 -> "if_else"
    | 3 -> "s"
    | 4 -> "stmt"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "ELSE"
    | 8 -> "EOF"
    | 9 -> "IF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 6
    | ELSE _ -> 7
    | EOF _ -> 8
    | IF _ -> 9

let mutable private cur = 0
let leftSide = [|3; 5; 1; 1; 2; 0; 0; 4; 4|]
let private rules = [|1; 3; 9; 4; 2; 9; 0; 7; 4; 2; 4; 1; 6|]
let private rulesStart = [|0; 1; 2; 4; 5; 9; 10; 11; 12; 13|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 10; 11; 12; 13; 14; 6; 7; 8; 9; 15; 19; 16; 17; 18|]
let private small_gotos =
        [|4; 65536; 131073; 196610; 589827; 262150; 4; 65541; 131078; 262151; 393224; 589833; 327681; 458762; 393221; 65547; 131073; 262156; 393229; 589827; 917510; 14; 65541; 131078; 262159; 393224; 589833; 983041; 458768; 1048581; 65541; 131089; 262162; 393224; 589833|]
let gotos = Array.zeroCreate 20
for i = 0 to 19 do
        gotos.[i] <- Array.zeroCreate 10
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
let private lists_reduces = [|[|0,1|]; [|3,1|]; [|7,1|]; [|4,4|]; [|8,1|]; [|5,1; 3,1|]; [|6,1|]; [|2,2|]; [|6,1; 2,2|]|]
let private small_reduces =
        [|65537; 524288; 131073; 524289; 458753; 524290; 524289; 524291; 589825; 524292; 655362; 458754; 524290; 720898; 458757; 524289; 786434; 458758; 524295; 851970; 458756; 524292; 1114114; 458753; 524289; 1179650; 458755; 524291; 1245186; 458760; 524295|]
let reduces = Array.zeroCreate 20
for i = 0 to 19 do
        reduces.[i] <- Array.zeroCreate 10
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
let zeroReduces = Array.zeroCreate 20
for i = 0 to 19 do
        zeroReduces.[i] <- Array.zeroCreate 10
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
let private accStates = Array.zeroCreate 20
for i = 0 to 19 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 8
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_good * '_rnglr_type_if * '_rnglr_type_if_else * '_rnglr_type_s * '_rnglr_type_stmt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 1 "Cond.yrd"
                           r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Cond.yrd"
               : '_rnglr_type_s) 
# 122 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 1 "Cond.yrd"
               : '_rnglr_type_yard_start_rule) 
# 132 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwith "IF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_stmt) 
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 2 "Cond.yrd"
                                                  r * 10
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 2 "Cond.yrd"
               : '_rnglr_type_if) 
# 154 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if_else) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 2 "Cond.yrd"
                                r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 2 "Cond.yrd"
               : '_rnglr_type_if) 
# 174 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwith "IF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_good) 
               |> List.iter (fun (t) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with ELSE _rnglr_val -> [_rnglr_val] | a -> failwith "ELSE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt) 
                   |> List.iter (fun (f) -> 
                    _rnglr_cycle_res := (
                      
# 3 "Cond.yrd"
                                                       t+f
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 3 "Cond.yrd"
               : '_rnglr_type_if_else) 
# 200 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if_else) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 4 "Cond.yrd"
                                               r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 4 "Cond.yrd"
               : '_rnglr_type_good) 
# 220 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 4 "Cond.yrd"
                               r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 4 "Cond.yrd"
               : '_rnglr_type_good) 
# 240 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 5 "Cond.yrd"
                                     r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 5 "Cond.yrd"
               : '_rnglr_type_stmt) 
# 260 "Cond.yrd.fs"
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
                
# 5 "Cond.yrd"
                          2
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 5 "Cond.yrd"
               : '_rnglr_type_stmt) 
# 280 "Cond.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_good)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_if)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_if_else)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
