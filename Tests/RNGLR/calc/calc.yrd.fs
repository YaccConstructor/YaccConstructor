module RNGLR.ParseCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | ADD of int
    | B of int
    | EOF of int
    | MUL of int

let numToString = function
    | 0 -> "expr"
    | 1 -> "fact"
    | 2 -> "num"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "ADD"
    | 6 -> "B"
    | 7 -> "EOF"
    | 8 -> "MUL"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | EOF _ -> 7
    | MUL _ -> 8

let mutable private cur = 0
let leftSide = [|0; 0; 3; 1; 1; 2; 2|]
let private rules = [|1; 0; 5; 0; 0; 2; 1; 8; 1; 6; 4|]
let private rulesStart = [|0; 1; 4; 5; 6; 9; 10; 11|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 7; 8; 9; 2; 3; 5; 6|]
let private small_gotos =
        [|5; 0; 65537; 131074; 262147; 393220; 65537; 327685; 131077; 6; 65537; 131074; 262147; 393220; 196609; 327685; 262145; 524295; 327684; 65544; 131074; 262147; 393220; 393217; 524295|]
let gotos = Array.zeroCreate 10
for i = 0 to 9 do
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
let private lists_reduces = [|[|1,3|]; [|0,1|]; [|4,3|]; [|3,1|]; [|6,1|]; [|5,1|]|]
let private small_reduces =
        [|196610; 327680; 458752; 262146; 327681; 458753; 393219; 327682; 458754; 524290; 458755; 327683; 458755; 524291; 524291; 327684; 458756; 524292; 589827; 327685; 458757; 524293|]
let reduces = Array.zeroCreate 10
for i = 0 to 9 do
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 10
for i = 0 to 9 do
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
let private accStates = Array.zeroCreate 10
for i = 0 to 9 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 7
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_expr * '_rnglr_type_fact * '_rnglr_type_num * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_fact) 
             |> List.iter (fun (f) -> 
              _rnglr_cycle_res := (
                
# 1 "Calc.yrd"
                f
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Calc.yrd"
               : '_rnglr_type_expr) 
# 1001 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ADD _rnglr_val -> [_rnglr_val] | a -> failwith "ADD expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := (
                    
# 1 "Calc.yrd"
                    a + b
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Calc.yrd"
               : '_rnglr_type_expr) 
# 1001 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 1 "Calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 1001 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_num) 
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 2 "Calc.yrd"
                n
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 2 "Calc.yrd"
               : '_rnglr_type_fact) 
# 1001 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_fact) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with MUL _rnglr_val -> [_rnglr_val] | a -> failwith "MUL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_fact) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := (
                    
# 2 "Calc.yrd"
                    a * b
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 2 "Calc.yrd"
               : '_rnglr_type_fact) 
# 1001 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 3 "Calc.yrd"
                5
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Calc.yrd"
               : '_rnglr_type_num) 
# 1001 "Calc.yrd.fs"
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
                
# 3 "Calc.yrd"
                3
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Calc.yrd"
               : '_rnglr_type_num) 
# 1001 "Calc.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_fact)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_num)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
