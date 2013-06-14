
# 2 "Calc.yrd.fs"
module RNGLR.ParseCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int * int)
    | B of (int * int)
    | RNGLR_EOF of (int * int)
    | ``L 8`` of (int * int)
    | ``L 9`` of (int * int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | "*" -> ``L 8`` (posStart, posEnd)
    | "+" -> ``L 9`` (posStart, posEnd)
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | B x -> box x
    | RNGLR_EOF x -> box x
    | ``L 8`` x -> box x
    | ``L 9`` x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "fact"
    | 3 -> "num"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "B"
    | 7 -> "RNGLR_EOF"
    | 8 -> "'*'"
    | 9 -> "'+'"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | B _ -> 6
    | RNGLR_EOF _ -> 7
    | ``L 8`` _ -> 8
    | ``L 9`` _ -> 9

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | RNGLR_EOF _ -> false
    | ``L 8`` _ -> true
    | ``L 9`` _ -> true

let getLiteralNames = ["*";"+";]
let mutable private cur = 0
let leftSide = [|1; 1; 4; 2; 2; 3; 3|]
let private rules = [|2; 1; 9; 1; 1; 3; 2; 8; 2; 6; 5|]
let private rulesStart = [|0; 1; 4; 5; 6; 9; 10; 11|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 7; 8; 9; 2; 3; 5; 6|]
let private small_gotos =
        [|5; 65536; 131073; 196610; 327683; 393220; 65537; 589829; 131077; 65542; 131073; 196610; 327683; 393220; 196609; 589829; 262145; 524295; 327684; 131080; 196610; 327683; 393220; 393217; 524295|]
let gotos = Array.zeroCreate 10
for i = 0 to 9 do
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
let private lists_reduces = [|[|1,3|]; [|0,1|]; [|4,3|]; [|3,1|]; [|6,1|]; [|5,1|]|]
let private small_reduces =
        [|196610; 458752; 589824; 262146; 458753; 589825; 393219; 458754; 524290; 589826; 458755; 458755; 524291; 589827; 524291; 458756; 524292; 589828; 589827; 458757; 524293; 589829|]
let reduces = Array.zeroCreate 10
for i = 0 to 9 do
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
let zeroReduces = Array.zeroCreate 10
for i = 0 to 9 do
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 10
for i = 0 to 9 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 7
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_fact * '_rnglr_type_num * '_rnglr_type_yard_start_rule>), 
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
                
# 10 "Calc.yrd"
                                                           f
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 10 "Calc.yrd"
               : '_rnglr_type_expr) 
# 149 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ``L 9`` _rnglr_val -> [_rnglr_val] | a -> failwith "+ expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := (
                    
# 10 "Calc.yrd"
                                              a + b
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 10 "Calc.yrd"
               : '_rnglr_type_expr) 
# 173 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 10 "Calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 183 "Calc.yrd.fs"
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
                
# 11 "Calc.yrd"
                                                          n
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 11 "Calc.yrd"
               : '_rnglr_type_fact) 
# 203 "Calc.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_fact) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ``L 8`` _rnglr_val -> [_rnglr_val] | a -> failwith "* expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_fact) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := (
                    
# 11 "Calc.yrd"
                                              a * b
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 11 "Calc.yrd"
               : '_rnglr_type_fact) 
# 227 "Calc.yrd.fs"
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
                
# 12 "Calc.yrd"
                                5
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "Calc.yrd"
               : '_rnglr_type_num) 
# 247 "Calc.yrd.fs"
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
                
# 12 "Calc.yrd"
                         3
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "Calc.yrd"
               : '_rnglr_type_num) 
# 267 "Calc.yrd.fs"
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
# 285 "Calc.yrd.fs"
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
