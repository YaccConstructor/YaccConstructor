
# 2 "simple.yrd.fs"
module RNGLR.Parse
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "simple.yrd"

let f1 s x = printf "%s %A\n" s x; x.ToString()
let f2 s x y = printf "%s %A %A\n" s x y; x.ToString()+y.ToString()

# 14 "simple.yrd.fs"
type Token =
    | A of string
    | B of string
    | EOF of string
    | X of string
    | Y of string

let numToString = function
    | 0 -> "s"
    | 1 -> "yard_start_rule"
    | 2 -> "A"
    | 3 -> "B"
    | 4 -> "EOF"
    | 5 -> "X"
    | 6 -> "Y"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 2
    | B _ -> 3
    | EOF _ -> 4
    | X _ -> 5
    | Y _ -> 6

let mutable private cur = 0
let leftSide = [|0; 0; 0; 0; 1|]
let private rules = [|6; 5; 0; 3; 0; 2; 0|]
let private rulesStart = [|0; 1; 2; 4; 6; 7|]
let startRule = 4

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 5; 2; 3|]
let private small_gotos =
        [|3; 0; 327681; 393218; 65538; 131075; 196612|]
let gotos = Array.zeroCreate 6
for i = 0 to 5 do
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
let private lists_reduces = [|[|3,2|]; [|2,2|]; [|1,1|]; [|0,1|]|]
let private small_reduces =
        [|131075; 131072; 196608; 262144; 196611; 131073; 196609; 262145; 262147; 131074; 196610; 262146; 327683; 131075; 196611; 262147|]
let reduces = Array.zeroCreate 6
for i = 0 to 5 do
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
let zeroReduces = Array.zeroCreate 6
for i = 0 to 5 do
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 6
for i = 0 to 5 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 4
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with Y _rnglr_val -> [_rnglr_val] | a -> failwith "Y expected, but %A found" a )
             |> List.iter (fun (arg) -> 
              _rnglr_cycle_res := (
                
# 9 "simple.yrd"
                          f1 "y" arg
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd"
               : '_rnglr_type_s) 
# 130 "simple.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with X _rnglr_val -> [_rnglr_val] | a -> failwith "X expected, but %A found" a )
             |> List.iter (fun (arg) -> 
              _rnglr_cycle_res := (
                
# 8 "simple.yrd"
                          f1 "x" arg
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd"
               : '_rnglr_type_s) 
# 150 "simple.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (arg1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
               |> List.iter (fun (arg2) -> 
                _rnglr_cycle_res := (
                  
# 7 "simple.yrd"
                                    f2 "b" arg1 arg2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd"
               : '_rnglr_type_s) 
# 172 "simple.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (arg1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
               |> List.iter (fun (arg2) -> 
                _rnglr_cycle_res := (
                  
# 6 "simple.yrd"
                                    f2 "a" arg1 arg2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd"
               : '_rnglr_type_s) 
# 194 "simple.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 6 "simple.yrd"
               : '_rnglr_type_yard_start_rule) 
# 204 "simple.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
