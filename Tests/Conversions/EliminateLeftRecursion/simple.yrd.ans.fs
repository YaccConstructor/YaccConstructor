
# 2 "simple.yrd.ans.fs"
module RNGLR.Parse
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "simple.yrd.ans"

let f1 s x = printf "%s %A\n" s x; x.ToString()
let f2 s x y = printf "%s %A %A\n" s x y; x.ToString()+y.ToString()

# 14 "simple.yrd.ans.fs"
type Token =
    | A of string
    | B of string
    | EOF of string
    | X of string
    | Y of string

let numToString = function
    | 0 -> "s"
    | 1 -> "yard_lr_s_s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | 6 -> "X"
    | 7 -> "Y"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5
    | X _ -> 6
    | Y _ -> 7

let mutable private cur = 0
let leftSide = [|0; 0; 2; 1; 1; 1|]
let private rules = [|7; 1; 6; 1; 0; 4; 1; 3; 1|]
let private rulesStart = [|0; 2; 4; 5; 5; 7; 9|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 3; 4; 6; 5; 7; 9|]
let private small_gotos =
        [|3; 0; 393217; 458754; 131075; 65539; 196612; 262149; 262147; 65542; 196612; 262149; 393219; 65543; 196612; 262149; 524291; 65544; 196612; 262149|]
let gotos = Array.zeroCreate 10
for i = 0 to 9 do
        gotos.[i] <- Array.zeroCreate 8
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
let private lists_reduces = [|[|1,1|]; [|1,2|]; [|5,1|]; [|5,2|]; [|4,1|]; [|4,2|]; [|0,1|]; [|0,2|]|]
let private small_reduces =
        [|131073; 327680; 196609; 327681; 262145; 327682; 327681; 327683; 393217; 327684; 458753; 327685; 524289; 327686; 589825; 327687|]
let reduces = Array.zeroCreate 10
for i = 0 to 9 do
        reduces.[i] <- Array.zeroCreate 8
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
let private lists_zeroReduces = [|[|3|]|]
let private small_zeroReduces =
        [|131073; 327680; 262145; 327680; 393217; 327680; 524289; 327680|]
let zeroReduces = Array.zeroCreate 10
for i = 0 to 9 do
        zeroReduces.[i] <- Array.zeroCreate 8
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
let eofIndex = 5
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_yard_lr_s_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with Y _rnglr_val -> [_rnglr_val] | a -> failwith "Y expected, but %A found" a )
             |> List.iter (fun (arg) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_lr_s_s) 
               |> List.iter (fun (yard_lst) -> 
                _rnglr_cycle_res := (
                  
# 9 "simple.yrd.ans"
                                                  let yard_tmp =
                                                      f1 "y" arg
                                                  List.fold (fun s f -> f s) yard_tmp yard_lst
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd.ans"
               : '_rnglr_type_s) 
# 135 "simple.yrd.ans.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with X _rnglr_val -> [_rnglr_val] | a -> failwith "X expected, but %A found" a )
             |> List.iter (fun (arg) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_lr_s_s) 
               |> List.iter (fun (yard_lst) -> 
                _rnglr_cycle_res := (
                  
# 6 "simple.yrd.ans"
                                                  let yard_tmp =
                                                      f1 "x" arg
                                                  List.fold (fun s f -> f s) yard_tmp yard_lst
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "simple.yrd.ans"
               : '_rnglr_type_s) 
# 159 "simple.yrd.ans.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 6 "simple.yrd.ans"
               : '_rnglr_type_yard_start_rule) 
# 169 "simple.yrd.ans.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 18 "simple.yrd.ans"
                             [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 12 "simple.yrd.ans"
               : '_rnglr_type_yard_lr_s_s) 
# 187 "simple.yrd.ans.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
             |> List.iter (fun (arg2) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_lr_s_s) 
               |> List.iter (fun (yard_lst) -> 
                _rnglr_cycle_res := (
                  
# 15 "simple.yrd.ans"
                                                             (fun (arg1) ->
                                                                 f2 "b" arg1 arg2
                                                             )::yard_lst
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "simple.yrd.ans"
               : '_rnglr_type_yard_lr_s_s) 
# 211 "simple.yrd.ans.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (arg2) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_lr_s_s) 
               |> List.iter (fun (yard_lst) -> 
                _rnglr_cycle_res := (
                  
# 12 "simple.yrd.ans"
                                                             (fun (arg1) -> 
                                                                 f2 "a" arg1 arg2
                                                             )::yard_lst
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "simple.yrd.ans"
               : '_rnglr_type_yard_lr_s_s) 
# 235 "simple.yrd.ans.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_lr_s_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
