
# 2 "SimpleAmb.yrd.fs"
module RNGLR.SimpleAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
type Token =
    | A of (int)
    | B of (int)
    | D of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | D x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "d"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "D"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | D _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | D _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 2; 3; 0|]
let private rules = [|4; 6; 5; 4; 0; 5; 2; 6|]
let private rulesStart = [|0; 3; 6; 7; 8|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 5; 4; 6|]
let private small_gotos =
        [|2; 131072; 262145; 131074; 2; 393219; 196609; 327684; 327681; 327685|]
let gotos = Array.zeroCreate 7
for i = 0 to 6 do
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
let private lists_reduces = [|[|1,3|]; [|3,1|]; [|0,3|]|]
let private small_reduces =
        [|262145; 458752; 327681; 327681; 393217; 458754|]
let reduces = Array.zeroCreate 7
for i = 0 to 6 do
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 7
for i = 0 to 6 do
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
let private accStates = Array.zeroCreate 7
for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 7
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_d * '_rnglr_type_error * '_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with D _rnglr_val -> [_rnglr_val] | a -> failwith "D expected, but %A found" a )
               |> List.iter (fun (S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
                 |> List.iter (fun (S3) -> 
                  _rnglr_cycle_res := (
                    
# 2 "SimpleAmb.yrd"
                        S1, S2, S3
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 2 "SimpleAmb.yrd"
               : '_rnglr_type_s) 
# 148 "SimpleAmb.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_d) 
               |> List.iter (fun (S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
                 |> List.iter (fun (S3) -> 
                  _rnglr_cycle_res := (
                    
# 2 "SimpleAmb.yrd"
                                S1, S2, S3
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 2 "SimpleAmb.yrd"
               : '_rnglr_type_s) 
# 172 "SimpleAmb.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 2 "SimpleAmb.yrd"
               : '_rnglr_type_yard_start_rule) 
# 182 "SimpleAmb.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with D _rnglr_val -> [_rnglr_val] | a -> failwith "D expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 3 "SimpleAmb.yrd"
                    S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "SimpleAmb.yrd"
               : '_rnglr_type_d) 
# 202 "SimpleAmb.yrd.fs"
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
# 220 "SimpleAmb.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_d)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
