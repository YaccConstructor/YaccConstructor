
# 2 "simpleCalc_with_Nterms_4.yrd.fs"
module RNGLR.SimpleCalcWithNTerms_4
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.AParser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | NUM of (int)
    | PLUS of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | NUM x -> box x
    | PLUS x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "e"
    | 1 -> "e2"
    | 2 -> "error"
    | 3 -> "pl"
    | 4 -> "s"
    | 5 -> "yard_start_rule"
    | 6 -> "NUM"
    | 7 -> "PLUS"
    | 8 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | NUM _ -> 6
    | PLUS _ -> 7
    | RNGLR_EOF _ -> 8

let isLiteral = function
    | NUM _ -> false
    | PLUS _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|4; 5; 0; 3; 1|]
let private rules = [|1; 6; 4; 6; 7; 0; 3|]
let private rulesStart = [|0; 2; 3; 4; 5; 7|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 6; 7; 2; 3; 5|]
let private small_gotos =
        [|4; 0; 65537; 262146; 393219; 65538; 196612; 458757; 262145; 393222|]
let gotos = Array.zeroCreate 8
for i = 0 to 7 do
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
let private lists_reduces = [|[|4,2|]; [|3,1|]; [|0,2|]; [|2,1|]|]
let private small_reduces =
        [|131073; 393216; 196609; 393217; 327681; 524290; 458753; 458755|]
let reduces = Array.zeroCreate 8
for i = 0 to 7 do
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
let zeroReduces = Array.zeroCreate 8
for i = 0 to 7 do
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
let private small_acc = [6]
let private accStates = Array.zeroCreate 8
for i = 0 to 7 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 8
let errorIndex = 2
let errorRulesExists = false
let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_e * '_rnglr_type_e2 * '_rnglr_type_error * '_rnglr_type_pl * '_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_e2) 
             |> List.iter (fun (S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with NUM _rnglr_val -> [_rnglr_val] | a -> failwith "NUM expected, but %A found" a )
               |> List.iter (fun (S2) -> 
                _rnglr_cycle_res := (
                  
# 2 "simpleCalc_with_Nterms_4.yrd"
                     S1, S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 2 "simpleCalc_with_Nterms_4.yrd"
               : '_rnglr_type_s) 
# 140 "simpleCalc_with_Nterms_4.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 2 "simpleCalc_with_Nterms_4.yrd"
               : '_rnglr_type_yard_start_rule)
               
# 151 "simpleCalc_with_Nterms_4.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUM _rnglr_val -> [_rnglr_val] | a -> failwith "NUM expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 2 "simpleCalc_with_Nterms_4.yrd"
                      S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "simpleCalc_with_Nterms_4.yrd"
               : '_rnglr_type_e) 
# 171 "simpleCalc_with_Nterms_4.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 4 "simpleCalc_with_Nterms_4.yrd"
                    S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 4 "simpleCalc_with_Nterms_4.yrd"
               : '_rnglr_type_pl) 
# 191 "simpleCalc_with_Nterms_4.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_e) 
             |> List.iter (fun (S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_pl) 
               |> List.iter (fun (S2) -> 
                _rnglr_cycle_res := (
                  
# 5 "simpleCalc_with_Nterms_4.yrd"
                      S1, S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 5 "simpleCalc_with_Nterms_4.yrd"
               : '_rnglr_type_e2) 
# 213 "simpleCalc_with_Nterms_4.yrd.fs"
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
# 231 "simpleCalc_with_Nterms_4.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_e2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_pl)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
