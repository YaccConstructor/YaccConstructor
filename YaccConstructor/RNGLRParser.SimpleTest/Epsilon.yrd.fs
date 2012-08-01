module RNGLR.ParseEpsilon
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | C of int
    | EOF of int

let numToString = function 
    | 0 -> "s"
    | 1 -> "yard_rule_op_1"
    | 2 -> "yard_rule_op_2"
    | 3 -> "yard_rule_op_3"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "B"
    | 7 -> "C"
    | 8 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 5
    | B _ -> 6
    | C _ -> 7
    | EOF _ -> 8

let leftSide = [|1; 1; 2; 2; 3; 3; 0; 4|]
let private rules = [|5; 6; 7; 1; 2; 3; 0|]
let private rulesStart = [|0; 0; 1; 1; 2; 2; 3; 6; 7|]
let startRule = 7

let acceptEmptyInput = true

let defaultAstToDot = 
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let inline unpack x = x >>> 16, x <<< 16 >>> 16
let private small_gotos =
        [|0, [|0,1; 1,2; 5,7|]; 2, [|2,3; 6,6|]; 3, [|3,4; 7,5|]|]
let private gotos = Array.zeroCreate 8
for i = 0 to 7 do
        gotos.[i] <- Array.create 9 None
for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
let private lists_reduces = [|[||]; [|6,1|]; [|6,2|]; [|6,3|]; [|5,1|]; [|3,1|]; [|1,1|]|]
let private small_reduces =
        [|131073; 524289; 196609; 524290; 262145; 524291; 327681; 524292; 393218; 458757; 524293; 458755; 393222; 458758; 524294|]
let reduces = Array.zeroCreate 8
for i = 0 to 7 do
        reduces.[i] <- Array.create 9 [||]
let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
let private lists_zeroReduces = [|[||]; [|0|]; [|7; 6; 0|]; [|2|]; [|4|]|]
let private small_zeroReduces =
        [|3; 393217; 458753; 524290; 131074; 458755; 524291; 196609; 524292|]
let zeroReduces = Array.zeroCreate 8
for i = 0 to 7 do
        zeroReduces.[i] <- Array.create 9 [||]
let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 8
for i = 0 to 7 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 8
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>([|NonTerm (new ResizeArray<_>([(6,[|1; 2; 3|])])); NonTerm (new ResizeArray<_>([(0,[||])])); NonTerm (new ResizeArray<_>([(2,[||])])); NonTerm (new ResizeArray<_>([(4,[||])]))|],0); new Tree<_>([|NonTerm (new ResizeArray<_>([(0,[||])]))|],0); new Tree<_>([|NonTerm (new ResizeArray<_>([(2,[||])]))|],0); new Tree<_>([|NonTerm (new ResizeArray<_>([(4,[||])]))|],0); new Tree<_>([|NonTerm (new ResizeArray<_>([(7,[|1|])])); NonTerm (new ResizeArray<_>([(6,[|2; 3; 4|])])); NonTerm (new ResizeArray<_>([(0,[||])])); NonTerm (new ResizeArray<_>([(2,[||])])); NonTerm (new ResizeArray<_>([(4,[||])]))|],0)|]
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_yard_rule_op_1 * '_rnglr_type_yard_rule_op_2 * '_rnglr_type_yard_rule_op_3 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := ( 1 )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_1)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 10 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_1)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := ( 1 )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_2)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 10 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_2)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := ( 1 )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_3)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with C _rnglr_val -> [_rnglr_val] | a -> failwith "C expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 10 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_yard_rule_op_3)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_op_1) 
             |> List.iter (fun (a1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_op_2) 
               |> List.iter (fun (a2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_op_3) 
                 |> List.iter (fun (a3) -> 
                  _rnglr_cycle_res := ( a1 + a2 + a3 )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
           ) : '_rnglr_type_yard_start_rule)
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate tokenToRangeFunction zeroPosition (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats _rnglr_epsilons tokenToRangeFunction zeroPosition) : '_rnglr_type_yard_start_rule
