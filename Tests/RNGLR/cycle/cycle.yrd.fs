module RNGLR.ParseCycle
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function 
    | 0 -> "s"
    | 1 -> "start"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5

let leftSide = [|1; 2; 0; 0|]
let rules = [|0; 4; 1; 0; 3|]
let rulesStart = [|0; 2; 3; 4; 5|]
let startRule = 1

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot startInd numToString getRight)

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,3; 3,4|]; 1, [|4,2|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 6 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[||]; [|2,1|]; [|0,2|]; [|3,1|]|]
    let small_reduces =
        [|65537; 262145; 131073; 327682; 262145; 262147|]
    let reduces = Array.zeroCreate 5
    for i = 0 to 4 do
        reduces.[i] <- Array.create 6 [||]
    let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
    let lists_zeroReduces = [|[||]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 5
    for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 6 [||]
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [3]
    let accStates = Array.zeroCreate 5
    for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 5
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
let _rnglr_epsilons : Tree<Token>[] = [|null; null; null|]
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_start * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (f) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_1) -> 
                _rnglr_cycle_res := ( f )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_start)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_start) 
           ) : '_rnglr_type_yard_start_rule)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
             |> List.iter (fun (v) -> 
              _rnglr_cycle_res := ( v+1 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 0 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_start)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats _rnglr_epsilons) : '_rnglr_type_yard_start_rule
