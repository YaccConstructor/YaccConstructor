module RNGLR.ParseCounter
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

let leftSide = [|0; 0; 1|]
let private rules = [|2; 2; 0; 0|]
let private rulesStart = [|0; 1; 3; 4|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot startInd numToString getRight)

let inline unpack x = x >>> 16, x <<< 16 >>> 16
let private small_gotos =
        [|0, [|0,1; 2,2|]; 2, [|0,3; 2,2|]|]
let private gotos = Array.zeroCreate 4
for i = 0 to 3 do
        gotos.[i] <- Array.create 4 None
for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
let private lists_reduces = [|[||]; [|0,1|]; [|1,2|]|]
let private small_reduces =
        [|131073; 196609; 196609; 196610|]
let reduces = Array.zeroCreate 4
for i = 0 to 3 do
        reduces.[i] <- Array.create 4 [||]
let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
let private lists_zeroReduces = [|[||]|]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 4
for i = 0 to 3 do
        zeroReduces.[i] <- Array.create 4 [||]
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
let private accStates = Array.zeroCreate 4
for i = 0 to 3 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 3
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
let _rnglr_epsilons : Tree<Token>[] = [|null; null|]
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_s * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 1 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_s) 
               |> List.iter (fun (v) -> 
                _rnglr_cycle_res := ( 1 + v )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_s)
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate  (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  let inline _rnglr_tokenToEmptyRange (x : 'a) = Microsoft.FSharp.Text.Lexing.Position.Empty, Microsoft.FSharp.Text.Lexing.Position.Empty
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats _rnglr_epsilons _rnglr_tokenToEmptyRange) : '_rnglr_type_yard_start_rule
