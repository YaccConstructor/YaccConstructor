module RNGLR.ParseFirst
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function 
    | 0 -> "a"
    | 1 -> "yard_start_rule"
    | 2 -> "A"
    | 3 -> "B"
    | 4 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 2
    | B _ -> 3
    | EOF _ -> 4

let leftSide = [|0; 0; 1|]
let rules = [|2; 0; 3; 0|]
let rulesStart = [|0; 2; 3; 4|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    Yard.Generators.RNGLR.AST.astToDot<Token> startInd numToString getRight

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 2,2; 3,4|]; 2, [|0,3; 2,2; 3,4|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 5 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [0,2]; [1,1]|]
    let small_reduces =
        [|196609; 262145; 262145; 262146|]
    let reduces = Array.zeroCreate 5
    for i = 0 to 4 do
        reduces.[i] <- Array.create 5 []
    let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
    let lists_zeroReduces = [|[]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 5
    for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 5 []
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [1]
    let accStates = Array.zeroCreate 5
    for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 4
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

let _rnglr_index = [|0; 1; 0|]
let _rnglr_stack_calls = ref []
let _rnglr_stack_res = new ResizeArray<_>()
let _rnglr_rule_a = Array.zeroCreate 2
let _rnglr_call_rule_a = Array.zeroCreate 2
let _rnglr_res_count_a = ref 0
let _rnglr_result_a = ref <| Array.zeroCreate 0
let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_call_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_res_count_yard_start_rule = ref 0
let _rnglr_result_yard_start_rule = ref <| Array.zeroCreate 0
let rec _rnglr_translate_token_a = 
   fun (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) -> 
    (
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal a expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm a can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_a.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_a.Value.[!(snd _rnglr_node)] <- res)
let rec _rnglr_translate_token_yard_start_rule = 
   fun (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) -> 
    (
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal yard_start_rule expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_yard_start_rule.Value.[!(snd _rnglr_node)] <- res)
let rec _rnglr_dfs_call_a (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) = 
  if !(snd _rnglr_node) = -1 then
    snd (_rnglr_node) := !_rnglr_res_count_a
    incr _rnglr_res_count_a;
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal a expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_stack_calls := (false, _rnglr_translate_token_a,_rnglr_node)::!_rnglr_stack_calls; 
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm a can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_a.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
let rec _rnglr_dfs_call_yard_start_rule (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) = 
  if !(snd _rnglr_node) = -1 then
    snd (_rnglr_node) := !_rnglr_res_count_yard_start_rule
    incr _rnglr_res_count_yard_start_rule;
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal yard_start_rule expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_stack_calls := (false, _rnglr_translate_token_yard_start_rule,_rnglr_node)::!_rnglr_stack_calls; 
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
_rnglr_call_rule_a.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    if !(snd _rnglr_children.[1]) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_a,_rnglr_children.[1])::!_rnglr_stack_calls; 
    ()
_rnglr_call_rule_a.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    ()
_rnglr_call_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    if !(snd _rnglr_children.[0]) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_a,_rnglr_children.[0])::!_rnglr_stack_calls; 
    ()
_rnglr_rule_a.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    []
_rnglr_rule_a.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    []
_rnglr_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    _rnglr_result_a.Value.[(snd _rnglr_children.[0]).Value] 
let translate node = 
  _rnglr_res_count_a := 0
  _rnglr_res_count_yard_start_rule := 0
  _rnglr_stack_calls := []
  _rnglr_stack_res.Clear()
  if !(snd node) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_yard_start_rule,node)::!_rnglr_stack_calls; 
  while not _rnglr_stack_calls.Value.IsEmpty do
    let n,f,x = _rnglr_stack_calls.Value.Head
    _rnglr_stack_calls := _rnglr_stack_calls.Value.Tail
    if n then f x
    else _rnglr_stack_res.Add(f,x) |> ignore
  _rnglr_result_a := Array.zeroCreate !_rnglr_res_count_a
  _rnglr_result_yard_start_rule := Array.zeroCreate !_rnglr_res_count_yard_start_rule
  for (f,x) in _rnglr_stack_res do f x
  _rnglr_result_yard_start_rule.Value.[!(snd node)]
