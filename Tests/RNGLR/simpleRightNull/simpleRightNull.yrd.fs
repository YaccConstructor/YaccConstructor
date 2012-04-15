module RNGLR.ParseSimpleRightNull
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
    | A of int
    | EOF of int

let numToString = function 
    | 0 -> "s"
    | 1 -> "t"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | EOF _ -> 4

let leftSide = [|0; 0; 2; 1|]
let rules = [|3; 0; 1; 0|]
let rulesStart = [|0; 0; 3; 4; 4|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    Yard.Generators.RNGLR.AST.astToDot<Token> startInd numToString getRight

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 3,2|]; 2, [|0,3; 3,2|]; 3, [|1,4|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 5 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [1,1]; [1,2]; [1,3]|]
    let small_reduces =
        [|131073; 262145; 196609; 262146; 262145; 262147|]
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
    let lists_zeroReduces = [|[]; [2; 0]; [0]; [3]|]
    let small_zeroReduces =
        [|1; 262145; 131073; 262146; 196609; 262147|]
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
    let small_acc = [1; 0]
    let accStates = Array.zeroCreate 5
    for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 4
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

let _rnglr_index = [|0; 1; 0; 0|]
let _rnglr_stack_calls = ref []
let _rnglr_stack_res = new ResizeArray<_>()
let _rnglr_epsilon_s : Yard.Generators.RNGLR.AST.AST<Token> list ref = (ref [Yard.Generators.RNGLR.AST.Inner (0, [||]) ], ref -1)
let _rnglr_epsilon_t : Yard.Generators.RNGLR.AST.AST<Token> list ref = (ref [Yard.Generators.RNGLR.AST.Inner (3, [||]) ], ref -1)
let _rnglr_epsilon_yard_start_rule : Yard.Generators.RNGLR.AST.AST<Token> list ref = (ref [Yard.Generators.RNGLR.AST.Inner (2, [|(Yard.Generators.RNGLR.AST.NonTerm (ref [Yard.Generators.RNGLR.AST.Inner (0, [||]) ]), ref -1)|]) ], ref -1)
let _rnglr_rule_s = Array.zeroCreate 2
let _rnglr_call_rule_s = Array.zeroCreate 2
let _rnglr_res_count_s = ref 0
let _rnglr_result_s = ref <| Array.zeroCreate 0
let _rnglr_rule_t = Array.zeroCreate 1
let _rnglr_call_rule_t = Array.zeroCreate 1
let _rnglr_res_count_t = ref 0
let _rnglr_result_t = ref <| Array.zeroCreate 0
let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_call_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_res_count_yard_start_rule = ref 0
let _rnglr_result_yard_start_rule = ref <| Array.zeroCreate 0
let rec _rnglr_translate_token_s = 
   fun (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) -> 
    (
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal s expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> _rnglr_translate_token_s _rnglr_epsilon_s
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_s.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_s.Value.[!(snd _rnglr_node)] <- res)
let rec _rnglr_translate_token_t = 
   fun (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) -> 
    (
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal t expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> _rnglr_translate_token_t _rnglr_epsilon_t
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_t.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_t.Value.[!(snd _rnglr_node)] <- res)
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
          | Yard.Generators.RNGLR.AST.Epsilon -> _rnglr_translate_token_yard_start_rule _rnglr_epsilon_yard_start_rule
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_yard_start_rule.Value.[!(snd _rnglr_node)] <- res)
let rec _rnglr_dfs_call_s (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) = 
  if !(snd _rnglr_node) = -1 then
    snd (_rnglr_node) := !_rnglr_res_count_s
    incr _rnglr_res_count_s;
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal s expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_stack_calls := (false, _rnglr_translate_token_s,_rnglr_node)::!_rnglr_stack_calls; 
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> if !(snd _rnglr_epsilon_s) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_s,_rnglr_epsilon_s)::!_rnglr_stack_calls; 
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_s.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
let rec _rnglr_dfs_call_t (_rnglr_node : Yard.Generators.RNGLR.AST.Node<Token>) = 
  if !(snd _rnglr_node) = -1 then
    snd (_rnglr_node) := !_rnglr_res_count_t
    incr _rnglr_res_count_t;
    match fst _rnglr_node with
    | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal t expected, but terminal found" 
    | Yard.Generators.RNGLR.AST.NonTerm _rnglr_multi_ast ->
      _rnglr_stack_calls := (false, _rnglr_translate_token_t,_rnglr_node)::!_rnglr_stack_calls; 
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> if !(snd _rnglr_epsilon_t) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_t,_rnglr_epsilon_t)::!_rnglr_stack_calls; 
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_t.[_rnglr_index.[_rnglr_number]] _rnglr_children 
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
          | Yard.Generators.RNGLR.AST.Epsilon -> if !(snd _rnglr_epsilon_yard_start_rule) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_yard_start_rule,_rnglr_epsilon_yard_start_rule)::!_rnglr_stack_calls; 
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
_rnglr_call_rule_s.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    ()
_rnglr_call_rule_s.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    if !(snd _rnglr_children.[1]) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_s,_rnglr_children.[1])::!_rnglr_stack_calls; 
    if !(snd _rnglr_children.[2]) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_t,_rnglr_children.[2])::!_rnglr_stack_calls; 
    ()
_rnglr_call_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    if !(snd _rnglr_children.[0]) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_s,_rnglr_children.[0])::!_rnglr_stack_calls; 
    ()
_rnglr_call_rule_t.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<_>[]) ->
    ()
_rnglr_rule_s.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    [
      yield ()
    ]
_rnglr_rule_s.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    []
_rnglr_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    _rnglr_result_s.Value.[(snd _rnglr_children.[0]).Value] 
_rnglr_rule_t.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.Node<Token>[]) -> 
    [
      yield ()
    ]
let translate node = 
  _rnglr_res_count_s := 0
  _rnglr_res_count_t := 0
  _rnglr_res_count_yard_start_rule := 0
  _rnglr_stack_calls := []
  _rnglr_stack_res.Clear()
  if !(snd node) = -1 then _rnglr_stack_calls := (true,_rnglr_dfs_call_yard_start_rule,node)::!_rnglr_stack_calls; 
  while not _rnglr_stack_calls.Value.IsEmpty do
    let n,f,x = _rnglr_stack_calls.Value.Head
    _rnglr_stack_calls := _rnglr_stack_calls.Value.Tail
    if n then f x
    else _rnglr_stack_res.Add(f,x) |> ignore
  _rnglr_result_s := Array.zeroCreate !_rnglr_res_count_s
  _rnglr_result_t := Array.zeroCreate !_rnglr_res_count_t
  _rnglr_result_yard_start_rule := Array.zeroCreate !_rnglr_res_count_yard_start_rule
  for (f,x) in _rnglr_stack_res do f x
  _rnglr_result_yard_start_rule.Value.[!(snd node)]
