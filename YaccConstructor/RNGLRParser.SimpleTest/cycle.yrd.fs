module RNGLR.ParseCycle
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function 
    | 0 -> "r"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5

let leftSide = [|0; 2; 1; 1|]
let rules = [|1; 4; 0; 1; 3|]
let rulesStart = [|0; 2; 3; 4; 5|]
let startRule = 1

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    Yard.Generators.RNGLR.AST.astToDot<Token> startInd numToString getRight

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,2; 3,4|]; 2, [|4,3|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 6 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[||]; [|2,1|]; [|0,2|]; [|3,1|]|]
    let small_reduces =
        [|131073; 262145; 196609; 327682; 262145; 262147|]
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
    let small_acc = [1]
    let accStates = Array.zeroCreate 5
    for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 5
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

let _rnglr_index = [|0; 0; 0; 1|]
let _rnglr_stack_calls_ind = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_calls_node = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_res = new ResizeArray<_>()
let _rnglr_rule_r = Array.zeroCreate 1
let _rnglr_call_rule_r = Array.zeroCreate 1
let _rnglr_res_count_r = ref 0
let _rnglr_result_r = ref <| Array.zeroCreate 0
let _rnglr_rule_s = Array.zeroCreate 2
let _rnglr_call_rule_s = Array.zeroCreate 2
let _rnglr_res_count_s = ref 0
let _rnglr_result_s = ref <| Array.zeroCreate 0
let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_call_rule_yard_start_rule = Array.zeroCreate 1
let _rnglr_res_count_yard_start_rule = ref 0
let _rnglr_result_yard_start_rule = ref <| Array.zeroCreate 0
let inline _rnglr_treenum x = match x with | Yard.Generators.RNGLR.AST.NonTerm (_,v) -> v | _ -> failwith "NonTerminal expected, but terminal found." 
let inline _rnglr_pop (x : ResizeArray<_>) = 
  let pos = x.Count - 1
  let res = x.[pos]
  x.RemoveAt(pos)
  res
let private _rnglr_translate_token = Array.zeroCreate 6
_rnglr_translate_token.[3] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal r expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm r can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_r.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_r.Value.[!num] <- res)
_rnglr_translate_token.[4] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal s expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm s can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_s.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_s.Value.[!num] <- res)
_rnglr_translate_token.[5] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal yard_start_rule expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_yard_start_rule.Value.[!num] <- res)
_rnglr_translate_token.[0] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal r expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_r
      incr _rnglr_res_count_r;
      _rnglr_stack_calls_ind.Push(3)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm r can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_r.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[1] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal s expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_s
      incr _rnglr_res_count_s;
      _rnglr_stack_calls_ind.Push(4)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm s can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_s.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[2] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal yard_start_rule expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_yard_start_rule
      incr _rnglr_res_count_yard_start_rule;
      _rnglr_stack_calls_ind.Push(5)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_call_rule_r.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_s.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_s.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    ()
_rnglr_rule_r.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_s.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (f) -> 
        (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (B value) -> [value] | _-> failwith "Token B expected") 
         |> List.iter (fun (_rnglr_var_1) -> 
          _rnglr_cycle_res := ( f )::!_rnglr_cycle_res ) )
      !_rnglr_cycle_res
    )
_rnglr_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    _rnglr_result_r.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
_rnglr_rule_s.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_s.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (r) -> 
        _rnglr_cycle_res := ( r+1 )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
_rnglr_rule_s.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (A value) -> [value] | _-> failwith "Token A expected") 
       |> List.iter (fun (_rnglr_var_0) -> 
        _rnglr_cycle_res := ( 0 )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
let translate node = 
  _rnglr_res_count_r := 0
  _rnglr_res_count_s := 0
  _rnglr_res_count_yard_start_rule := 0
  _rnglr_stack_calls_ind.Clear()
  _rnglr_stack_calls_node.Clear()
  _rnglr_stack_res.Clear()
  if !(_rnglr_treenum node) = -1 then (_rnglr_stack_calls_ind.Push(2);_rnglr_stack_calls_node.Push(node))
  while _rnglr_stack_calls_ind.Count > 0 do
    let i = _rnglr_stack_calls_ind.Pop()
    let x = _rnglr_stack_calls_node.Pop()
    if i < 3 then _rnglr_translate_token.[i] x
    else _rnglr_stack_res.Add(i,x)
  _rnglr_result_r := Array.zeroCreate !_rnglr_res_count_r
  _rnglr_result_s := Array.zeroCreate !_rnglr_res_count_s
  _rnglr_result_yard_start_rule := Array.zeroCreate !_rnglr_res_count_yard_start_rule
  for (i,x) in _rnglr_stack_res do _rnglr_translate_token.[i] x
  _rnglr_result_yard_start_rule.Value.[!(_rnglr_treenum node)]
