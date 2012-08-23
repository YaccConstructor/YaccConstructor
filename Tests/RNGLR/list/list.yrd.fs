module RNGLR.ParseList
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
    | A of int
    | B of int
    | C of int
    | EOF of int

let numToString = function 
    | 0 -> "elem"
    | 1 -> "list"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "C"
    | 6 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | C _ -> 5
    | EOF _ -> 6

let leftSide = [|1; 1; 2; 0; 0|]
let rules = [|0; 1; 5; 0; 1; 4; 3|]
let rulesStart = [|0; 1; 4; 5; 6; 7|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    Yard.Generators.RNGLR.AST.astToDot<Token> startInd numToString getRight

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,2; 3,5; 4,6|]; 2, [|5,3|]; 3, [|0,4; 3,5; 4,6|]|]
    let gotos = Array.zeroCreate 7
    for i = 0 to 6 do
        gotos.[i] <- Array.create 7 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[||]; [|0,1|]; [|1,3|]; [|4,1|]; [|3,1|]|]
    let small_reduces =
        [|65538; 327681; 393217; 262146; 327682; 393218; 327682; 327683; 393219; 393218; 327684; 393220|]
    let reduces = Array.zeroCreate 7
    for i = 0 to 6 do
        reduces.[i] <- Array.create 7 [||]
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
    let zeroReduces = Array.zeroCreate 7
    for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 7 [||]
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [2]
    let accStates = Array.zeroCreate 7
    for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 6
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

let _rnglr_index = [|0; 1; 0; 0; 1|]
let _rnglr_stack_calls_ind = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_calls_node = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_res = new ResizeArray<_>()
let _rnglr_rule_elem = Array.zeroCreate 2
let _rnglr_call_rule_elem = Array.zeroCreate 2
let _rnglr_res_count_elem = ref 0
let _rnglr_result_elem = ref <| Array.zeroCreate 0
let _rnglr_rule_list = Array.zeroCreate 2
let _rnglr_call_rule_list = Array.zeroCreate 2
let _rnglr_res_count_list = ref 0
let _rnglr_result_list = ref <| Array.zeroCreate 0
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
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal elem expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm elem can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_elem.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_elem.Value.[!num] <- res)
_rnglr_translate_token.[4] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal list expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm list can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_list.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_list.Value.[!num] <- res)
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
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal elem expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_elem
      incr _rnglr_res_count_elem;
      _rnglr_stack_calls_ind.Push(3)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm elem can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_elem.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[1] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal list expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_list
      incr _rnglr_res_count_list;
      _rnglr_stack_calls_ind.Push(4)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm list can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_list.[_rnglr_index.[_rnglr_number]] _rnglr_children
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
_rnglr_call_rule_list.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_list.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    if !(_rnglr_treenum _rnglr_children.[2]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[2]))
    ()
_rnglr_call_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_elem.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    ()
_rnglr_call_rule_elem.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    ()
_rnglr_rule_list.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_elem.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (e) -> 
        _rnglr_cycle_res := ( [e] )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
_rnglr_rule_list.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_list.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (l) -> 
        (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (C value) -> [value] | _-> failwith "Token C expected") 
         |> List.iter (fun (_rnglr_var_1) -> 
          _rnglr_result_elem.Value.[(_rnglr_treenum _rnglr_children.[2]).Value] 
           |> List.iter (fun (r) -> 
            _rnglr_cycle_res := (  l@[r]  )::!_rnglr_cycle_res ) ) )
      !_rnglr_cycle_res
    )
_rnglr_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    _rnglr_result_list.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
_rnglr_rule_elem.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    []
_rnglr_rule_elem.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    []
let translate node = 
  _rnglr_res_count_elem := 0
  _rnglr_res_count_list := 0
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
  _rnglr_result_elem := Array.zeroCreate !_rnglr_res_count_elem
  _rnglr_result_list := Array.zeroCreate !_rnglr_res_count_list
  _rnglr_result_yard_start_rule := Array.zeroCreate !_rnglr_res_count_yard_start_rule
  for (i,x) in _rnglr_stack_res do _rnglr_translate_token.[i] x
  _rnglr_result_yard_start_rule.Value.[!(_rnglr_treenum node)]
