module RNGLR.ParseCalc
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
    | A of int
    | ADD of int
    | B of int
    | EOF of int
    | MUL of int

let numToString = function 
    | 0 -> "expr"
    | 1 -> "fact"
    | 2 -> "num"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "ADD"
    | 6 -> "B"
    | 7 -> "EOF"
    | 8 -> "MUL"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | EOF _ -> 7
    | MUL _ -> 8

let leftSide = [|0; 0; 3; 1; 1; 2; 2|]
let rules = [|1; 0; 5; 0; 0; 2; 1; 8; 1; 6; 4|]
let rulesStart = [|0; 1; 4; 5; 6; 9; 10; 11|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    Yard.Generators.RNGLR.AST.astToDot<Token> startInd numToString getRight

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,4; 2,7; 4,8; 6,9|]; 1, [|5,2|]; 2, [|0,3; 1,4; 2,7; 4,8; 6,9|]; 3, [|5,2|]; 4, [|8,5|]; 5, [|1,6; 2,7; 4,8; 6,9|]; 6, [|8,5|]|]
    let gotos = Array.zeroCreate 10
    for i = 0 to 9 do
        gotos.[i] <- Array.create 9 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[||]; [|1,3|]; [|0,1|]; [|4,3|]; [|3,1|]; [|6,1|]; [|5,1|]|]
    let small_reduces =
        [|196610; 327681; 458753; 262146; 327682; 458754; 393219; 327683; 458755; 524291; 458755; 327684; 458756; 524292; 524291; 327685; 458757; 524293; 589827; 327686; 458758; 524294|]
    let reduces = Array.zeroCreate 10
    for i = 0 to 9 do
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
    let lists_zeroReduces = [|[||]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 10
    for i = 0 to 9 do
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
    let small_acc = [1]
    let accStates = Array.zeroCreate 10
    for i = 0 to 9 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let eofIndex = 7
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

let _rnglr_index = [|0; 1; 0; 0; 1; 0; 1|]
let _rnglr_stack_calls_ind = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_calls_node = new System.Collections.Generic.Stack<_>()
let _rnglr_stack_res = new ResizeArray<_>()
let _rnglr_rule_expr = Array.zeroCreate 2
let _rnglr_call_rule_expr = Array.zeroCreate 2
let _rnglr_res_count_expr = ref 0
let _rnglr_result_expr = ref <| Array.zeroCreate 0
let _rnglr_rule_fact = Array.zeroCreate 2
let _rnglr_call_rule_fact = Array.zeroCreate 2
let _rnglr_res_count_fact = ref 0
let _rnglr_result_fact = ref <| Array.zeroCreate 0
let _rnglr_rule_num = Array.zeroCreate 2
let _rnglr_call_rule_num = Array.zeroCreate 2
let _rnglr_res_count_num = ref 0
let _rnglr_result_num = ref <| Array.zeroCreate 0
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
let private _rnglr_translate_token = Array.zeroCreate 8
_rnglr_translate_token.[4] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal expr expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm expr can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_expr.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_expr.Value.[!num] <- res)
_rnglr_translate_token.[5] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal fact expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm fact can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_fact.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_fact.Value.[!num] <- res)
_rnglr_translate_token.[6] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal num expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    ( 
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm num can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_num.[_rnglr_index.[_rnglr_number]] _rnglr_children 
        )
        |> List.concat
      ) |> (fun res -> _rnglr_result_num.Value.[!num] <- res)
_rnglr_translate_token.[7] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
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
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal expr expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_expr
      incr _rnglr_res_count_expr;
      _rnglr_stack_calls_ind.Push(4)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm expr can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_expr.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[1] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal fact expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_fact
      incr _rnglr_res_count_fact;
      _rnglr_stack_calls_ind.Push(5)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm fact can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_fact.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[2] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal num expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_num
      incr _rnglr_res_count_num;
      _rnglr_stack_calls_ind.Push(6)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm num can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_num.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_translate_token.[3] <- fun (_rnglr_node : Yard.Generators.RNGLR.AST.AST<Token>) -> 
  match _rnglr_node with
  | Yard.Generators.RNGLR.AST.Term _ -> failwith "Nonterminal yard_start_rule expected, but terminal found" 
  | Yard.Generators.RNGLR.AST.NonTerm (_rnglr_multi_ast,num) ->
    if !num = -1 then
      num := !_rnglr_res_count_yard_start_rule
      incr _rnglr_res_count_yard_start_rule;
      _rnglr_stack_calls_ind.Push(7)
      _rnglr_stack_calls_node.Push(_rnglr_node)
      _rnglr_multi_ast.Value
      |> List.iter (
        fun (_rnglr_ast : Yard.Generators.RNGLR.AST.Child<Token>) -> 
          match _rnglr_ast with
          | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_call_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]] _rnglr_children
        )
_rnglr_call_rule_expr.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_expr.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    if !(_rnglr_treenum _rnglr_children.[2]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[2]))
    ()
_rnglr_call_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(0);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_fact.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(2);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    ()
_rnglr_call_rule_fact.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    if !(_rnglr_treenum _rnglr_children.[0]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[0]))
    if !(_rnglr_treenum _rnglr_children.[2]) = -1 then (_rnglr_stack_calls_ind.Push(1);_rnglr_stack_calls_node.Push(_rnglr_children.[2]))
    ()
_rnglr_call_rule_num.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    ()
_rnglr_call_rule_num.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<_>[]) ->
    ()
_rnglr_rule_expr.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_fact.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (f) -> 
        _rnglr_cycle_res := ( f )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
_rnglr_rule_expr.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_expr.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (a) -> 
        (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (ADD value) -> [value] | _-> failwith "Token ADD expected") 
         |> List.iter (fun (_rnglr_var_1) -> 
          _rnglr_result_expr.Value.[(_rnglr_treenum _rnglr_children.[2]).Value] 
           |> List.iter (fun (b) -> 
            _rnglr_cycle_res := ( a + b )::!_rnglr_cycle_res ) ) )
      !_rnglr_cycle_res
    )
_rnglr_rule_yard_start_rule.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    _rnglr_result_expr.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
_rnglr_rule_fact.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_num.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (n) -> 
        _rnglr_cycle_res := ( n )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
_rnglr_rule_fact.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      _rnglr_result_fact.Value.[(_rnglr_treenum _rnglr_children.[0]).Value] 
       |> List.iter (fun (a) -> 
        (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (MUL value) -> [value] | _-> failwith "Token MUL expected") 
         |> List.iter (fun (_rnglr_var_1) -> 
          _rnglr_result_fact.Value.[(_rnglr_treenum _rnglr_children.[2]).Value] 
           |> List.iter (fun (b) -> 
            _rnglr_cycle_res := ( a * b )::!_rnglr_cycle_res ) ) )
      !_rnglr_cycle_res
    )
_rnglr_rule_num.[0] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (B value) -> [value] | _-> failwith "Token B expected") 
       |> List.iter (fun (_rnglr_var_0) -> 
        _rnglr_cycle_res := ( 5 )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
_rnglr_rule_num.[1] <- 
  fun (_rnglr_children : Yard.Generators.RNGLR.AST.AST<Token>[]) ->  
    (
      let _rnglr_cycle_res = ref []
      (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (A value) -> [value] | _-> failwith "Token A expected") 
       |> List.iter (fun (_rnglr_var_0) -> 
        _rnglr_cycle_res := ( 3 )::!_rnglr_cycle_res )
      !_rnglr_cycle_res
    )
let translate node = 
  _rnglr_res_count_expr := 0
  _rnglr_res_count_fact := 0
  _rnglr_res_count_num := 0
  _rnglr_res_count_yard_start_rule := 0
  _rnglr_stack_calls_ind.Clear()
  _rnglr_stack_calls_node.Clear()
  _rnglr_stack_res.Clear()
  if !(_rnglr_treenum node) = -1 then (_rnglr_stack_calls_ind.Push(3);_rnglr_stack_calls_node.Push(node))
  while _rnglr_stack_calls_ind.Count > 0 do
    let i = _rnglr_stack_calls_ind.Pop()
    let x = _rnglr_stack_calls_node.Pop()
    if i < 4 then _rnglr_translate_token.[i] x
    else _rnglr_stack_res.Add(i,x)
  _rnglr_result_expr := Array.zeroCreate !_rnglr_res_count_expr
  _rnglr_result_fact := Array.zeroCreate !_rnglr_res_count_fact
  _rnglr_result_num := Array.zeroCreate !_rnglr_res_count_num
  _rnglr_result_yard_start_rule := Array.zeroCreate !_rnglr_res_count_yard_start_rule
  for (i,x) in _rnglr_stack_res do _rnglr_translate_token.[i] x
  _rnglr_result_yard_start_rule.Value.[!(_rnglr_treenum node)]
