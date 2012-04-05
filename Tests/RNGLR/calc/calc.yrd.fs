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
    | _ -> ""
let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | EOF _ -> 7
    | MUL _ -> 8

let leftSide = [|0; 0; 3; 1; 1; 2; 2|]

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,4; 2,7; 4,8; 6,9|]; 1, [|5,2|]; 2, [|0,3; 1,4; 2,7; 4,8; 6,9|]; 3, [|5,2|]; 4, [|8,5|]; 5, [|1,6; 2,7; 4,8; 6,9|]; 6, [|8,5|]|]
    let gotos = Array.zeroCreate 10
    for i = 0 to 9 do
        gotos.[i] <- Array.create 9 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [1,3]; [0,1]; [4,3]; [3,1]; [6,1]; [5,1]|]
    let small_reduces =
        [|196610; 327681; 458753; 262146; 327682; 458754; 393219; 327683; 458755; 524291; 458755; 327684; 458756; 524292; 524291; 327685; 458757; 524293; 589827; 327686; 458758; 524294|]
    let reduces = Array.zeroCreate 10
    for i = 0 to 9 do
        reduces.[i] <- Array.create 9 []
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
    let zeroReduces = Array.zeroCreate 10
    for i = 0 to 9 do
        zeroReduces.[i] <- Array.create 9 []
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
    let rules = [|1; 0; 5; 0; 0; 2; 1; 8; 1; 6; 4|]
    let rulesStart = [|0; 1; 4; 5; 6; 9; 10|]
    let startRule = 2
    let eofIndex = 7
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource

let _rnglr_index = [|0; 1; 0; 0; 1; 0; 1|]
let _rnglr_rule_expr = Array.zeroCreate 2
let _rnglr_rule_fact = Array.zeroCreate 2
let _rnglr_rule_num = Array.zeroCreate 2
let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
let rec _rnglr_translate_token_expr = 
   fun (_rnglr_multi_ast : Yard.Generators.RNGLR.AST.AST<Token> list ref) ->
    _rnglr_multi_ast.Value
    |> List.map (
      fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
        match _rnglr_ast with
        | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm expr can't infer epsilon"
        | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_expr.[_rnglr_index.[_rnglr_number]]  _rnglr_children
      )
      |> List.concat
let rec _rnglr_translate_token_fact = 
   fun (_rnglr_multi_ast : Yard.Generators.RNGLR.AST.AST<Token> list ref) ->
    _rnglr_multi_ast.Value
    |> List.map (
      fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
        match _rnglr_ast with
        | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm fact can't infer epsilon"
        | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_fact.[_rnglr_index.[_rnglr_number]]  _rnglr_children
      )
      |> List.concat
let rec _rnglr_translate_token_num = 
   fun (_rnglr_multi_ast : Yard.Generators.RNGLR.AST.AST<Token> list ref) ->
    _rnglr_multi_ast.Value
    |> List.map (
      fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
        match _rnglr_ast with
        | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm num can't infer epsilon"
        | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_num.[_rnglr_index.[_rnglr_number]]  _rnglr_children
      )
      |> List.concat
let rec _rnglr_translate_token_yard_start_rule = 
   fun (_rnglr_multi_ast : Yard.Generators.RNGLR.AST.AST<Token> list ref) ->
    _rnglr_multi_ast.Value
    |> List.map (
      fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
        match _rnglr_ast with
        | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
        | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]]  _rnglr_children
      )
      |> List.concat
_rnglr_rule_expr.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for f in 
       (_rnglr_translate_token_fact (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
        do
        yield ( f )
    ]
  )
_rnglr_rule_expr.[1] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for a in 
       (_rnglr_translate_token_expr (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
        do
        for _rnglr_var_1 in 
         (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (ADD value) -> [value] | _-> failwith "Token ADD expected") 
          do
          for b in 
           (_rnglr_translate_token_expr (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[2]))
            do
            yield ( a + b )
    ]
  )
_rnglr_rule_yard_start_rule.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    (_rnglr_translate_token_expr (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
  )
_rnglr_rule_fact.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for n in 
       (_rnglr_translate_token_num (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
        do
        yield ( n )
    ]
  )
_rnglr_rule_fact.[1] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for a in 
       (_rnglr_translate_token_fact (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
        do
        for _rnglr_var_1 in 
         (match _rnglr_children.[1] with | Yard.Generators.RNGLR.AST.Term (MUL value) -> [value] | _-> failwith "Token MUL expected") 
          do
          for b in 
           (_rnglr_translate_token_fact (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[2]))
            do
            yield ( a * b )
    ]
  )
_rnglr_rule_num.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for _rnglr_var_0 in 
       (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (B value) -> [value] | _-> failwith "Token B expected") 
        do
        yield ( 5 )
    ]
  )
_rnglr_rule_num.[1] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for _rnglr_var_0 in 
       (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (A value) -> [value] | _-> failwith "Token A expected") 
        do
        yield ( 3 )
    ]
  )
let translate = 
  Yard.Generators.RNGLR.AST.getFamily >> _rnglr_translate_token_yard_start_rule
