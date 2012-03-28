module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
        | A of int
        | ADD of int
        | B of int
        | EOF of int
        | MUL of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|1,1; 2,4; 3,7; 5,8; 7,9|]; 1, [|6,2|]; 2, [|1,3; 2,4; 3,7; 5,8; 7,9|]; 3, [|6,2|]; 4, [|9,5|]; 5, [|2,6; 3,7; 5,8; 7,9|]; 6, [|9,5|]|]
    let gotos = Array.zeroCreate 10
    for i = 0 to 9 do
        gotos.[i] <- Array.create 10 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [1,3]; [0,1]; [4,3]; [3,1]; [6,1]; [5,1]|]
    let small_reduces =
        [|196610; 393217; 524289; 262146; 393218; 524290; 393219; 393219; 524291; 589827; 458755; 393220; 524292; 589828; 524291; 393221; 524293; 589829; 589827; 393222; 524294; 589830|]
    let reduces = Array.zeroCreate 10
    for i = 0 to 9 do
        reduces.[i] <- Array.create 10 []
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
        zeroReduces.[i] <- Array.create 10 []
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
    let rules = [|2; 1; 6; 1; 1; 3; 2; 9; 2; 7; 5|]
    let rulesStart = [|0; 1; 4; 5; 6; 9; 10|]
    let leftSide =
        [|1; 1; 4; 2; 2; 3; 3|]
    let startRule = 2
    let eofIndex = 8
    let tokenToNumber = function
        | A _ -> 5
        | ADD _ -> 6
        | B _ -> 7
        | EOF _ -> 8
        | MUL _ -> 9
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource

let translate (_rnglr_tokens_init : seq<Token>) = 
  let _rnglr_tokens = Array.ofSeq _rnglr_tokens_init
  let _rnglr_index = [|0; 1; 0; 0; 1; 0; 1|]
  let _rnglr_rule_error = Array.zeroCreate 0
  let _rnglr_rule_expr = Array.zeroCreate 2
  let _rnglr_rule_fact = Array.zeroCreate 2
  let _rnglr_rule_num = Array.zeroCreate 2
  let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
  let rec _rnglr_translate_token_error = 
     fun (_rnglr_multi_ast : AST<_> list ref) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST<_>) -> 
          match _rnglr_ast with
          | Epsilon -> failwith "Nonterm error can't infer epsilon"
          | Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_error.[_rnglr_index.[_rnglr_number]]  _rnglr_children
        )
        |> List.concat
  let rec _rnglr_translate_token_expr = 
     fun (_rnglr_multi_ast : AST<_> list ref) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST<_>) -> 
          match _rnglr_ast with
          | Epsilon -> failwith "Nonterm expr can't infer epsilon"
          | Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_expr.[_rnglr_index.[_rnglr_number]]  _rnglr_children
        )
        |> List.concat
  let rec _rnglr_translate_token_fact = 
     fun (_rnglr_multi_ast : AST<_> list ref) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST<_>) -> 
          match _rnglr_ast with
          | Epsilon -> failwith "Nonterm fact can't infer epsilon"
          | Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_fact.[_rnglr_index.[_rnglr_number]]  _rnglr_children
        )
        |> List.concat
  let rec _rnglr_translate_token_num = 
     fun (_rnglr_multi_ast : AST<_> list ref) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST<_>) -> 
          match _rnglr_ast with
          | Epsilon -> failwith "Nonterm num can't infer epsilon"
          | Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_num.[_rnglr_index.[_rnglr_number]]  _rnglr_children
        )
        |> List.concat
  let rec _rnglr_translate_token_yard_start_rule = 
     fun (_rnglr_multi_ast : AST<_> list ref) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST<_>) -> 
          match _rnglr_ast with
          | Epsilon -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_yard_start_rule.[_rnglr_index.[_rnglr_number]]  _rnglr_children
        )
        |> List.concat
  _rnglr_rule_expr.[0] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for f in  (_rnglr_translate_token_fact (getFamily _rnglr_children.[0]))
          do
          yield ( f )
      ]
    )
  _rnglr_rule_expr.[1] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for a in  (_rnglr_translate_token_expr (getFamily _rnglr_children.[0]))
          do
          for _rnglr_var_1 in 
           (match _rnglr_children.[1] with | Term (ADD value) -> [value] | _-> failwith "Token ADD expected") 
            do
            for b in 
             (_rnglr_translate_token_expr (getFamily _rnglr_children.[2]))  do
              yield ( a + b )
      ]
    )
  _rnglr_rule_yard_start_rule.[0] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      (_rnglr_translate_token_expr (getFamily _rnglr_children.[0]))
    )
  _rnglr_rule_fact.[0] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for n in  (_rnglr_translate_token_num (getFamily _rnglr_children.[0]))
          do
          yield ( n )
      ]
    )
  _rnglr_rule_fact.[1] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for a in  (_rnglr_translate_token_fact (getFamily _rnglr_children.[0]))
          do
          for _rnglr_var_1 in 
           (match _rnglr_children.[1] with | Term (MUL value) -> [value] | _-> failwith "Token MUL expected") 
            do
            for b in 
             (_rnglr_translate_token_fact (getFamily _rnglr_children.[2]))  do
              yield ( a * b )
      ]
    )
  _rnglr_rule_num.[0] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for _rnglr_var_0 in 
         (match _rnglr_children.[0] with | Term (B value) -> [value] | _-> failwith "Token B expected") 
          do
          yield ( 5 )
      ]
    )
  _rnglr_rule_num.[1] <- (
     fun (_rnglr_children : MultiAST<_>[]) ->
      [
        for _rnglr_var_0 in 
         (match _rnglr_children.[0] with | Term (A value) -> [value] | _-> failwith "Token A expected") 
          do
          yield ( 3 )
      ]
    )
  getFamily >> _rnglr_translate_token_yard_start_rule
