module RNGLR.ParseCounter
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | A of int
        | EOF of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|1,1; 3,2|]; 2, [|1,3; 3,2|]|]
    let gotos = Array.zeroCreate 4
    for i = 0 to 3 do
        gotos.[i] <- Array.create 5 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [0,1]; [1,2]|]
    let small_reduces =
        [|131073; 262145; 196609; 262146|]
    let reduces = Array.zeroCreate 4
    for i = 0 to 3 do
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
    let zeroReduces = Array.zeroCreate 4
    for i = 0 to 3 do
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
    let accStates = Array.zeroCreate 4
    for i = 0 to 3 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let rules = [|3; 3; 1; 1|]
    let rulesStart = [|0; 1; 3|]
    let leftSide =
        [|1; 1; 2|]
    let startRule = 2
    let eofIndex = 4
    let tokenToNumber = function
        | A _ -> 3
        | EOF _ -> 4
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource

let translate (_rnglr_tokens_init : seq<Token>) = 
  let _rnglr_tokens = Array.ofSeq _rnglr_tokens_init
  let _rnglr_index = [|0; 1; 0|]
  let _rnglr_rule_error = Array.zeroCreate 0
  let _rnglr_rule_s = Array.zeroCreate 2
  let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
  let rec _rnglr_translate_token_error = 
     fun (_rnglr_multi_ast : MultiAST) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST) -> 
          match _rnglr_ast.nodeType with
          | EpsTree -> failwith "Nonterm error can't infer epsilon"
          | Term -> failwithf "Expected nonTerm error expansion, but token %A found" 0
          | NonTerm -> _rnglr_rule_error.[_rnglr_index.[_rnglr_ast.number]]  _rnglr_ast.children
        )
        |> List.concat
  let rec _rnglr_translate_token_s = 
     fun (_rnglr_multi_ast : MultiAST) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST) -> 
          match _rnglr_ast.nodeType with
          | EpsTree -> failwith "Nonterm s can't infer epsilon"
          | Term -> failwithf "Expected nonTerm s expansion, but token %A found" 0
          | NonTerm -> _rnglr_rule_s.[_rnglr_index.[_rnglr_ast.number]]  _rnglr_ast.children
        )
        |> List.concat
  let rec _rnglr_translate_token_yard_start_rule = 
     fun (_rnglr_multi_ast : MultiAST) ->
      _rnglr_multi_ast.Value
      |> List.map (
        fun (_rnglr_ast : AST) -> 
          match _rnglr_ast.nodeType with
          | EpsTree -> failwith "Nonterm yard_start_rule can't infer epsilon"
          | Term -> failwithf "Expected nonTerm yard_start_rule expansion, but token %A found" 0
          | NonTerm ->
                let x = _rnglr_index.[_rnglr_ast.number]
                printfn "%A" x
                _rnglr_rule_yard_start_rule.[x]  _rnglr_ast.children
        )
        |> List.concat
  _rnglr_rule_s.[0] <- (
     fun (_rnglr_children : MultiAST[]) ->
      [
        for _rnglr_var_0 in 
         (match _rnglr_tokens.[_rnglr_children.[0].Value.[0].number] with | A value -> [value] | _-> failwith "Token A expected") 
          do
          yield
            (1)
      ]
    )
  _rnglr_rule_s.[1] <- (
     fun (_rnglr_children : MultiAST[]) ->
      [
        for _rnglr_var_0 in 
         (match _rnglr_tokens.[_rnglr_children.[0].Value.[0].number] with | A value -> [value] | _-> failwith "Token A expected") 
          do
          for v in  (_rnglr_translate_token_s _rnglr_children.[1])  do
            yield
              (1 + v)
      ]
    )
  _rnglr_rule_yard_start_rule.[0] <- (
     fun (_rnglr_children : MultiAST[]) ->
      (_rnglr_translate_token_s _rnglr_children.[0])
    )
  _rnglr_translate_token_yard_start_rule

