module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | A of int
        | EOF of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 2,2|]; 2, [|0,3; 2,2|]|]
    let gotos = Array.zeroCreate 4
    for i = 0 to 3 do
        gotos.[i] <- Array.create 4 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [0,1]; [1,2]|]
    let small_reduces =
        [|131073; 196609; 196609; 196610|]
    let reduces = Array.zeroCreate 4
    for i = 0 to 3 do
        reduces.[i] <- Array.create 4 []
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
        zeroReduces.[i] <- Array.create 4 []
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
    let rules = [|2; 2; 0; 0|]
    let rulesStart = [|0; 1; 3|]
    let leftSide =
        [|0; 0; 1|]
    let startRule = 2
    let eofIndex = 3
    let tokenToNumber = function
        | A _ -> 2
        | EOF _ -> 3
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource

let _rnglr_index = [|0; 1; 0|]
let _rnglr_rule_s = Array.zeroCreate 2
let _rnglr_rule_yard_start_rule = Array.zeroCreate 1
let rec _rnglr_translate_token_s = 
   fun (_rnglr_multi_ast : Yard.Generators.RNGLR.AST.AST<Token> list ref) ->
    _rnglr_multi_ast.Value
    |> List.map (
      fun (_rnglr_ast : Yard.Generators.RNGLR.AST.AST<Token>) -> 
        match _rnglr_ast with
        | Yard.Generators.RNGLR.AST.Epsilon -> failwith "Nonterm s can't infer epsilon"
        | Yard.Generators.RNGLR.AST.Inner (_rnglr_number, _rnglr_children) -> _rnglr_rule_s.[_rnglr_index.[_rnglr_number]]  _rnglr_children
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
_rnglr_rule_s.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for _rnglr_var_0 in 
       (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (A value) -> [value] | _-> failwith "Token A expected") 
        do
        yield ( 1 )
    ]
  )
_rnglr_rule_s.[1] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    [
      for _rnglr_var_0 in 
       (match _rnglr_children.[0] with | Yard.Generators.RNGLR.AST.Term (A value) -> [value] | _-> failwith "Token A expected") 
        do
        for v in 
         (_rnglr_translate_token_s (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[1]))
          do
          yield ( 1 + v )
    ]
  )
_rnglr_rule_yard_start_rule.[0] <- (
   fun (_rnglr_children : Yard.Generators.RNGLR.AST.MultiAST<Token>[]) ->
    (_rnglr_translate_token_s (Yard.Generators.RNGLR.AST.getFamily _rnglr_children.[0]))
  )
let translate = 
  Yard.Generators.RNGLR.AST.getFamily >> _rnglr_translate_token_yard_start_rule
