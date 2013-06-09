
# 2 "Parser.fs"
module Calc.Parse
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.AbstractParser
//open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open QuickGraph

type Token =
    | DIV of string
    | EOF of string
    | LBRACE of string
    | MINUS of string
    | MULT of string
    | NUMBER of string
    | PLUS of string
    | POW of string
    | RBRACE of string

let numToString = function
    | 0 -> "expr"
    | 1 -> "factor"
    | 2 -> "factorOp"
    | 3 -> "powExpr"
    | 4 -> "powOp"
    | 5 -> "term"
    | 6 -> "termOp"
    | 7 -> "yard_many_1"
    | 8 -> "yard_many_2"
    | 9 -> "yard_many_3"
    | 10 -> "yard_rule_binExpr_1"
    | 11 -> "yard_rule_binExpr_2"
    | 12 -> "yard_rule_binExpr_3"
    | 13 -> "yard_start_rule"
    | 14 -> "DIV"
    | 15 -> "EOF"
    | 16 -> "LBRACE"
    | 17 -> "MINUS"
    | 18 -> "MULT"
    | 19 -> "NUMBER"
    | 20 -> "PLUS"
    | 21 -> "POW"
    | 22 -> "RBRACE"
    | _ -> ""
let tokenToNumber = function
    | DIV _ -> 14
    | EOF _ -> 15
    | LBRACE _ -> 16
    | MINUS _ -> 17
    | MULT _ -> 18
    | NUMBER _ -> 19
    | PLUS _ -> 20
    | POW _ -> 21
    | RBRACE _ -> 22

let mutable private cur = 0
let leftSide = [|10; 7; 7; 0; 13; 6; 6; 11; 8; 8; 5; 2; 2; 12; 9; 9; 1; 4; 3; 3|]
let private rules = [|5; 7; 6; 5; 7; 10; 0; 17; 20; 1; 8; 2; 1; 8; 11; 14; 18; 3; 9; 4; 3; 9; 12; 21; 16; 0; 22; 19|]
let private rulesStart = [|0; 2; 5; 5; 6; 7; 8; 9; 11; 14; 14; 15; 16; 17; 19; 22; 22; 23; 24; 27; 28|]
let startRule = 4

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 20; 28; 26; 18; 13; 16; 3; 19; 6; 7; 4; 5; 9; 17; 12; 10; 11; 14; 15; 21; 27; 24; 25; 22; 23|]
let private small_gotos =
                [|9; 0; 65537; 196610; 327683; 655364; 720901; 786438; 1048583; 1245192; 131076; 131081; 524298; 917515; 1179660; 196613; 65549; 196610; 786438; 1048583; 1245192; 262148; 131081; 524302; 917515; 1179660; 524291; 262159; 589840; 1376273; 589827; 196626; 1048583; 1245192; 655363; 262159; 589843; 1376273; 851977; 20; 65537; 196610; 327683; 655364; 720901; 786438; 1048583; 1245192; 917505; 1441813; 1310724; 393238; 458775; 1114136; 1310745; 1376263; 65537; 196610; 327706; 720901; 786438; 1048583; 1245192; 1441796; 393238; 458779; 1114136; 1310745|]
let gotos = Array.zeroCreate 29
for i = 0 to 28 do
        gotos.[i] <- Array.zeroCreate 23
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|7,1|]; [|8,2|]; [|8,3|]; [|11,1|]; [|12,1|]; [|13,1|]; [|14,2|]; [|14,3|]; [|17,1|]; [|18,3|]; [|19,1|]; [|13,2|]; [|16,1|]; [|7,2|]; [|0,1|]; [|1,2|]; [|1,3|]; [|5,1|]; [|6,1|]; [|10,1|]; [|0,2|]; [|3,1|]|]
let private small_reduces =
                [|131076; 983040; 1114112; 1310720; 1441792; 262148; 983041; 1114113; 1310721; 1441793; 327684; 983042; 1114114; 1310722; 1441794; 393218; 1048579; 1245187; 458754; 1048580; 1245188; 524294; 917509; 983045; 1114117; 1179653; 1310725; 1441797; 655366; 917510; 983046; 1114118; 1179654; 1310726; 1441798; 720902; 917511; 983047; 1114119; 1179655; 1310727; 1441799; 786434; 1048584; 1245192; 983047; 917513; 983049; 1114121; 1179657; 1310729; 1376265; 1441801; 1048583; 917514; 983050; 1114122; 1179658; 1310730; 1376266; 1441802; 1114118; 917515; 983051; 1114123; 1179659; 1310731; 1441803; 1179654; 917516; 983052; 1114124; 1179660; 1310732; 1441804; 1245188; 983053; 1114125; 1310733; 1441805; 1310722; 983054; 1441806; 1441794; 983055; 1441807; 1507330; 983056; 1441808; 1572866; 1048593; 1245201; 1638402; 1048594; 1245202; 1703940; 983059; 1114131; 1310739; 1441811; 1769474; 983060; 1441812; 1835010; 983061; 1441813|]
let reduces = Array.zeroCreate 29
for i = 0 to 28 do
        reduces.[i] <- Array.zeroCreate 23
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|9|]; [|15|]; [|2|]|]
let private small_zeroReduces =
                [|131076; 983040; 1114112; 1310720; 1441792; 262148; 983040; 1114112; 1310720; 1441792; 524294; 917505; 983041; 1114113; 1179649; 1310721; 1441793; 655366; 917505; 983041; 1114113; 1179649; 1310721; 1441793; 1310722; 983042; 1441794; 1441794; 983042; 1441794|]
let zeroReduces = Array.zeroCreate 29
for i = 0 to 28 do
        zeroReduces.[i] <- Array.zeroCreate 23
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 29
for i = 0 to 28 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 15
let private parserSource = new ParserSource<'TokenType> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (AdjacencyGraph<int, TaggedEdge<_,'TokenType>> -> ParseResult<Token>) = buildAst<'TokenType> parserSource
//let buildAst : (seq<Token> -> ParseResult<Token>) = buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_many_1 * '_rnglr_type_yard_many_2 * '_rnglr_type_yard_many_3 * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_2 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_1) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 3 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_1) 
# 149 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_termOp) 
               |> List.iter (fun (op) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_term) 
                 |> List.iter (fun (r) -> 
                  _rnglr_cycle_res := (
                    
# 2 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_many_1) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 6 "calc.yrd"
                                          yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "calc.yrd"
               : '_rnglr_type_yard_many_1) 
# 182 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 6 "calc.yrd"
                                      []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 6 "calc.yrd"
               : '_rnglr_type_yard_many_1) 
# 200 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_1) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 6 "calc.yrd"
                                                   res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "calc.yrd"
               : '_rnglr_type_expr) 
# 220 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 6 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 230 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwith "MINUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 8 "calc.yrd"
                                               (-) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 8 "calc.yrd"
               : '_rnglr_type_termOp) 
# 250 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 8 "calc.yrd"
                               (+) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 8 "calc.yrd"
               : '_rnglr_type_termOp) 
# 270 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_2) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 3 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_2) 
# 292 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_factorOp) 
               |> List.iter (fun (op) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_factor) 
                 |> List.iter (fun (r) -> 
                  _rnglr_cycle_res := (
                    
# 2 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_many_2) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 10 "calc.yrd"
                                            yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 10 "calc.yrd"
               : '_rnglr_type_yard_many_2) 
# 325 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 10 "calc.yrd"
                                        []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 10 "calc.yrd"
               : '_rnglr_type_yard_many_2) 
# 343 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_2) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 10 "calc.yrd"
                                                       res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 10 "calc.yrd"
               : '_rnglr_type_term) 
# 363 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIV _rnglr_val -> [_rnglr_val] | a -> failwith "DIV expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 12 "calc.yrd"
                                                 (/) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 383 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MULT _rnglr_val -> [_rnglr_val] | a -> failwith "MULT expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 12 "calc.yrd"
                                 ( * ) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 403 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_3) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 3 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_3) 
# 425 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_powOp) 
               |> List.iter (fun (op) -> 
                ((unbox _rnglr_children.[1]) : '_rnglr_type_powExpr) 
                 |> List.iter (fun (r) -> 
                  _rnglr_cycle_res := (
                    
# 2 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_many_3) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 14 "calc.yrd"
                                               yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_many_3) 
# 458 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 14 "calc.yrd"
                                           []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_many_3) 
# 476 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_3) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 14 "calc.yrd"
                                                       res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_factor) 
# 496 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with POW _rnglr_val -> [_rnglr_val] | a -> failwith "POW expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 16 "calc.yrd"
                             ( ** ) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 16 "calc.yrd"
               : '_rnglr_type_powOp) 
# 516 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_expr) 
               |> List.iter (fun (e) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 18 "calc.yrd"
                                                                                         e 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 540 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwith "NUMBER expected, but %A found" a )
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 18 "calc.yrd"
                                    System.Double.Parse n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 18 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 560 "Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factor)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_factorOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powExpr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_powOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_term)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_termOp)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_1)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_2)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_3)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
