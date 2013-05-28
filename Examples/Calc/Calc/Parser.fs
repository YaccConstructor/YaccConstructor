
# 2 "Parser.fs"
module Calc.Parse
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | DIV of string
    | LBRACE of string
    | MINUS of string
    | MULT of string
    | NUMBER of string
    | PLUS of string
    | POW of string
    | RBRACE of string
    | RNGLR_EOF of string

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "factor"
    | 3 -> "factorOp"
    | 4 -> "powExpr"
    | 5 -> "powOp"
    | 6 -> "term"
    | 7 -> "termOp"
    | 8 -> "yard_many_1"
    | 9 -> "yard_many_2"
    | 10 -> "yard_many_3"
    | 11 -> "yard_rule_binExpr_1"
    | 12 -> "yard_rule_binExpr_2"
    | 13 -> "yard_rule_binExpr_3"
    | 14 -> "yard_start_rule"
    | 15 -> "DIV"
    | 16 -> "LBRACE"
    | 17 -> "MINUS"
    | 18 -> "MULT"
    | 19 -> "NUMBER"
    | 20 -> "PLUS"
    | 21 -> "POW"
    | 22 -> "RBRACE"
    | 23 -> "RNGLR_EOF"
    | _ -> ""
let tokenToNumber = function
    | DIV _ -> 15
    | LBRACE _ -> 16
    | MINUS _ -> 17
    | MULT _ -> 18
    | NUMBER _ -> 19
    | PLUS _ -> 20
    | POW _ -> 21
    | RBRACE _ -> 22
    | RNGLR_EOF _ -> 23

let mutable private cur = 0
let leftSide = [|1; 14; 11; 8; 8; 7; 7; 6; 12; 9; 9; 3; 3; 2; 13; 10; 10; 5; 4; 4|]
let private rules = [|11; 1; 6; 8; 7; 6; 8; 17; 20; 12; 2; 9; 3; 2; 9; 15; 18; 13; 4; 10; 5; 4; 10; 21; 16; 1; 22; 19|]
let private rulesStart = [|0; 1; 2; 4; 7; 7; 8; 9; 10; 12; 15; 15; 16; 17; 18; 20; 23; 23; 24; 27; 28|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 20; 28; 26; 18; 13; 16; 3; 19; 6; 7; 4; 5; 9; 17; 12; 10; 11; 14; 15; 21; 27; 24; 25; 22; 23|]
let private small_gotos =
        [|9; 65536; 131073; 262146; 393219; 720900; 786437; 851974; 1048583; 1245192; 131076; 196617; 589834; 983051; 1179660; 196613; 131085; 262146; 851974; 1048583; 1245192; 262148; 196617; 589838; 983051; 1179660; 524291; 327695; 655376; 1376273; 589827; 262162; 1048583; 1245192; 655363; 327695; 655379; 1376273; 851977; 65556; 131073; 262146; 393219; 720900; 786437; 851974; 1048583; 1245192; 917505; 1441813; 1310724; 458774; 524311; 1114136; 1310745; 1376263; 131073; 262146; 393242; 786437; 851974; 1048583; 1245192; 1441796; 458774; 524315; 1114136; 1310745|]
let gotos = Array.zeroCreate 29
for i = 0 to 28 do
        gotos.[i] <- Array.zeroCreate 24
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
let private lists_reduces = [|[|8,1|]; [|9,2|]; [|9,3|]; [|11,1|]; [|12,1|]; [|14,1|]; [|15,2|]; [|15,3|]; [|17,1|]; [|18,3|]; [|19,1|]; [|14,2|]; [|13,1|]; [|8,2|]; [|2,1|]; [|3,2|]; [|3,3|]; [|5,1|]; [|6,1|]; [|7,1|]; [|2,2|]; [|0,1|]|]
let private small_reduces =
        [|131076; 1114112; 1310720; 1441792; 1507328; 262148; 1114113; 1310721; 1441793; 1507329; 327684; 1114114; 1310722; 1441794; 1507330; 393218; 1048579; 1245187; 458754; 1048580; 1245188; 524294; 983045; 1114117; 1179653; 1310725; 1441797; 1507333; 655366; 983046; 1114118; 1179654; 1310726; 1441798; 1507334; 720902; 983047; 1114119; 1179655; 1310727; 1441799; 1507335; 786434; 1048584; 1245192; 983047; 983049; 1114121; 1179657; 1310729; 1376265; 1441801; 1507337; 1048583; 983050; 1114122; 1179658; 1310730; 1376266; 1441802; 1507338; 1114118; 983051; 1114123; 1179659; 1310731; 1441803; 1507339; 1179654; 983052; 1114124; 1179660; 1310732; 1441804; 1507340; 1245188; 1114125; 1310733; 1441805; 1507341; 1310722; 1441806; 1507342; 1441794; 1441807; 1507343; 1507330; 1441808; 1507344; 1572866; 1048593; 1245201; 1638402; 1048594; 1245202; 1703940; 1114131; 1310739; 1441811; 1507347; 1769474; 1441812; 1507348; 1835010; 1441813; 1507349|]
let reduces = Array.zeroCreate 29
for i = 0 to 28 do
        reduces.[i] <- Array.zeroCreate 24
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
let private lists_zeroReduces = [|[|10|]; [|16|]; [|4|]|]
let private small_zeroReduces =
        [|131076; 1114112; 1310720; 1441792; 1507328; 262148; 1114112; 1310720; 1441792; 1507328; 524294; 983041; 1114113; 1179649; 1310721; 1441793; 1507329; 655366; 983041; 1114113; 1179649; 1310721; 1441793; 1507329; 1310722; 1441794; 1507330; 1441794; 1441794; 1507330|]
let zeroReduces = Array.zeroCreate 29
for i = 0 to 28 do
        zeroReduces.[i] <- Array.zeroCreate 24
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
let eofIndex = 23
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_many_1 * '_rnglr_type_yard_many_2 * '_rnglr_type_yard_many_3 * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_2 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_start_rule>), 
  [|
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
# 150 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 6 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 160 "Parser.fs"
      );
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
# 182 "Parser.fs"
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
# 215 "Parser.fs"
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
# 233 "Parser.fs"
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
# 253 "Parser.fs"
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
# 273 "Parser.fs"
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
# 293 "Parser.fs"
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
# 315 "Parser.fs"
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
# 348 "Parser.fs"
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
# 366 "Parser.fs"
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
# 386 "Parser.fs"
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
# 406 "Parser.fs"
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
# 426 "Parser.fs"
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
# 448 "Parser.fs"
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
# 481 "Parser.fs"
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
# 499 "Parser.fs"
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
# 519 "Parser.fs"
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
# 543 "Parser.fs"
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
# 563 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Microsoft.FSharp.Text.Lexing.Position * Microsoft.FSharp.Text.Lexing.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 581 "Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
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
