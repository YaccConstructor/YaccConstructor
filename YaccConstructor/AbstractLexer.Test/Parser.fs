
# 2 "Parser.fs"
module AbstractLexer.Test.Calc.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | DIV of (string)
    | LBRACE of (string)
    | MINUS of (string)
    | MULT of (string)
    | NUMBER of (string)
    | PLUS of (string)
    | POW of (string)
    | RBRACE of (string)
    | RNGLR_EOF of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | DIV x -> box x
    | LBRACE x -> box x
    | MINUS x -> box x
    | MULT x -> box x
    | NUMBER x -> box x
    | PLUS x -> box x
    | POW x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "factor"
    | 3 -> "factorOp"
    | 4 -> "powExpr"
    | 5 -> "powOp"
    | 6 -> "term"
    | 7 -> "termOp"
    | 8 -> "yard_rule_binExpr_1"
    | 9 -> "yard_rule_binExpr_3"
    | 10 -> "yard_rule_binExpr_5"
    | 11 -> "yard_rule_yard_many_1_2"
    | 12 -> "yard_rule_yard_many_1_4"
    | 13 -> "yard_rule_yard_many_1_6"
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

let isLiteral = function
    | DIV _ -> false
    | LBRACE _ -> false
    | MINUS _ -> false
    | MULT _ -> false
    | NUMBER _ -> false
    | PLUS _ -> false
    | POW _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 14; 8; 11; 11; 7; 7; 6; 9; 12; 12; 3; 3; 2; 10; 13; 13; 5; 4; 4|]
let private rules = [|8; 1; 6; 11; 7; 6; 11; 17; 20; 9; 2; 12; 3; 2; 12; 15; 18; 10; 4; 13; 5; 4; 13; 21; 16; 1; 22; 19|]
let private rulesStart = [|0; 1; 2; 4; 7; 7; 8; 9; 10; 12; 15; 15; 16; 17; 18; 20; 23; 23; 24; 27; 28|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 20; 28; 26; 18; 13; 16; 3; 19; 6; 7; 4; 5; 9; 17; 12; 10; 11; 14; 15; 21; 27; 24; 25; 22; 23|]
let private small_gotos =
        [|9; 65536; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1245192; 131076; 196617; 786442; 983051; 1179660; 196613; 131085; 262146; 655366; 1048583; 1245192; 262148; 196617; 786446; 983051; 1179660; 524291; 327695; 851984; 1376273; 589827; 262162; 1048583; 1245192; 655363; 327695; 851987; 1376273; 851977; 65556; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1245192; 917505; 1441813; 1310724; 458774; 720919; 1114136; 1310745; 1376263; 131073; 262146; 393242; 589829; 655366; 1048583; 1245192; 1441796; 458774; 720923; 1114136; 1310745|]
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

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(16, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_rule_binExpr_5 * '_rnglr_type_yard_rule_yard_many_1_2 * '_rnglr_type_yard_rule_yard_many_1_4 * '_rnglr_type_yard_rule_yard_many_1_6 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_1) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 19 "calc.yrd"
                                                 res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_expr) 
# 177 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 187 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_2) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 16 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_1) 
# 209 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
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
                    
# 15 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_2) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 15 "calc.yrd"
                                    yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 242 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 15 "calc.yrd"
                                []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_2) 
# 260 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwith "MINUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 21 "calc.yrd"
                                               (-) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 21 "calc.yrd"
               : '_rnglr_type_termOp) 
# 280 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 21 "calc.yrd"
                               (+) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 21 "calc.yrd"
               : '_rnglr_type_termOp) 
# 300 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_3) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 23 "calc.yrd"
                                                     res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "calc.yrd"
               : '_rnglr_type_term) 
# 320 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_4) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 16 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_3) 
# 342 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
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
                    
# 15 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_4) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 15 "calc.yrd"
                                    yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 375 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 15 "calc.yrd"
                                []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_4) 
# 393 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIV _rnglr_val -> [_rnglr_val] | a -> failwith "DIV expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 25 "calc.yrd"
                                                 (/) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 413 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MULT _rnglr_val -> [_rnglr_val] | a -> failwith "MULT expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 25 "calc.yrd"
                                 ( * ) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "calc.yrd"
               : '_rnglr_type_factorOp) 
# 433 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_5) 
             |> List.iter (fun (res) -> 
              _rnglr_cycle_res := (
                
# 27 "calc.yrd"
                                                     res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_factor) 
# 453 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_yard_many_1_6) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 16 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_5) 
# 475 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
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
                    
# 15 "calc.yrd"
                                                        op,r 
                      )::!_rnglr_cycle_res ) )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_yard_many_1_6) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 15 "calc.yrd"
                                    yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 508 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 15 "calc.yrd"
                                []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 15 "calc.yrd"
               : '_rnglr_type_yard_rule_yard_many_1_6) 
# 526 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with POW _rnglr_val -> [_rnglr_val] | a -> failwith "POW expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 29 "calc.yrd"
                             ( ** ) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "calc.yrd"
               : '_rnglr_type_powOp) 
# 546 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
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
                    
# 31 "calc.yrd"
                                                                                         e 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 570 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwith "NUMBER expected, but %A found" a )
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 31 "calc.yrd"
                                    System.Double.Parse n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "calc.yrd"
               : '_rnglr_type_powExpr) 
# 590 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
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
# 608 "Parser.fs"
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
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_binExpr_5)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_2)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_4)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_yard_many_1_6)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
