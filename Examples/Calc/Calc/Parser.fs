
# 2 "Parser.fs"
module Calc.Parser
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
    | 8 -> "yard_exp_brackets_4"
    | 9 -> "yard_exp_brackets_5"
    | 10 -> "yard_exp_brackets_6"
    | 11 -> "yard_many_1"
    | 12 -> "yard_many_2"
    | 13 -> "yard_many_3"
    | 14 -> "yard_rule_binExpr_1"
    | 15 -> "yard_rule_binExpr_2"
    | 16 -> "yard_rule_binExpr_3"
    | 17 -> "yard_start_rule"
    | 18 -> "DIV"
    | 19 -> "LBRACE"
    | 20 -> "MINUS"
    | 21 -> "MULT"
    | 22 -> "NUMBER"
    | 23 -> "PLUS"
    | 24 -> "POW"
    | 25 -> "RBRACE"
    | 26 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 18
    | LBRACE _ -> 19
    | MINUS _ -> 20
    | MULT _ -> 21
    | NUMBER _ -> 22
    | PLUS _ -> 23
    | POW _ -> 24
    | RBRACE _ -> 25
    | RNGLR_EOF _ -> 26

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
let leftSide = [|10; 9; 8; 4; 4; 5; 13; 13; 16; 2; 3; 3; 12; 12; 15; 6; 7; 7; 11; 11; 14; 1; 17|]
let private rules = [|5; 4; 3; 2; 7; 6; 22; 19; 1; 25; 24; 10; 13; 4; 13; 16; 21; 18; 9; 12; 2; 12; 15; 23; 20; 8; 11; 6; 11; 14; 1|]
let private rulesStart = [|0; 2; 4; 6; 7; 10; 11; 11; 13; 15; 16; 17; 18; 18; 20; 22; 23; 24; 25; 25; 27; 29; 30; 31|]
let startRule = 22

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 5; 22; 31; 25; 16; 8; 11; 3; 17; 21; 19; 20; 4; 6; 12; 15; 14; 7; 9; 10; 13; 18; 23; 26; 30; 28; 29; 24; 27|]
let private small_gotos =
        [|9; 65536; 131073; 262146; 393219; 917508; 983045; 1048582; 1245191; 1441800; 131077; 196617; 589834; 786443; 1179660; 1376269; 196613; 131086; 262146; 1048582; 1245191; 1441800; 327684; 327695; 655376; 851985; 1572882; 393219; 262163; 1245191; 1441800; 524297; 65556; 131073; 262146; 393219; 917508; 983045; 1048582; 1245191; 1441800; 589825; 1638421; 786436; 327695; 655376; 851990; 1572882; 1114117; 196617; 589834; 786455; 1179660; 1376269; 1441797; 458776; 524313; 720922; 1310747; 1507356; 1507335; 131073; 262146; 393245; 983045; 1048582; 1245191; 1441800; 1703941; 458776; 524313; 720926; 1310747; 1507356|]
let gotos = Array.zeroCreate 32
for i = 0 to 31 do
        gotos.[i] <- Array.zeroCreate 27
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
let private lists_reduces = [|[|14,1|]; [|1,2|]; [|8,1|]; [|0,2|]; [|4,3|]; [|3,1|]; [|7,1|]; [|7,2|]; [|5,1|]; [|8,2|]; [|9,1|]; [|13,1|]; [|13,2|]; [|11,1|]; [|10,1|]; [|14,2|]; [|20,1|]; [|2,2|]; [|15,1|]; [|19,1|]; [|19,2|]; [|17,1|]; [|16,1|]; [|20,2|]; [|21,1|]|]
let private small_reduces =
        [|131076; 1310720; 1507328; 1638400; 1703936; 262150; 1179649; 1310721; 1376257; 1507329; 1638401; 1703937; 327686; 1179650; 1310722; 1376258; 1507330; 1638402; 1703938; 458759; 1179651; 1310723; 1376259; 1507331; 1572867; 1638403; 1703939; 655367; 1179652; 1310724; 1376260; 1507332; 1572868; 1638404; 1703940; 720903; 1179653; 1310725; 1376261; 1507333; 1572869; 1638405; 1703941; 786438; 1179654; 1310726; 1376262; 1507334; 1638406; 1703942; 851974; 1179655; 1310727; 1376263; 1507335; 1638407; 1703943; 917506; 1245192; 1441800; 983046; 1179657; 1310729; 1376265; 1507337; 1638409; 1703945; 1048582; 1179658; 1310730; 1376266; 1507338; 1638410; 1703946; 1114116; 1310731; 1507339; 1638411; 1703947; 1179652; 1310732; 1507340; 1638412; 1703948; 1245186; 1245197; 1441805; 1310722; 1245198; 1441806; 1376260; 1310735; 1507343; 1638415; 1703951; 1441794; 1638416; 1703952; 1572868; 1310737; 1507345; 1638417; 1703953; 1638404; 1310738; 1507346; 1638418; 1703954; 1703938; 1638419; 1703955; 1769474; 1638420; 1703956; 1835010; 1245205; 1441813; 1900546; 1245206; 1441814; 1966082; 1638423; 1703959; 2031618; 1638424; 1703960|]
let reduces = Array.zeroCreate 32
for i = 0 to 31 do
        reduces.[i] <- Array.zeroCreate 27
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
let private lists_zeroReduces = [|[|12|]; [|6|]; [|18|]|]
let private small_zeroReduces =
        [|131076; 1310720; 1507328; 1638400; 1703936; 327686; 1179649; 1310721; 1376257; 1507329; 1638401; 1703937; 786438; 1179649; 1310721; 1376257; 1507329; 1638401; 1703937; 1114116; 1310720; 1507328; 1638400; 1703936; 1441794; 1638402; 1703938; 1703938; 1638402; 1703938|]
let zeroReduces = Array.zeroCreate 32
for i = 0 to 31 do
        zeroReduces.[i] <- Array.zeroCreate 27
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
let private accStates = Array.zeroCreate 32
for i = 0 to 31 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 26
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); null; null; null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_expr * '_rnglr_type_factor * '_rnglr_type_factorOp * '_rnglr_type_powExpr * '_rnglr_type_powOp * '_rnglr_type_term * '_rnglr_type_termOp * '_rnglr_type_yard_exp_brackets_4 * '_rnglr_type_yard_exp_brackets_5 * '_rnglr_type_yard_exp_brackets_6 * '_rnglr_type_yard_many_1 * '_rnglr_type_yard_many_2 * '_rnglr_type_yard_many_3 * '_rnglr_type_yard_rule_binExpr_1 * '_rnglr_type_yard_rule_binExpr_2 * '_rnglr_type_yard_rule_binExpr_3 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
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
          )
            )

               : '_rnglr_type_yard_exp_brackets_6) 
# 182 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
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
          )
            )

               : '_rnglr_type_yard_exp_brackets_5) 
# 204 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
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
          )
            )

               : '_rnglr_type_yard_exp_brackets_4) 
# 226 "Parser.fs"
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
# 246 "Parser.fs"
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
# 270 "Parser.fs"
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
# 290 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 27 "calc.yrd"
                                          []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_yard_many_3) 
# 308 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_6) l
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_3) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 27 "calc.yrd"
                                              yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_yard_many_3) 
# 330 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_powExpr) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_3) l
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
# 352 "Parser.fs"
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
                
# 27 "calc.yrd"
                                                     res 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "calc.yrd"
               : '_rnglr_type_factor) 
# 372 "Parser.fs"
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
# 392 "Parser.fs"
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
# 412 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 23 "calc.yrd"
                                       []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 23 "calc.yrd"
               : '_rnglr_type_yard_many_2) 
# 430 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_5) l
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_2) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 23 "calc.yrd"
                                           yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 23 "calc.yrd"
               : '_rnglr_type_yard_many_2) 
# 452 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_factor) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_2) l
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 16 "calc.yrd"
                     List.fold (fun l (op,r) -> op l r) l r 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 14 "calc.yrd"
               : '_rnglr_type_yard_rule_binExpr_2) 
# 474 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_binExpr_2) 
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
# 494 "Parser.fs"
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
# 514 "Parser.fs"
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
# 534 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 19 "calc.yrd"
                                     []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_many_1) 
# 552 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun l ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_4) l
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_1) l
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 19 "calc.yrd"
                                         yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_many_1) 
# 574 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_term) 
             |> List.iter (fun (l) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_1) l
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
# 596 "Parser.fs"
      );
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
# 616 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
            )
# 19 "calc.yrd"
               : '_rnglr_type_yard_start_rule) 
# 626 "Parser.fs"
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
# 644 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_4)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_5)  l ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun l ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_6)  l ) |> List.concat));
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
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
