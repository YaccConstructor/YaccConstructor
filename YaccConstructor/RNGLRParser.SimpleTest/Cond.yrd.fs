
# 2 "Cond.yrd.fs"
module RNGLR.ParseCond
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | ELSE of int
    | IF of int
    | RNGLR_EOF of int

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | ELSE x -> box x
    | IF x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "good"
    | 2 -> "if"
    | 3 -> "if_else"
    | 4 -> "s"
    | 5 -> "stmt"
    | 6 -> "yard_start_rule"
    | 7 -> "A"
    | 8 -> "ELSE"
    | 9 -> "IF"
    | 10 -> "RNGLR_EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 7
    | ELSE _ -> 8
    | IF _ -> 9
    | RNGLR_EOF _ -> 10

let mutable private cur = 0
let leftSide = [|4; 6; 2; 2; 3; 1; 1; 5; 5|]
let private rules = [|2; 4; 9; 5; 3; 9; 1; 8; 5; 3; 5; 2; 7|]
let private rulesStart = [|0; 1; 2; 4; 5; 9; 10; 11; 12; 13|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 10; 11; 12; 13; 14; 6; 7; 8; 9; 15; 19; 16; 17; 18|]
let private small_gotos =
        [|4; 131072; 196609; 262146; 589827; 262150; 65540; 131077; 196614; 327687; 458760; 589833; 327681; 524298; 393221; 131083; 196609; 327692; 458765; 589827; 917510; 65550; 131077; 196614; 327695; 458760; 589833; 983041; 524304; 1048581; 131077; 196625; 327698; 458760; 589833|]
let gotos = Array.zeroCreate 20
for i = 0 to 19 do
        gotos.[i] <- Array.zeroCreate 11
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
let private lists_reduces = [|[|0,1|]; [|3,1|]; [|7,1|]; [|4,4|]; [|8,1|]; [|5,1; 3,1|]; [|6,1|]; [|2,2|]; [|6,1; 2,2|]|]
let private small_reduces =
        [|65537; 655360; 131073; 655361; 458753; 655362; 524289; 655363; 589825; 655364; 655362; 524290; 655362; 720898; 524293; 655361; 786434; 524294; 655367; 851970; 524292; 655364; 1114114; 524289; 655361; 1179650; 524291; 655363; 1245186; 524296; 655367|]
let reduces = Array.zeroCreate 20
for i = 0 to 19 do
        reduces.[i] <- Array.zeroCreate 11
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 20
for i = 0 to 19 do
        zeroReduces.[i] <- Array.zeroCreate 11
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
let private small_acc = [3]
let private accStates = Array.zeroCreate 20
for i = 0 to 19 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 10
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_good * '_rnglr_type_if * '_rnglr_type_if_else * '_rnglr_type_s * '_rnglr_type_stmt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 2 "Cond.yrd"
                          r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 2 "Cond.yrd"
               : '_rnglr_type_s) 
# 136 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 2 "Cond.yrd"
               : '_rnglr_type_yard_start_rule) 
# 146 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwith "IF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_stmt) 
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 3 "Cond.yrd"
                                                  r * 10
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 3 "Cond.yrd"
               : '_rnglr_type_if) 
# 168 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if_else) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 3 "Cond.yrd"
                                r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Cond.yrd"
               : '_rnglr_type_if) 
# 188 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwith "IF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_good) 
               |> List.iter (fun (t) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with ELSE _rnglr_val -> [_rnglr_val] | a -> failwith "ELSE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt) 
                   |> List.iter (fun (f) -> 
                    _rnglr_cycle_res := (
                      
# 4 "Cond.yrd"
                                                       t+f
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 4 "Cond.yrd"
               : '_rnglr_type_if_else) 
# 214 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if_else) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 5 "Cond.yrd"
                                               r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 5 "Cond.yrd"
               : '_rnglr_type_good) 
# 234 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 5 "Cond.yrd"
                               r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 5 "Cond.yrd"
               : '_rnglr_type_good) 
# 254 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_if) 
             |> List.iter (fun (r) -> 
              _rnglr_cycle_res := (
                
# 6 "Cond.yrd"
                                     r
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "Cond.yrd"
               : '_rnglr_type_stmt) 
# 274 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 6 "Cond.yrd"
                          2
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "Cond.yrd"
               : '_rnglr_type_stmt) 
# 294 "Cond.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
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
# 312 "Cond.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_good)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_if)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_if_else)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
