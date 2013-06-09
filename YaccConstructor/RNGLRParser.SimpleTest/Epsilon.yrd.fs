
# 2 "Epsilon.yrd.fs"
module RNGLR.ParseEpsilon
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_rule_op_1"
    | 3 -> "yard_rule_op_2"
    | 4 -> "yard_rule_op_3"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "B"
    | 8 -> "C"
    | 9 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | B _ -> 7
    | C _ -> 8
    | RNGLR_EOF _ -> 9

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 5; 4; 4; 3; 3; 2; 2|]
let private rules = [|2; 3; 4; 1; 8; 7; 6|]
let private rulesStart = [|0; 3; 4; 4; 5; 5; 6; 6; 7|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 7; 3; 6; 4; 5|]
let private small_gotos =
        [|3; 65536; 131073; 393218; 131074; 196611; 458756; 196610; 262149; 524294|]
let gotos = Array.zeroCreate 8
for i = 0 to 7 do
        gotos.[i] <- Array.zeroCreate 10
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
let private lists_reduces = [|[|0,1|]; [|0,2|]; [|0,3|]; [|3,1|]; [|5,1|]; [|7,1|]|]
let private small_reduces =
        [|131073; 589824; 196609; 589825; 262145; 589826; 327681; 589827; 393218; 524292; 589828; 458755; 458757; 524293; 589829|]
let reduces = Array.zeroCreate 8
for i = 0 to 7 do
        reduces.[i] <- Array.zeroCreate 10
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
let private lists_zeroReduces = [|[|6|]; [|6; 1; 0|]; [|4|]; [|2|]|]
let private small_zeroReduces =
        [|3; 458752; 524288; 589825; 131074; 524290; 589826; 196609; 589827|]
let zeroReduces = Array.zeroCreate 8
for i = 0 to 7 do
        zeroReduces.[i] <- Array.zeroCreate 10
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
let private small_acc = [1; 0]
let private accStates = Array.zeroCreate 8
for i = 0 to 7 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 9
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(2, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(2, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_s * '_rnglr_type_yard_rule_op_1 * '_rnglr_type_yard_rule_op_2 * '_rnglr_type_yard_rule_op_3 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_rule_op_1) 
             |> List.iter (fun (a1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_rule_op_2) 
               |> List.iter (fun (a2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_rule_op_3) 
                 |> List.iter (fun (a3) -> 
                  _rnglr_cycle_res := (
                    
# 2 "Epsilon.yrd"
                                                    a1 + a2 + a3
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 2 "Epsilon.yrd"
               : '_rnglr_type_s) 
# 147 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 2 "Epsilon.yrd"
               : '_rnglr_type_yard_start_rule) 
# 157 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 3 "Epsilon.yrd"
                                1
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_3) 
# 175 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with C _rnglr_val -> [_rnglr_val] | a -> failwith "C expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 3 "Epsilon.yrd"
                           10
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_3) 
# 195 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 3 "Epsilon.yrd"
                                1
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_2) 
# 213 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 3 "Epsilon.yrd"
                           10
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_2) 
# 233 "Epsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 3 "Epsilon.yrd"
                                1
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_1) 
# 251 "Epsilon.yrd.fs"
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
                
# 3 "Epsilon.yrd"
                           10
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 3 "Epsilon.yrd"
               : '_rnglr_type_yard_rule_op_1) 
# 271 "Epsilon.yrd.fs"
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
# 289 "Epsilon.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_s)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_rule_op_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
