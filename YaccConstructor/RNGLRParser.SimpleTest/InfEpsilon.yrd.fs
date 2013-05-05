
# 2 "InfEpsilon.yrd.fs"
module RNGLR.ParseInfEpsilon
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | RNGLR_EOF of int

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_many_1"
    | 3 -> "yard_opt_1"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "RNGLR_EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 5
    | RNGLR_EOF _ -> 6

let mutable private cur = 0
let leftSide = [|1; 4; 3; 3; 2; 2|]
let private rules = [|2; 1; 5; 3; 2|]
let private rulesStart = [|0; 1; 2; 3; 3; 5; 5|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 5; 4|]
let private small_gotos =
        [|4; 65536; 131073; 196610; 327683; 196611; 131076; 196610; 327683|]
let gotos = Array.zeroCreate 6
for i = 0 to 5 do
        gotos.[i] <- Array.zeroCreate 7
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
let private lists_reduces = [|[|0,1|]; [|4,1|]; [|4,2|]; [|2,1|]|]
let private small_reduces =
        [|131073; 393216; 196609; 393217; 262145; 393218; 327682; 327683; 393219|]
let reduces = Array.zeroCreate 6
for i = 0 to 5 do
        reduces.[i] <- Array.zeroCreate 7
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
let private lists_zeroReduces = [|[|3|]; [|5; 4; 3; 1; 0|]; [|5; 4; 3|]|]
let private small_zeroReduces =
        [|2; 327680; 393217; 196610; 327680; 393218|]
let zeroReduces = Array.zeroCreate 6
for i = 0 to 5 do
        zeroReduces.[i] <- Array.zeroCreate 7
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
let private accStates = Array.zeroCreate 6
for i = 0 to 5 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 6
let errorNIndex = 0
let errorTIndex = -1
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorNIndex, errorTIndex)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_s * '_rnglr_type_yard_many_1 * '_rnglr_type_yard_opt_1 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          []
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_s) 
# 109 "InfEpsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_s) 
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_yard_start_rule) 
# 119 "InfEpsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 2 "InfEpsilon.yrd"
                    Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_yard_opt_1) 
# 139 "InfEpsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 2 "InfEpsilon.yrd"
                  None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_yard_opt_1) 
# 157 "InfEpsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_1) 
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_1) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 2 "InfEpsilon.yrd"
                      yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_yard_many_1) 
# 179 "InfEpsilon.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 2 "InfEpsilon.yrd"
                  []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 2 "InfEpsilon.yrd"
               : '_rnglr_type_yard_many_1) 
# 197 "InfEpsilon.yrd.fs"
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
# 215 "InfEpsilon.yrd.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
