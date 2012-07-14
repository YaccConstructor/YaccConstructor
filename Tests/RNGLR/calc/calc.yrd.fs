module RNGLR.ParseCalc
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | ADD of int
    | B of int
    | EOF of int
    | MUL of int

let numToString = function 
    | 0 -> "expr"
    | 1 -> "fact"
    | 2 -> "num"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "ADD"
    | 6 -> "B"
    | 7 -> "EOF"
    | 8 -> "MUL"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | EOF _ -> 7
    | MUL _ -> 8

let leftSide = [|0; 0; 3; 1; 1; 2; 2|]
let rules = [|1; 0; 5; 0; 0; 2; 1; 8; 1; 6; 4|]
let rulesStart = [|0; 1; 4; 5; 6; 9; 10; 11|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot startInd numToString getRight)

let buildAst : (seq<Token> -> ParseResult<Token>) =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 1,4; 2,7; 4,8; 6,9|]; 1, [|5,2|]; 2, [|0,3; 1,4; 2,7; 4,8; 6,9|]; 3, [|5,2|]; 4, [|8,5|]; 5, [|1,6; 2,7; 4,8; 6,9|]; 6, [|8,5|]|]
    let gotos = Array.zeroCreate 10
    for i = 0 to 9 do
        gotos.[i] <- Array.create 9 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[||]; [|1,3|]; [|0,1|]; [|4,3|]; [|3,1|]; [|6,1|]; [|5,1|]|]
    let small_reduces =
        [|196610; 327681; 458753; 262146; 327682; 458754; 393219; 327683; 458755; 524291; 458755; 327684; 458756; 524292; 524291; 327685; 458757; 524293; 589827; 327686; 458758; 524294|]
    let reduces = Array.zeroCreate 10
    for i = 0 to 9 do
        reduces.[i] <- Array.create 9 [||]
    let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
    let lists_zeroReduces = [|[||]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 10
    for i = 0 to 9 do
        zeroReduces.[i] <- Array.create 9 [||]
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
    let eofIndex = 7
    let parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<Token> parserSource

#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null|]
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_expr * '_rnglr_type_fact * '_rnglr_type_num * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_fact) 
             |> List.iter (fun (f) -> 
              _rnglr_cycle_res := ( f )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_expr)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ADD _rnglr_val -> [_rnglr_val] | a -> failwith "ADD expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_expr) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := ( a + b )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_expr)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_expr) 
           ) : '_rnglr_type_yard_start_rule)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_num) 
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := ( n )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_fact)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_fact) 
             |> List.iter (fun (a) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with MUL _rnglr_val -> [_rnglr_val] | a -> failwith "MUL expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_1) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_fact) 
                 |> List.iter (fun (b) -> 
                  _rnglr_cycle_res := ( a * b )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_fact)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with B _rnglr_val -> [_rnglr_val] | a -> failwith "B expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 5 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_num)
      );
  (
    fun (_rnglr_children : array<_>) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with A _rnglr_val -> [_rnglr_val] | a -> failwith "A expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := ( 3 )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          ) ) : '_rnglr_type_num)
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_expr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_fact)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_num)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats _rnglr_epsilons) : '_rnglr_type_yard_start_rule
