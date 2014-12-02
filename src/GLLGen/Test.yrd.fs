
# 2 "Test.yrd.fs"
module GLL.Parse.Test
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.Common.AST2
open Dispatcher
type Token =
    | A of (int)
    | B of (int)
    | D of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | D x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "d"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "D"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | D _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | D _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | B _ -> true
    | D _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | d -> true
    | error -> true
    | s -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|2; 2; 3; 0|]
let table = [| [||];[||];[|"L_d_1"|];[||];[||];[||];[||];[||];[|"L_s_2"; "L_s_1"|];[||];[||];[||];[|"L_yard_start_rule_1"|];[||];[||];[||]; |]
let private rules = [|4; 6; 5; 4; 0; 5; 2; 6|]
let private canInferEpsilon = [|false; true; false; false; false; false; false; false|]

let private rulesStart = [|0; 3; 6; 7; 8|]
let startRule = 2
let indexatorFullCount = 8
let rulesCount = 4
let indexEOF = 7
let nonTermCount = 4
let termCount = 4
let termStart = 4
let termEnd = 7
let literalStart = 8
let literalEnd = 7
let literalsCount = 0
let resultAST = ref None

let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput, numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let L_s (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    if funs.Test 2 "L_s_1" then
        funs.AddContext 0 !currentIndex !currentGSSNode funs.DummyAst
    if funs.Test 2 "L_s_2" then
        funs.AddContext 1 !currentIndex !currentGSSNode funs.DummyAst

let L_s_1 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 0
    let mutable position = 0
    currentN := funs.GetNodeT !currentIndex
    currentIndex := !currentIndex + 1
    position <- position + 1
    if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]
    then
        currentR := funs.GetNodeT !currentIndex
        let label = funs.PackLabel rule position
        currentN := funs.GetNodeP label !currentN !currentR
        currentIndex := !currentIndex + 1
        position <- position + 1
        if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]
        then
            currentR := funs.GetNodeT !currentIndex
            let label = funs.PackLabel rule position
            currentN := funs.GetNodeP label !currentN !currentR
            let key = funs.Pack rule (getLeftExtension currentN.Value.extension) (getRightExtension currentN.Value.extension)
            let resTree = funs.FindTree (!currentN).tree rule key 
            funs.Pop !currentGSSNode !currentIndex resTree currentN.Value.extension

let L_s_2 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 1
    let mutable position = 0
    currentN := funs.GetNodeT !currentIndex
    currentGSSNode := funs.Create 2 !currentGSSNode !currentIndex !currentN
    funs.AddContext 3 !currentIndex !currentGSSNode funs.DummyAst

let L_s_2_2 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 1
    let mutable position = 2
    if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]
    then
        currentR := funs.GetNodeT !currentIndex
        let label = funs.PackLabel rule position
        currentN := funs.GetNodeP label !currentN !currentR
        let key = funs.Pack rule (getLeftExtension currentN.Value.extension) (getRightExtension currentN.Value.extension)
        let resTree = funs.FindTree currentN.Value.tree rule key
        funs.Pop !currentGSSNode !currentIndex resTree currentN.Value.extension

let L_yard_start_rule (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    if funs.Test 3 "L_yard_start_rule_1" then
        funs.AddContext 5 !currentIndex !currentGSSNode funs.DummyAst

let L_yard_start_rule_1 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 2
    let mutable position = 0
    currentGSSNode := funs.Create 6 !currentGSSNode !currentIndex !currentN
    funs.AddContext 7 !currentIndex !currentGSSNode funs.DummyAst

let L_yard_start_rule_1_1 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 2
    let mutable position = 0
    let label = funs.PackLabel rule 1
    currentN := funs.GetNodeP label !currentN !currentR
    let key = funs.Pack rule (getLeftExtension currentN.Value.extension) (getRightExtension currentN.Value.extension)
    let resTree = funs.FindTree currentN.Value.tree (rule) key
    if  key = funs.FinalExt
    then resultAST := Some resTree


let L_d (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    if funs.Test 0 "L_d_1" then
        funs.AddContext 8 !currentIndex !currentGSSNode funs.DummyAst

let L_d_1 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 3
    let mutable position = 0
    currentN := funs.GetNodeT !currentIndex
    let key = pack3ToInt64 rule (getLeftExtension currentN.Value.extension) (getRightExtension currentN.Value.extension)
    let resTree = funs.FindTree currentN.Value.tree  rule key
    funs.Pop !currentGSSNode !currentIndex resTree currentN.Value.extension

let startFunctionName = 4
let functions = [|L_s_1; L_s_2; L_s_2_2; L_d; L_yard_start_rule; L_yard_start_rule_1; L_yard_start_rule_1_1; L_s; L_d_1|]
let buildAst tokens =
    new buildAst<Token>(parserSource, tokens, functions, startFunctionName, resultAST)


