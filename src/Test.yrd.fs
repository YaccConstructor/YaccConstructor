
# 2 "Test.yrd.fs"
module GLL.Parse.Test
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type

open Yard.Generators.GLL
open Dispatcher
open Yard.Generators.Common.AST2
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
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "D"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | D _ -> 5
    | RNGLR_EOF _ -> 6

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
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | s -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 1; 2|]
let table = [| [||];[||];[||];[||];[|"L_s_2"; "L_s_1"|];[||];[||];[||];[|"L_yard_start_rule_1"|];[||];[||];[||]; |]
let private rules = [|3; 5; 4; 3; 1|]
let private canInferEpsilon = [|true; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST2.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 3; 4; 5|]
let startRule = 2
let indexatorFullCount = 7
let rulesCount = 3
let indexEOF = 6
let nonTermCount = 3
let termCount = 4
let termStart = 3
let termEnd = 6
let literalStart = 7
let literalEnd = 6
let literalsCount = 0
let resultAST = ref None
//нужно генерировать номера правил в функци€х дл€ них
//вот там дл€ создани€ стартовой €чейки стека используетс€ число элементов в массиве минус 1.кажетс€, -1 не нужен.
let private parserSource = new ParserSource2<Token>(tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput, numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)

let L_s (currentLabel : int ref) (currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
        if funs.Test 1 "L_s_1" then
            funs.AddContext 1 !currentIndex !currentGSSNode funs.DummyAst
        if funs.Test 1 "L_s_2" then
            funs.AddContext 2 !currentIndex !currentGSSNode funs.DummyAst

let L_s_1 (currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 0
    let mutable position = 0
    currentN := funs.GetNodeT !currentIndex
    currentIndex := !currentIndex + 1
    position <- position + 1
    if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]
    then
        currentR := funs.GetNodeT !currentIndex
        currentIndex := !currentIndex + 1
        position <- position + 1
        let label = funs.PackLabel rule position
        currentN := funs.GetNodeP label !currentN !currentR
        if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]
        then
            currentR := funs.GetNodeT !currentIndex
            currentIndex := !currentIndex + 1
            let label = funs.PackLabel rule position
            currentN := funs.GetNodeP label !currentN !currentR
            funs.Pop !currentGSSNode !currentIndex !currentN currentN.Value.extension

let L_s_2 (currentLabel : int ref) (currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let ruleNumber = 1
    let mutable position = 0
    currentN := funs.GetNodeT !currentIndex
    funs.Pop !currentGSSNode !currentIndex !currentN currentN.Value.extension

let L_yard_start_rule (currentLabel : int ref) (currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
        if funs.Test 2 "L_yard_start_rule_1" then
            funs.AddContext 4 !currentIndex !currentGSSNode funs.DummyAst

let L_yard_start_rule_1 (currentLabel : int ref) (currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    currentGSSNode := funs.Create 5 !currentGSSNode !currentIndex !currentN
    funs.AddContext 0 !currentIndex !currentGSSNode funs.DummyAst

let L_yard_start_rule_1_1 (currentLabel : int ref) (currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[]) = 
    let rule = 2
    //KOSTIIIILLLL
    let label = funs.PackLabel rule 1
    currentN := funs.GetNodeP label !currentN !currentR
    let curRight = !currentN
    let rule = 2
    let key = funs.Pack rule (getLeftExtension curRight.extension) (getRightExtension curRight.extension) 
    let l = getLeftExtension curRight.extension
    let r = getRightExtension curRight.extension
    let resTree = funs.FindTree curRight.tree (rule) key
    if  key = funs.FinalExt
    then resultAST := Some resTree
    
    
let functions = [|L_s; L_s_1; L_s_2; L_yard_start_rule; L_yard_start_rule_1; L_yard_start_rule_1_1|]

let buildAst tokens = new buildAst<Token>(parserSource, tokens, functions, resultAST)

