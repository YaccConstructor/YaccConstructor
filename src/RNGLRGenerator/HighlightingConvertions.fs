module HighlightingConvertions

open System

open Yard.Core.IL
open Yard.Generators.Common

open HighlightingPrinter

let toClassName (str : string) = 
    let symbols = [| 
                    for i = 0 to str.Length - 1 do
                        if i = 0 
                        then yield Char.ToUpper str.[0]
                        else yield str.[i] 
                    |] 
    new String(symbols)

let litToClassName (lit : string) = 
    toClassName <| lit.ToLowerInvariant()

let private getLeafSemanticForTerminal token = 
    let printer = new FormatPrinter()

    printer.PrintBr "let pos = _rnglr_var_0"
    printer.PrintBr "let ranges = calculatePos pos"

    printer.PrintBr "new %s%s(ranges) :> ITreeNode" <| toClassName token <| termSuffix
    printer.ToString()

let private getLeafSemanticForLiteral litName litText = 
    let printer = new FormatPrinter()

    printer.PrintBr "let pos =  _rnglr_var_0"
    printer.PrintBr "let ranges = calculatePos pos"

    printer.PrintBr "new %s%s(ranges) :> ITreeNode"  <| litToClassName litName <| literalSuffix
    printer.ToString()
                               
let private getNodeSemantic parent children = 
    let printer = new FormatPrinter()

    printer.PrintBrInd 0 "let parent = new %s%s()" <| toClassName parent <| nonTermSuffix
    printer.PrintBrInd 0 "let children = %A" children
    printer.PrintBrInd 0 "addSemantic parent children"
    printer.ToString()

let private changeRule (oldRule : Rule<_,_>) (elemList : ProductionElem<Source.t, Source.t> list) (bindings : Source.t list) = 
    let actionCode = getNodeSemantic oldRule.name.text bindings
    let newRule : Rule<Source.t, Source.t> = 
        {
            name = oldRule.name
            args = []
            body = PSeq(elemList, Some <| new Source.t(actionCode), None)
            isStart = oldRule.isStart
            isPublic = oldRule.isPublic
            isInline = oldRule.isInline
            metaArgs = []
        }
    newRule

let private createNewBinding (numOpt : int ref option) = 
    let newBinding = 
        numOpt
        |> Option.map
            (
                fun num -> 
                    incr num
                    new Source.t (sprintf "h%d" num.Value)
            )
    newBinding

let private getNewElem newBinding refRule = 
    let newElem : ProductionElem<Source.t, Source.t> = 
        {
            binding = newBinding
            checker = None
            omit = false
            rule = refRule
        }
    newElem

let private literalToName rules lit = 
    let indexator = new Indexator(rules, true)
    lit
    |> indexator.literalToIndex
    |> indexator.getLiteralName

let createHighlightingRule name newElem actionCode =
    let newRule : Rule<Source.t, Source.t> =
        {
            name = new Source.t(sprintf "highlight_%s" name)
            args = []
            body = PSeq([newElem], Some <| actionCode, None)
            isStart = false
            isPublic = false
            isInline = false
            metaArgs = []
        }
    newRule

let getRulesForTerminal terminals = 
    let processTerminal terminal = 
        let actionCode = new Source.t(getLeafSemanticForTerminal terminal)
        let newElem = getNewElem None <| PToken (new Source.t(terminal))
        createHighlightingRule terminal newElem actionCode

    terminals
    |> List.map processTerminal

let getRulesForLiterals literals = 
    
    let processLiteral literal =
        let litName, litText = literal
        let actionCode = new Source.t (getLeafSemanticForLiteral litName litText)
        let newElem =  getNewElem None <| PLiteral(new Source.t(litName))

        createHighlightingRule litName newElem actionCode

    literals
    |> List.map processLiteral

let highlightingConvertions (def : Definition<Source.t, Source.t>) = 
    let rules = def.grammar.Head.rules
    let literalToName' = literalToName rules

    let terminals = ref []
    let literals = ref []

    let rec processElem (oldElem : ProductionElem<Source.t, Source.t>) count = 
        let result = ref []
        let newElem = ref oldElem

        let inline createHighlightRefRule text = 
            PRef(new Source.t(sprintf "highlight_%s" text), None)
        
        match oldElem.rule with
        | PSeq (metaList,_,_) -> 
            metaList
            |> List.iter
                (
                    fun meta -> 
                        let someElemList = processElem meta count
                        result := !result @ someElemList
                )

        | PRef (name, _) as oldElemRule -> 
            let newBinding = createNewBinding <| Some count
            newElem := getNewElem <| newBinding <| PRef (name, None)
            result := !result @ [!newElem]

        | PToken tok -> 
            if List.forall ((<>) tok.text) !terminals
            then terminals := tok.text :: !terminals
            
            let newBinding = createNewBinding <| Some count
            newElem := getNewElem newBinding <| createHighlightRefRule tok.text
            result := !result @ [!newElem]
        
        | PLiteral lit -> 
            let litName = literalToName' lit.text
            if List.forall (fun symbol -> fst symbol <> litName) !literals
            then literals := (litName, lit.text) :: !literals
        
            let newBinding = createNewBinding <| Some count
            newElem := getNewElem <| newBinding <| createHighlightRefRule litName
            result := !result @ [!newElem]

        | _ -> failwith "Error in highlighting convertions"
        !result

    let processRule (oldRule : Rule<Source.t, Source.t>) = 
        let count = ref 0
        match oldRule.body with 
        | PSeq(elemList, _, _) -> 
            let newElemList = ref []
            let bindingsList = ref []
            elemList
            |> List.iter(fun item -> newElemList := !newElemList @ processElem item count)

            !newElemList
            |> List.iter (fun newElem -> bindingsList := newElem.binding.Value :: !bindingsList)

            changeRule <| oldRule <| !newElemList <| List.rev !bindingsList
        | PRef _ -> oldRule
        | _ -> failwith "Error in highlighting convertions"

    let addHighlightRules() = 
        getRulesForTerminal !terminals @ getRulesForLiterals !literals

    let newRules = List.map processRule rules @ addHighlightRules()
    {def with grammar = [{def.grammar.Head with rules = newRules}]}