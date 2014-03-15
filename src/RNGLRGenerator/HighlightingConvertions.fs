module HighlightingConvertions

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.RNGLR
open PrintTreeNode

let getLeafSemantic leaf isTok = 
    let res = new System.Text.StringBuilder()
    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    if isTok
    then printBr "new %sTermNode(\"%s\")" <| firstLetterToUpper leaf <| leaf
    else printBr "new %sLitNode(\"%s\")"  <| firstLetterToUpper leaf <| leaf
    
    res.ToString()
                               
let getNodeSemantic parent children = 
    let res = new System.Text.StringBuilder()
    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x

    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

    let inline printBrInd num (x : 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x

    printBrInd 0 "let parent = new %sNonTermNode(\"%s\")" <| firstLetterToUpper parent <| parent
    printBrInd 0 "let children : IAbstractTreeNode list = %A" children
    printBrInd 0 "addSemantic parent children"
    res.ToString()

let highlightingConvertions (def : Definition.t<Source.t, Source.t>) = 
    let rules = def.grammar.Head.rules    
    let literalToName lit = 
        let indexator = new Indexator(rules, true)
        lit
        |> indexator.literalToIndex
        |> indexator.getLiteralName

    let termsAndLitsList = ref []

    let getNewElem newBinding refRule : elem<Source.t, Source.t> = 
        let newElem : elem<Source.t, Source.t> = 
            {
                binding = newBinding
                checker = None
                omit = false
                rule = refRule
            }
        newElem

    let changeRule (oldRule : Rule.t<_,_>) (elemList : elem<Source.t, Source.t> list) (bindings : Source.t list) = 
        let actionCode = getNodeSemantic oldRule.name.text bindings
        let newRule : Rule.t<Source.t, Source.t> = 
            {
                name = oldRule.name
                args = []
                body = PSeq(elemList, Some <| new Source.t(actionCode), None)
                isStart = oldRule.isStart
                isPublic = oldRule.isPublic
                metaArgs = []
            }
        newRule

    let processElem (element : elem<Source.t, Source.t>) newBinding = 
        
        match element.rule with
        | t.PSeq (_,_,_) 
        | t.PRef (_,_) as refer -> 
            getNewElem newBinding <| refer
        | t.PToken tok -> 
            if not <| List.exists (fun symbol -> fst symbol = tok.text) !termsAndLitsList
            then termsAndLitsList := (tok.text, true) :: !termsAndLitsList
            
            getNewElem newBinding <| t.PRef(new Source.t("Highlight_" + tok.text), None)
        
        | t.PLiteral lit -> 
            let litName = literalToName lit.text
            if not <| List.exists (fun symbol -> fst symbol = litName) !termsAndLitsList
            then termsAndLitsList := (litName, false) :: !termsAndLitsList
        
            getNewElem newBinding <| t.PRef(new Source.t("Highlight_" + litName), None)
        
        | _ as smth -> 
                eprintfn "Unmatched %s pattern" <| element.rule.GetType().FullName
                printfn  "Unmatched %s pattern" <| element.rule.GetType().FullName
                failwith "Unexpected pattern "  <| element.ToString()

    let processRule (rule : Rule.t<Source.t, Source.t>) = 
        match rule.body with 
        | t.PSeq(elemList, _, _) -> 
                            let mutable newElemList = []
                            let mutable bindingsList = []
                            for item in elemList do
                                let newBinding = Some <| new Source.t ("H" + newElemList.Length.ToString())
                                bindingsList <- newBinding.Value :: bindingsList
                                        
                                let newElem = processElem item newBinding
                                newElemList <- newElem :: newElemList

                            changeRule <| rule <| List.rev newElemList <| List.rev bindingsList
        | t.PRef (_, _) -> rule

    let addHighlightRules() = 
        let mutable res = []
        for tok, isTok in !termsAndLitsList do 
            let actionCode, newElem =
                if isTok 
                then new Source.t(getLeafSemantic tok isTok), getNewElem None <| t.PToken (new Source.t(tok))
                else new Source.t(getLeafSemantic tok isTok), getNewElem None <| t.PLiteral (new Source.t(tok))

            let newRule : Rule.t<Source.t, Source.t> = 
                {
                    name = new Source.t("Highlight_" + tok)
                    args = []
                    body = PSeq([newElem], Some <| actionCode, None)
                    isStart = false
                    isPublic = false
                    metaArgs = []
                }
            res <- newRule :: res
        res

    let newRules = List.map processRule rules @ addHighlightRules()

    {def with grammar = [{def.grammar.Head with rules=newRules}]}