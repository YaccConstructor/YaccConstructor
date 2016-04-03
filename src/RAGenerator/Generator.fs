namespace Yard.Generators.RAGenerator

open Mono.Addins
open Yard.Core
open IL
open Constraints


[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()
[<Extension>]
type RAGenerator() = 
    inherit Generator()

    let ILtoInternal (il: Yard.Core.IL.Definition.t<_, _>) =
            let terms : Set<Internal.Terminal> ref = ref Set.empty
            let nonterms : Set<Internal.NonTerminal> ref = ref Set.empty
            let toList s = Set.fold (fun l se -> se :: l) [] s

            let startSym = ref ""

            let buildRule head body = Internal.Prod (head, body)

            let processBody (body: Production.elem<'patt, 'expr'> list) = 
                body |> List.fold (fun (acc: Internal.Symbol list) elem ->
                    let curSym = 
                        match elem.rule with
                        | Production.PToken tok
                        | Production.PLiteral tok -> 
                            if not ((!terms).Contains (tok.text)) then 
                                terms := (!terms).Add (tok.text)
                                Internal.Terminal tok.text
                            else Internal.Terminal tok.text
                        | Production.PRef (ref, _) ->
                            if not ((!nonterms).Contains ref.text) then 
                                nonterms := (!nonterms).Add ref.text
                                Internal.NonTerminal ref.text
                            else Internal.NonTerminal ref.text
                        | _ -> failwith "Invalid body structure"
                    acc @ [curSym]) []
            
            let processRule (r: Rule.t<_, _>) =
                let name = r.name
                let body = r.body    

                if r.isStart then startSym := name.text

                if not ((!nonterms).Contains name.text) then 
                    nonterms := (!nonterms).Add name.text  
                else ()  
                        
                match body with
                | Production.PSeq (body, _, lbl) ->
                    let processedBody = processBody body
                    buildRule name.text processedBody
                | x -> failwithf "Invalid rule structure %A" x
          
            let prods = il.grammar.[0].rules |> List.map processRule

            if !startSym = "" then failwith "Non start symbol"

            { Internal.Terminals = toList !terms
              Internal.NonTerminals = toList !nonterms
              Internal.Productions = prods
              Internal.StartSymbol = !startSym }
       
    override this.Name = "RAGenerator"
    override this.Constraints = [|noEbnf; noMeta; noInnerAlt; noAlt; noBrackets; singleModule|]
    override this.Generate (definition, args) = 
        let internalGrammar = ILtoInternal definition
        let test ex = Internal.generateInternal (fun funPrint -> funPrint stdout) ex
        test internalGrammar |> ignore
        box()
    override this.Generate definition = this.Generate (definition, "")
