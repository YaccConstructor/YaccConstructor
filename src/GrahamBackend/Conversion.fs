module Yard.Core.GrahamBachend

open Yard.Core.IL
open Constraints
open FSharpYacc.Ast
open Mono.Addins

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type FacioBackend() = 
    inherit Generator()
        override this.Name = "FacioBackend"
        override this.Generate (t, args) =       
        
            let getHeader =
                match t.head with
                | Some v -> Some v.text
                | None -> None
            
            let getFooter = 
                match t.foot with
                | Some v -> Some v.text
                | None -> None
            
            let getStart =
                let rec grammarLoop (grammar : Grammar.t<_,_>) (startingProductions : string list) = 
                    match grammar with
                    | head :: tail ->
                        let rec rulesLoop (rulesList : Rule.t<_,_> list) acc = 
                            match rulesList with
                            | head1 :: tail1 ->
                                match head1.isStart with
                                | true -> rulesLoop tail1 (List.concat [acc; [head1.name.text]])
                                | false -> rulesLoop tail1 acc
                            | [] -> grammarLoop tail acc
                        rulesLoop head.rules startingProductions
                    | [] -> startingProductions
                grammarLoop t.grammar List.empty

            let getTermsNonterms (grammar : Grammar.t<_,_>) =
                let tks = new ResizeArray<_>()
                //let nTerms = new ResizeArray<_>()
                let f (r : Rule.t<_,_>) =
                    match r.body  with
                    | Production.PSeq (p,ac,_) ->
                        let rec func (part : Production.elem<_,_> list) = 
                            match part with
                            | h :: t ->
                                match h.rule with
                                | Production.PToken pt -> 
                                    if not (tks.Contains pt.text) then
                                        tks.Add pt.text
                                //| Production.PRef(nt,_) -> nTerms.Add nt.text
                                | _ -> () //func t
                                func t
                            | [] -> ()
                        func p
                    | _ -> ()
                grammar |> List.iter (fun m -> m.rules |> List.iter f)
                List.ofSeq tks//, List.ofSeq nTerms

            //parse args
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length-1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]

            let  outputParserPath = ref None
            let  defaultTokenType = ref <| Some("")
            let  parserType = ref None

            for opt, value in pairs do
                match opt with
                | "-token" -> defaultTokenType := Some value
                | "-o" -> outputParserPath := Some value
                | "-parserType" -> parserType := Some value
                | _ -> ()

            let terminals = getTermsNonterms t.grammar

            let getTermWithTypes (definition : Definition.t<_,_>) =
                let tokensIL = definition.tokens
                let result = new System.Collections.Generic.Dictionary<DeclaredType option, TerminalIdentifier list>()
                let rec func terms = 
                    match terms with
                    | h :: t ->
                        if tokensIL.ContainsKey h then
                            if result.ContainsKey tokensIL.[h] then
                                result.[tokensIL.[h]] <- h :: result.[tokensIL.[h]]
                            else
                                result.Add(tokensIL.[h], [h])
                            func t
                        elif result.ContainsKey !defaultTokenType then
                            result.[!defaultTokenType] <- h :: result.[!defaultTokenType]
                            func t
                        elif not (result.ContainsKey !defaultTokenType) then
                            result.Add(!defaultTokenType, [h])
                            func t
                        else
                            failwithf "Expect type for token %s" h
                    | [] -> ()
                func terminals
                List.ofSeq result
                |> List.map (fun kvp -> kvp.Key, kvp.Value)

            let getNonterminalDeclarations (startingProductions : NonterminalIdentifier list) =
                let value : DeclaredType = "_"
                let (objList : DeclaredType list) = List.init startingProductions.Length (fun _ -> value)
                List.zip objList startingProductions

            let getRuleArgsAndAction (rule : Rule.t<_,Source.t>) = 
                match rule.body with
                | Production.PSeq (p,ac,_) ->
                    match ac with
                    | Some v -> List.rev p, Some v.text
                    | None -> failwithf "Action code for rule %s is empty" rule.name.text
                | _ -> failwithf "Rule %s don`t has args and action code" rule.name.text

            let getStrArgs args =
                let strArgs = new ResizeArray<_>()
                let rec func (ar : Production.elem<_,_> list) = 
                    match ar with
                    | h :: t ->
                        match h.rule with
                        | Production.PRef (name,_) -> strArgs.Add name.text
                        | Production.PToken src -> strArgs.Add src.text
                        | _ -> failwithf "Args type is n`t PRef or PToken in rule"
                        func t
                    | [] ->  strArgs
                List.ofSeq (func args)

            let getProductions =
                let productions = new System.Collections.Generic.Dictionary<NonterminalIdentifier, ProductionRule list>()
                let rec grammarLoop (grammar : Grammar.t<_,_>) = 
                    match grammar with
                    | head :: tail ->
                        let rec rulesLoop (rulesList : Rule.t<_,_> list) = 
                            match rulesList with
                            | headRule :: tailRule ->
                                let args, actionCode = getRuleArgsAndAction headRule
                                if productions.ContainsKey headRule.name.text then
                                    productions.[headRule.name.text] <-
                                        {   Symbols = getStrArgs args
                                            Action = actionCode;
                                            ImpersonatedPrecedence=Some("") } :: productions.[headRule.name.text]
                                else
                                    productions.Add(headRule.name.text,
                                        [{ Symbols = getStrArgs args;
                                           Action = actionCode;
                                           ImpersonatedPrecedence=Some("") }])
                                rulesLoop tailRule
                            | [] -> grammarLoop tail
                        rulesLoop head.rules
                    | [] -> ()
                grammarLoop t.grammar
                List.ofSeq productions
                |> List.map (fun kvp -> kvp.Key, kvp.Value)

            let specification : FSharpYacc.Ast.Specification = {
                Header = getHeader //Some("\r\nopen Ast\r\n")
                Footer = getFooter //Some(null)
                NonterminalDeclarations = getNonterminalDeclarations getStart //["Ast.Prog","start"]
                TerminalDeclarations = getTermWithTypes t //[(Some(""),["DO";"PRINT";"WHILE"]); (Some("System.Int32"), ["INT"])]
                StartingProductions = getStart //["start"]
                Associativities = []
                Productions = getProductions } //["Smth",[{Symbols=["Expr";"PRINT"]; Action=Some(" Print($2) "); ImpersonatedPrecedence=Some("")}]]; }
            
            let backendOpt : FSharpYacc.FsyaccBackendOptions = {
                OutputPath = (!outputParserPath).Value ;
                ModuleName = Some("Parser");
                LexerInterpreterNamespace = None;
                ParserInterpreterNamespace = None;
                OpenDeclarations = [||];
                InternalModule = true; }
            
            let getParserType =
                match (!parserType).Value with
                | "LR0" -> FSharpYacc.ParserType.LR0
                | "LR1" -> FSharpYacc.ParserType.LR1
                | "SLR1" -> FSharpYacc.ParserType.SLR1
                | "LALR1" -> FSharpYacc.ParserType.LALR1
                | _ -> failwith "Unrecognized parser type: %s" (!parserType).Value

            let options : FSharpYacc.CompilationOptions = {
                ParserType = getParserType;
                FsyaccBackendOptions = Some(backendOpt); }
            
            let procSpec, validMes = FSharpYacc.Compiler.precompile(specification, options)
            let backends = FSharpYacc.Program.loadBackends()
            let compileRes = FSharpYacc.Compiler.compile(procSpec, options)

            let backendInvoke =
                match compileRes with
                | Choice2Of3 errorMessages ->
                    failwith "ERROR: %s"errorMessages
                | Choice1Of3 parserTable ->
                    backends.FsyaccBackend.Invoke (
                        procSpec,
                        parserTable,
                        options)
                | Choice3Of3 parserTable ->
                    backends.FsyaccBackend.Invoke (
                        procSpec,
                        parserTable,
                        options)
            backendInvoke
            box "3" 
           
        override this.Constraints = [|noMeta; noEbnf; noInnerAlt; noAlt; noLiterals; noBrackets; needAC; singleModule|]
        override this.Generate definition = this.Generate (definition, "")

