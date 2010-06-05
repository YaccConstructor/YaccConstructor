namespace Yard.Frontends.IronyFrontend

open Yard.Core
open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Irony.Parsing
open Microsoft.FSharp.Collections

module Converter =

    let Convert (ironyGrammar : Irony.Parsing.Grammar) = 
        let refTerms = ref [ironyGrammar.Root :> BnfTerm]
        let rec findBnfTerms (start:NonTerminal) = 
            ResizeArray.iter
                (fun termList -> 
                    (ResizeArray.iter 
                        (fun bnfTerm -> 
                            if not (List.exists (fun findedTerm -> findedTerm = bnfTerm) !refTerms) then
                                refTerms := bnfTerm :: !refTerms
                                if bnfTerm :? NonTerminal then
                                    findBnfTerms (bnfTerm :?> NonTerminal) )                                                       
                        termList ) )
                start.Rule.Data
  
        findBnfTerms ironyGrammar.Root

        let nonTerminals = List.filter (fun (t : BnfTerm) -> (t :? NonTerminal)) !refTerms
        let (grammar : IL.Grammar.t<IL.Source.t, IL.Source.t>) = 
            List.map
                (fun (bnfTerm : BnfTerm) ->
                    let nt = bnfTerm :?> NonTerminal
                    let productionOpt = 
                        ResizeArray.fold
                            (fun prOpt bnfTermList -> 
                                let pseq = 
                                    PSeq(
                                        ResizeArray.toList (ResizeArray.map 
                                            (fun (bnfTerm : BnfTerm)-> 
                                                ({omit = false; 
                                                rule = match bnfTerm with
                                                        | :? NonTerminal as term -> PRef((term.Name, (-419,-419)), None)
                                                        | :? Terminal as term -> PToken(term.Name, (-419,-419))
                                                        | _ -> failwith "Not supported BnfTerm type"
                                                        ;
                                                binding=None; 
                                                checker=None})) 
                                            (bnfTermList)), 
                                        None)
                                match prOpt with
                                | Some(pr)  -> Some(PAlt(pr, pseq))
                                | None      -> Some(pseq) )
                            None
                            nt.Rule.Data
                    match productionOpt with
                    | Some(pr)  -> {name = nt.Name; args = []; body = pr; metaArgs = []; _public = (nt = (ironyGrammar.Root))}
                    | None      -> failwith "minimum 1 alternative is required" )
                nonTerminals
        grammar


