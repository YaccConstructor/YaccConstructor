//  Converter.fs
//
//  Copyright 2010 Konstantin Ulitin <ulitin.k@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.


namespace Yard.Frontends.IronyFrontend

open Yard.Core
open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Irony.Parsing
open Microsoft.FSharp.Collections

module Converter =

    let formatTermName termName = 
        String.collect (
            function
            | '>' -> "GREATER"
            | '=' -> "EQUAL"
            | '<' -> "LESS"
            | c   -> string (System.Char.ToLower c) 
            )
            termName

    let formatNontermName = String.uncapitalize
            
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
                                                        | :? NonTerminal as term -> PRef((formatNontermName term.Name, (-419,-419)), None)
                                                        | :? Terminal as term -> PToken(formatTermName term.Name, (-419,-419))
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
                    | Some(pr)  -> {name = formatNontermName nt.Name; args = []; body = pr; metaArgs = []; _public = (nt = (ironyGrammar.Root))}
                    | None      -> failwith "minimum 1 alternative is required" )
                nonTerminals
        grammar


