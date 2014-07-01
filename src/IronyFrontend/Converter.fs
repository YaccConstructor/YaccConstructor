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
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Irony.Parsing
open Microsoft.FSharp.Collections


module Converter =


    let dummyPos s = new Source.t(s)
    let pos419 = new Source.Position(-419,0,0)

    let formatTermName termName = 
        String.collect (
            function
            | '>' -> "GREATER"
            | '=' -> "EQUAL"
            | '<' -> "LESS"
            | c   -> string (System.Char.ToUpper c) 
            )
            termName
    

    let formatNontermName (s:string) = (s.[0..0].ToLowerInvariant()) + s.[1..s.Length-1]
            
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
        nonTerminals |> List.map (fun (bnfTerm : BnfTerm) ->
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
                                                | :? NonTerminal as term -> PRef(new Source.t(formatNontermName term.Name, pos419, pos419,""), None)
                                                | :? Terminal as term -> PToken(new Source.t(formatTermName term.Name, pos419, pos419,""))
                                                | _ -> failwith "Not supported BnfTerm type"
                                                ;
                                        binding=None; 
                                        checker=None})) 
                                    (bnfTermList)), 
                                None,None)
                        match prOpt with
                        | Some(pr)  -> Some(PAlt(pr, pseq))
                        | None      -> Some(pseq) )
                    None
                    nt.Rule.Data
            match productionOpt with
            | Some(pr)  ->
                {
                    name = dummyPos (formatNontermName nt.Name);
                    args = [];
                    body = pr;
                    metaArgs = [];
                    isStart = (nt = (ironyGrammar.Root))
                    isPublic=false
                }
            | None      -> failwith "minimum 1 alternative is required"
        )
        |> defaultModules


