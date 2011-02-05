//  TableGenerator.fs contains functions for tables generation (goto, items, LFA data, etc)
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace  Yard.Generators.RACCGenerator

open Yard.Core.IL.Definition
open Yard.Core.IL.Production
open Yard.Core.IL
open Yard.Core.IL.Rule

type TableGenerator(outPath: string) = 
    class

        let textWriter = TextWriter outPath                                
        let write str = textWriter.Write(str)

        let enumerator = new Enumerator()
         
        let buildDLFA production =
            let stateEnumerator = new Enumerator()
            let builder = new AtmBuilder(stateEnumerator)
            let rec build production =
                match production with
                | PSeq (seq,attr)   -> 
                    let automataLst = List.map (fun t -> build t.rule) seq
                    let seqNum = enumerator.Next()
                    List.fold (fun x y -> builder.Concat x y Omega)  automataLst.Head automataLst.Tail
                    |> builder.AddInHead None Epsilon (FATrace (TSeqS seqNum))
                    |> builder.Append None Epsilon (FATrace (TSeqE seqNum))

                | PAlt (l,r)        -> 
                    let lAtm = build l
                    let rAtm = build r
                    let alt1Num, alt2Num = enumerator.Next(), enumerator.Next()
                    builder.Alt lAtm rAtm 
                        (FATrace (TAlt1S alt1Num)) (FATrace (TAlt1E alt1Num)) 
                        (FATrace (TAlt2S alt2Num)) (FATrace (TAlt2E alt2Num))

                | PSome (expr)     ->
                    let clsNum = enumerator.Next()
                    let atm = build expr                    
                    builder.Cls (build expr) Omega Omega
                    |> fun x -> builder.Concat atm x Omega
                    |> builder.AddInHead None Epsilon (FATrace (TClsS clsNum))
                    |> builder.Append None Epsilon (FATrace (TClsE clsNum))

                | PMany (expr)      -> 
                    let clsNum = enumerator.Next()
                    builder.Cls (build expr) (FATrace (TClsS clsNum)) (FATrace (TClsE clsNum))

                | POpt (expr)        ->
                    let clsNum = enumerator.Next()
                    builder.Opt (build expr) (FATrace (TOptS clsNum)) (FATrace (TOptE clsNum))

                | PRef (ch,_)
                | PToken (ch)        -> 
                    let smbNum = enumerator.Next()
                    builder.Trivial None None (NSymbol (Source.toString ch)) Omega
                    |> builder.AddInHead None Epsilon (FATrace (TSmbS smbNum))
                    |> builder.Append None Epsilon (FATrace (TSmbE smbNum))

                | x                 -> failwith ("You should support elem " + x.ToString())

            NLFAToDLFA.NLFAToDLFA (build production) (fun x -> List.filter ((<>)Omega) x) 

        let goto items (dlfaMap:System.Collections.Generic.IDictionary<_,DLFA<_,_,_>>) =
            let cls q =
                let q' = ref q
                let l = ref 0
                while (!l < Set.count !q') do
                    l:= Set.count !q';
                    for (fa,st) in !q' 
                        do for (fa',st') in items
                               do 
                                let rules = Set.filter (fun rule -> rule.FromStateID = st) (dlfaMap.[fa]).DRules
                                let isEq rule = 
                                    let symbStr = 
                                        match rule.Symbol with
                                        |DSymbol(s) -> s
                                        | _         -> ""
                                    fa' = symbStr
                                if (Set.exists isEq rules)&&((dlfaMap.[fa]).DStartState = st')
                                then q':= Set.add (fa',st') !q'                       
                !q'

            let symbols = 
                Seq.map (fun key -> (dlfaMap.[key]).DRules) dlfaMap.Keys
                |> Seq.concat
                |> Seq.map (fun rule -> rule.Symbol)
                |> Set.ofSeq
            
            Set.map 
                (fun smb ->
                    List.map
                        (fun item -> 
                            cls (Set.singleton item)
                            |> Set.map 
                                (fun elt -> 
                                    dlfaMap.[fst elt].DRules
                                    |> Set.filter (fun rule -> 
                                                    rule.FromStateID = snd elt
                                                    && rule.Symbol = smb)                                              
                                    |> Set.map (fun rule -> hash (item, smb), (fst elt, rule.ToStateID)))                                    
                            |>Set.unionMany)                        
                        items
                        |>Set.unionMany)
                symbols
            |> Set.unionMany
            |> fun a -> 
                a
                |> Set.map (fun (x,y) -> x)
                |> Set.map 
                    (fun x -> x, Set.filter (fun (a,b) -> x = a) a |> Set.map snd)
            
        
        let items dlfaMap =
            List.map 
                (fun (name,dlfa) -> 
                    Seq.map 
                        (fun stateID ->                         
                            name,stateID)
                        dlfa.DIDToStateMap.Keys)
                dlfaMap
            |> Seq.concat 
            |> List.ofSeq 
            
        let generatePreheader grammarName =
            write "//this tables was generated by RACC"
            write ("//source grammar:" + grammarName )
            write ("//date:" + System.DateTime.Now.ToString())
            write ("")
            write ("#light \"off\"")
            write ("module Yard.Generators.RACCGenerator.Tables")
            write ("")
            write ("open Yard.Generators.RACCGenerator")
            write ("")


        let genearte grammar =
            generatePreheader grammar.info.fileName
            let publicRule = List.find (fun rule -> rule._public) grammar.grammar
            let startRule = 
                {
                    name    = Constants.raccStartRuleName
                    args    = []
                    body    = PRef((publicRule.name,(0,0)),None)
                    _public = true
                    metaArgs= []
                }
            let dlfaMap = 
                startRule :: grammar.grammar
                |> List.map (fun x -> x.name, buildDLFA x.body)
            "let autumataDict = \n" + ToString.dictToString (dict dlfaMap) + "\n"
            |> write
            let items = items dlfaMap
            let goto = goto items (dict dlfaMap)
            "let gotoSet = \n    " + ToString.setToString goto + "\n    |> dict"
            |> write
            textWriter.CloseOutStream ()
                
        member self.Gemerate grammar = genearte grammar

    end