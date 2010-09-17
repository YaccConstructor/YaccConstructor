// TableGenerator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace  Yard.Generators._RACCGenerator

open Yard.Core.IL.Definition
open Yard.Core.IL.Production
open Yard.Core.IL

type TableGenerator(outPath: string) = 
    class

        let textWriter = TextWriter outPath                                
        let write str = textWriter.Write(str)        
         
        let buildDLFA production =
            let stateEnumerator = new Enumerator()
            let builder = new AtmBuilder(stateEnumerator)
            let rec build production =
                match production with
                | PSeq (seq,attr)   -> 
                    let automataLst = List.map (fun t -> build t.rule) seq                      
                    List.fold (fun x y -> builder.Concat x y Omega)  automataLst.Head automataLst.Tail
                    |> builder.AddInHead None Epsilon (FATrace TSeqS)
                    |> builder.Append None Epsilon (FATrace TSeqE)

                | PAlt (l,r)        -> 
                    let lAtm = build l
                    let rAtm = build r
                    builder.Alt lAtm rAtm (FATrace TAlt1S) (FATrace TAlt1E) (FATrace TAlt2S) (FATrace TAlt2E)

                | PSome (expr)      -> 
                    builder.Cls (build expr) (FATrace TClsS) (FATrace TClsE)

                | PRef(ch,_)
                | PToken(ch)        -> 
                    builder.Trivial None None (NSymbol (Source.toString ch)) Omega
                    |> builder.AddInHead None Epsilon (FATrace TSmbS)
                    |> builder.Append None Epsilon (FATrace TSmbE)

                | x                 -> failwith ("You should support elem " + x.ToString())

            NLFAToDLFA.NLFAToDLFA (build production) (fun x -> List.filter ((<>)Omega) x) 

        let goto items (dlfaMap:System.Collections.Generic.IDictionary<_,_>) =
            let cls q =
                let q' = ref q
                let l = ref 0
                while (!l < Set.count !q') do
                    l:= Set.count !q';
                    for (fa,st) in !q' 
                        do for (fa',st') in items
                               do 
                                let rule = List.filter (fun rule -> x) dlfaMap.[fa].Rules
                                if
                                then q':= Set.add (fa',st') !q'       
                
                !q'
            1
        
        let items dlfaMap =
            List.map 
                (fun (name,dlfa) -> 
                    Seq.map 
                        (fun stateID -> (name,stateID))
                        dlfa.DIDToStateMap.Keys)
                dlfaMap
            |> Seq.concat 
            |> List.ofSeq 
            
        let genearte (grammar:Yard.Core.IL.Definition.t<_,_>) =
            let dlfaMap = 
                List.map (fun (x:Rule.t<_,_>) ->x.name, buildDLFA x.body) grammar.grammar                            
            let str = "let autumataDict = \n" + ToString.dictToString (dict dlfaMap) + "\n"
            write str
            let items = items dlfaMap
            let str2 = "let items = \n" + ToString.listToString items + "\n"
            write str2
            textWriter.CloseOutStream ()
                
        member self.Gemerate grammar = genearte grammar
                
    end

