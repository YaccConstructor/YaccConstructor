// TableGenerator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace  Yard.Generators.RACC

open System.IO
open Yard.Core.IL.Definition
open Yard.Core.IL.Production
open Yard.Core.IL

type TableGenerator(outPath: string) = 
    class
        let outStrieam =         
            try
                let t = new FileInfo(outPath)
                let writer = t.CreateText()             
                writer     
            with e -> failwith ("Writer Exception:" + e.ToString())
                         
        let write (str:string) = outStrieam.WriteLine(str)

        let closeOutStream _ = outStrieam.Close()
         
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
            NLFAToDLFA.NLFAToDLFA (build production)
        

        let genearte (grammar:Yard.Core.IL.Grammar.t<_,_>) =            
            let DLFAList = List.map (fun (x:Rule.t<_,_>) -> buildDLFA x.body) grammar
            closeOutStream ()
                
        member self.Gemerate grammar = genearte grammar
                
    end

