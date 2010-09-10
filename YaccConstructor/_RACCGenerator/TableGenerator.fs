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
         
        let buildDLFA rule =
            let rec build production =
                match production with
                | PSeq (seq,attr)   -> 
                    let automataLst = List.map (fun t -> build t.rule) seq  
                    let autConcat lAutom rAutom = 
                        {
                            NIDToStateMap = dict [] //lAutom.NIDToStateMap  rAutom.NIDToStateMap
                            NStartState   = lAutom.NStartState
                            NFinaleState  = rAutom.NFinaleState
                            NRules        = lAutom.NRules + rAutom.NRules 
                                            |> Set.add
                                                  {
                                                    FromStateID = lAutom.NFinaleState
                                                    ToStateID   = rAutom.NStartState
                                                    Label       = Omega
                                                    Symbol      = Epsilon
                                                  }
                        }

                    List.fold autConcat automataLst.Head automataLst.Tail

//                | PAlt (l,r)        -> 
  //              | PMany (expr)      ->
    //            | PSome (expr)      ->                            
      //          | PToken(ch)        ->          
                | x                 -> failwith ("You should support new elem " + x.ToString())
            1
        

        let genearte grammar=         
            closeOutStream ()
                
        member self.Gemerate grammar = genearte grammar
                
    end

