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

type TableGenerator(outPath: string) = 
    class
        let outStrieam =         
            try
                let t = new FileInfo(outPath)
                let writer = t.CreateText()             
                writer     
            with e -> failwith ("Writer Exception:" + e.ToString())
                         
        let write (str:string) = outStrieam.WriteLine(str)

        let closeOutStream = outStrieam.Close()
         
        
        let genearte grammar=         
            closeOutStream
                
        member self.Gemerate grammar = genearte grammar
                
    end

