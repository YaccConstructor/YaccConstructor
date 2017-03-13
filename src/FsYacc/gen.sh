#!/usr/bin/env bash

mono ../../lkg/FSharp.PowerPack-1.9.7.7/bin/FsLex.exe --unicode --lexlib Microsoft.FSharp.Text.Lexing fsyacclex.fsl
mono ../../lkg/FSharp.PowerPack-1.9.7.7/bin/FsYacc.exe --internal --module FSharp.PowerPack.FsYacc.Parser --lexlib Microsoft.FSharp.Text.Lexing  --parslib Microsoft.FSharp.Text.Parsing fsyaccpars.fsy
