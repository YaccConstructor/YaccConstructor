#r @"../../Bin/Release/v40/YC.RNGLR.dll"
#r @"../../Bin/Release/v40/YC.Common.dll"
#r @"../../Bin/Release/v40/YC.FsYaccFrontend.dll"
#r @"../../Bin/Release/v40/YC.YaccConstructor.exe"

open Yard.Generators.RNGLR
open Yard.Frontends.FsYaccFrontend
open YaccConstructor.API

let gen = new RNGLR()
let fe = new FsYaccFrontend()
let filename = "Parser.fsy"

let generate = 
    generateToFile filename
                   fe
                   gen
                   "Parser.fs"
