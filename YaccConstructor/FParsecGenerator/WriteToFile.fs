module Yard.Generators.FParsecGenerator.WriteToFile

open System.IO
open System.Xml


let WriteFile (fn : string, txt : string) =
    let strm = new StreamWriter(fn)
    strm.Write(txt)
    strm.Close()