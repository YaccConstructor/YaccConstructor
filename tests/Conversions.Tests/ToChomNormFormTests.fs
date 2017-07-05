module ToChomNormFormTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Helpers
open Conversions.TransformAux
open NUnit.Framework
open ConversionsTests
open Yard.Core.Helpers

let basePath = System.IO.Path.Combine(conversionTestPath, "ToChomNormForm")
let path f = System.IO.Path.Combine(basePath, f)
      
let applyConversion (conversion:Conversion) loadIL = 
    {
        loadIL
            with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
    }

(*let dummyRule : elem<Source.t,Source.t> = {omit=false; binding=None; checker=None; rule=PToken (Source.t "DUMMY")}
let rules = 
    (verySimpleRules "e"
        [{dummyRule with rule = PToken (Source.t "A")}
         {dummyRule with rule = PRef (Source.t "yard_e_2", None)}]) 
   @(verySimpleNotStartRules "yard_e_1"
        [{dummyRule with rule = PRef (Source.t "a", None)}
         {dummyRule with rule = PToken (Source.t "B")}])
   @(verySimpleNotStartRules "yard_e_2"
        [{dummyRule with rule = PRef (Source.t "yard_e_1", None)}
         {dummyRule with rule = PRef (Source.t "e", None)}])*)

let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/longrule.yrd") //"C:/YaccConstructor/tests/GraphParsing.Test/GPPerf2_cnf.yrd")//"C:/YaccConstructor/tests/data/Conversions/ToCNF/longrule_0.yrd")
Namer.initNamer loadIL.grammar
let result = loadIL |> applyConversion conversionChomNormForm
printfn "%A" result 
System.Console.ReadKey() |> ignore



