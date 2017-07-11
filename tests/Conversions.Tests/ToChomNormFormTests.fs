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

//let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/grammar.yrd") 
(*let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/gr1.yrd") 
Namer.initNamer loadIL.grammar
let result = loadIL |> applyConversion conversionChomNormForm*)
let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/GraphParsing.Test/GPPerf2_cnf.yrd")
Namer.initNamer loadIL.grammar
let result = loadIL |> applyConversion expandTopLevelAlt |> 
                    applyConversion expandEbnf |> applyConversion expandMeta |> applyConversion conversionChomNormForm

let rules = result.grammar.[0].rules
//for i in 0..rules.Length - 1 do
 //   printfn "%A" (rules.[i].name.text + ": " + rules.[i].body.ToString())
//printfn "%A" result 
System.Console.ReadKey() |> ignore



