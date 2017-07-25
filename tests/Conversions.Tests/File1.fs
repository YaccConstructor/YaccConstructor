module File1

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
let test = 
    (simpleRules "s"
         (PConj ((PSeq ([{dummyRule with rule = PToken (Source.t "A")}; 
                         {dummyRule with rule = PRef (Source.t "x", None)}; 
                         {dummyRule with rule = PToken (Source.t "B")} ],None,None)),
                 (PConj ((PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}; 
                                 {dummyRule with rule = PToken (Source.t "B")}; 
                                 {dummyRule with rule = PRef (Source.t "x", None)}], None, None)),
                        (PConj ((PNeg(PSeq ([{dummyRule with rule = PToken (Source.t "A")}; 
                                             {dummyRule with rule = PToken(Source.t "B")}], None, None))),
                               (PNeg (PSeq ([{dummyRule with rule = PRef (Source.t "x", None)}; 
                                             {dummyRule with rule = PToken(Source.t "C")};
                                             {dummyRule with rule = PRef (Source.t "y", None)}], None, None))))))))))
    @(simpleNotStartRules "x"
         (PConj (PSeq ([{dummyRule with rule = PToken (Source.t "A")}], None, None),
                 PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}], None, None))))
    @(simpleNotStartRules "x"
         (PConj ((PSeq ([{dummyRule with rule = PToken (Source.t "B")}; 
                         {dummyRule with rule = PRef (Source.t "y", None)}],None,None)),
                 (PConj ((PSeq ([{dummyRule with rule = PRef (Source.t "x", None)}; 
                                 {dummyRule with rule = PToken (Source.t "C")}], None, None)),
                        (PConj ((PSeq ([{dummyRule with rule = PToken (Source.t "A")}; 
                                        {dummyRule with rule = PToken(Source.t "B")}
                                        {dummyRule with rule = PToken(Source.t "C")}], None, None)),
                               (PNeg (PSeq ([{dummyRule with rule = PRef (Source.t "y", None)}; 
                                             {dummyRule with rule = PToken(Source.t "D")};
                                             {dummyRule with rule = PToken (Source.t "B")}], None, None))))))))))
    @(verySimpleNotStartRules "x"
        [])
    @(verySimpleNotStartRules "y"
        [{dummyRule with rule = PRef (Source.t "x", None)}
         {dummyRule with rule = PToken(Source.t "C")}
         {dummyRule with rule = PToken(Source.t "A")}])
    @(verySimpleNotStartRules "y"
        [{dummyRule with rule = PToken (Source.t  "C")}])
    @(verySimpleNotStartRules "z"
        [{dummyRule with rule = PRef (Source.t "x", None)}])
   

//let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/grammar.yrd") 
(*let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/grammar2.yrd") 
Namer.initNamer loadIL.grammar
let result = loadIL |> applyConversion conversionChomNormForm
let rules = result.grammar.[0].rules
for i in 0..rules.Length - 1 do
    printfn "%A" (rules.[i].name.text + ": " + rules.[i].body.ToString() + rules.[i].isStart.ToString())
////printfn "%A" result 
System.Console.ReadKey() |> ignore*)

//let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/data/Conversions/ToCNF/gr2.yrd") 
let loadIL = defaultDefinition test
//Namer.initNamer loadIL.grammar
let result = loadIL |> applyConversion conversionBinNormForm
//let loadIL = fe.ParseGrammar (path "C:/YaccConstructor/tests/GraphParsing.Test/GPPerf2_cnf.yrd")
//Namer.initNamer loadIL.grammar
(*for i in 0..loadIL.grammar.[0].rules.Length - 1 do
    printfn "%A" (loadIL.grammar.[0].rules.[i].name.text + ": " + loadIL.grammar.[0].rules.[i].body.ToString())*)
//let result = loadIL |> applyConversion expandEbnf |> applyConversion expandRepeat |> applyConversion expandMeta 
  //                  |> applyConversion expandInnerAlt |> applyConversion expandTopLevelAlt |>  applyConversion conversionChomNormForm

let rules = result.grammar.[0].rules
for i in 0..rules.Length - 1 do
    printfn "%A" (rules.[i].name.text + ": " + rules.[i].body.ToString())
System.Console.ReadKey() |> ignore

