module GraphLabelledVertexTests

open System.IO
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open YaccConstructor.API
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

open System.Collections.Generic
open System.Linq

let dataDir = (__SOURCE_DIRECTORY__ + @"\..\data\AbstractGLL_LabelledVertices\")
let grammarsDir = __SOURCE_DIRECTORY__ + @"\"

let removeIdFromVert (s:string) = 
    let ind =  s.IndexOf("_")
    if ind <> -1
    then s.Substring(0, ind)
    else s

let getInputGraphVertLbl tokenizer inputFile startPos =    
    let edges = 
        File.ReadAllLines (dataDir + inputFile)
        |> Array.filter(fun x -> not (x = ""))
        |> Array.map (fun s -> let x = s.Split([|' '|])
                               x.[0], x.[1], x.[2])
    let edg (f : string) (t : string) (l : string) = 
        new TaggedEdge<_,_>(f, l, t)

    let g = new GraphLabelledVertex<_>(startPos, (fun x -> ((tokenizer (removeIdFromVert x)) |>int)))
    
    [|for (first, tag, last) in edges -> edg first tag last |]
    |> g.AddEdges
    |> ignore
    
    g 

let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate (grammarsDir + grammarFile)
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

let test grammarFile inputFile startPos nodesCount edgesCount termsCount ambiguityCount = 
    let conv = [new ExpandMeta()]
    let parser = getParserSource grammarFile conv
    let input  = getInputGraphVertLbl parser.StringToToken inputFile startPos
    let tree = buildAst parser input
//    printfn "%A" tree
//    tree.AstToDot (dataDir + inputFile + ".dot")
    let n, e, t, amb = tree.CountCounters
    //printfn "%d %d %d %d" n e t amb
    Assert.AreEqual(nodesCount, n, sprintf "Nodes expected:%i, found:%i." nodesCount n)
    Assert.AreEqual(edgesCount, e, sprintf "Edges expected:%i, found:%i." edgesCount e)
    Assert.AreEqual(termsCount, t, sprintf "Terms expected:%i, found:%i." termsCount t) 
    Assert.AreEqual(ambiguityCount, amb, sprintf "Ambiguities expected:%i, found:%i." ambiguityCount amb)
    Assert.Pass()

[<TestFixture>]
type ``GLL abstract parser GraphLabelledVertex tests``() =
    [<Test>]  
    member this._04_RightRecursionCheck () =
        test "RightRecursionCheck.yrd" 
             "RightRecursionCheck.txt"
             [|"P"|] 15 16 4 1
    
    [<Test>]  
    member this._01_PrettySimpleCalc_SequenceInput () =
        test "PrettySimpleCalc.yrd" 
             "PrettySimpleCalc.txt"
             [|"NUM_1"|] 15 14 3 0

    [<Test>]
    member this._06_NotAmbigousSimpleCalc_Loop () =
        test "NotAmbigousSimpleCalc.yrd" 
             "NotAmbigousSimpleCalc_Loop.txt"
             [|"NUM_1"|] 17 18 4 1

    [<Test>]
    member this._07_NotAmbigousSimpleCalc_LoopInLoop () =
        test "NotAmbigousSimpleCalc.yrd" 
             "NotAmbigousSimpleCalc_LoopInLoop.txt"
             [|"NUM_1"|] 27 29 7 2 

    [<Test>]
    member this._14_NotAmbigousSimpleCalcWith2Ops_Loop () =
        test "NotAmbigousSimpleCalcWith2Ops.yrd" 
             "NotAmbigousSimpleCalcWith2Ops_Loop.txt"
             [|"NUM_1"|] 31 31 8 1

    [<Test>]
    member this._25_UnambiguousBrackets_BiggerCircle () =
        test "StrangeBrackets.yrd" 
             "StrangeBrackets2.txt"
             [|"LBR_1"|] 30 32 8 2

    [<Test>]
    member this._29_Attrs () =
        test "Attrs.yrd" 
             "Attrs.txt"
             [|"A_1"|] 15 14 5 0

    [<Test>]
    member this._30_Condition () =
        test "Cond.yrd" 
             "Cond.txt"
             [|"IF"|] 42 47 5 1

    [<Test>]
    member this._31_Counter () =
        test "Counter.yrd" 
             "Counter.txt"
             [|"A_1"|] 35 40 5 0

    [<Test>]
    member this._35_Expression () =
        test "Expr.yrd" 
             "Expr.txt"
             [|"N_1"|] 24 27 5 1

    [<Test>]
    member this._36_First () =
        test "First.yrd" 
             "First.txt"
             [|"A_1"|] 15 14 5 0

    [<Test>]
    member this._38_LolCalc () =
        test "LolCalc.yrd" 
             "LolCalc.txt"
             [|"A_1"|] 115 174 11 10

    [<Test>]
    member this._41_Longest () =
        test "Longest.yrd" 
             "Longest.txt"
             [|"A_1"|] 24 25 6 0

    [<Test>]
    member this._43_Omit () =
        test "Omit.yrd" 
             "Omit.txt"
             [|"A_1"|] 22 20 4 0

    [<Test>]
    member this._45_SimpleRightRecursion () =
        test "SimpleRightRecursion.yrd" 
             "SimpleRightRecursion.txt"
             [|"B_1"|] 15 15 3 0

    [<Test>]
    member this._46_BadLeftRecursion () =
        test "BadLeftRecursion.yrd" 
             "BadLeftRecursion.txt"
             [|"B_1"|] 19 24 3 1

    [<Test>]
    member this._47_SimpleAmb () =
        test "SimpleAmb.yrd" 
             "SimpleAmb.txt"
             [|"A"|] 10 11 3 1

    [<Test>]
    member this._49_SimpleLeftRecursion () =
        test "SimpleLeftRecursion.yrd" 
             "SimpleLeftRecursion.txt"
             [|"B_1"|] 9 8 3 0
