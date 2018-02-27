module BottomUpParsingViaAutomataIntersection.Test

open NUnit.Framework
open YC.Parsers.BottomUp
open System.Collections.Generic


let mutable basePath = @"..\..\..\data\RDF"

[<Ignore("Not finished")>]
[<TestFixture>]
type ``Tests for bottom-Up parser based on automata intersection``() =

    let rdfTokenizer str =
        match str with
        | "T"     -> 1
        | "TR"    -> 2
        | "SCO"   -> 3
        | "SCOR"  -> 4
        | "OTHER" -> 5
        | x -> failwithf "Unexpected edge label: %A" x

    let rdfToDot () =
        let files = System.IO.Directory.GetFiles(basePath,"*")
        for file in files do 
            let parserInputGraph,_ = YC.GLL.Abstarct.Tests.RDFPerformance.getParseInputGraph rdfTokenizer file
            parserInputGraph.PrintToDot (System.IO.Path.GetFileNameWithoutExtension file + ".dot") string
        0

    let getRDFInput file = 
        
        let path file = System.IO.Path.Combine(basePath, file)
        let parserInputGraph,triples = YC.GLL.Abstarct.Tests.RDFPerformance.getParseInputGraph rdfTokenizer (path file)
        let input = Array2D.init parserInputGraph.EdgeCount parserInputGraph.EdgeCount (fun i j -> new HashSet<_>())
        parserInputGraph.Edges
        |> Seq.iter (fun e -> input.[e.Source,e.Target].Add e.Tag |> ignore)
        input


    let getRNAinput file =
        let input = Array2D.init 512 512 (fun i j -> new HashSet<_>())
        (System.IO.File.ReadAllText file).Substring(330,511).ToLower()
        |> Seq.map (fun ch -> match ch with 'a' -> 1 | 'u' -> 2 | 'c' -> 3 | 'g' -> 4 | x -> failwithf "Strange input %A" x)
        |> Seq.iteri (fun i k -> input.[i, i + 1].Add k |> ignore)
        input

    // s0 = 5
    // st1 = 6
    // st2 = 7
    // stem = 8
    // any_0_7 = 9
    let grammar_16s =
        let g = Array2D.init 31 31 (fun i j -> new HashSet<_>())
        //any_0_7
        g.[0,1].Add 1 |> ignore
        g.[0,1].Add 2 |> ignore
        g.[0,1].Add 3 |> ignore
        g.[0,1].Add 4 |> ignore

        g.[1,2].Add 1 |> ignore
        g.[1,2].Add 2 |> ignore
        g.[1,2].Add 3 |> ignore
        g.[1,2].Add 4 |> ignore

        g.[2,3].Add 1 |> ignore
        g.[2,3].Add 2 |> ignore
        g.[2,3].Add 3 |> ignore
        g.[2,3].Add 4 |> ignore

        g.[3,4].Add 1 |> ignore
        g.[3,4].Add 2 |> ignore
        g.[3,4].Add 3 |> ignore
        g.[3,4].Add 4 |> ignore

        g.[4,5].Add 1 |> ignore
        g.[4,5].Add 2 |> ignore
        g.[4,5].Add 3 |> ignore
        g.[4,5].Add 4 |> ignore

        g.[5,6].Add 1 |> ignore
        g.[5,6].Add 2 |> ignore
        g.[5,6].Add 3 |> ignore
        g.[5,6].Add 4 |> ignore

        g.[6,7].Add 1 |> ignore
        g.[6,7].Add 2 |> ignore
        g.[6,7].Add 3 |> ignore
        g.[6,7].Add 4 |> ignore

        // s0
        g.[28,29].Add 9 |> ignore
        g.[29,30].Add 8 |> ignore
        //g.[30,31].Add 5 |> ignore

        //stems
        g.[8,9].Add 1 |> ignore
        g.[8,10].Add 2 |> ignore
        g.[8,11].Add 3 |> ignore
        g.[8,12].Add 4 |> ignore
        
        g.[9,13].Add 6 |> ignore
        g.[9,14].Add 8 |> ignore
        g.[9,15].Add 5 |> ignore

        g.[10,16].Add 6 |> ignore
        g.[10,17].Add 8 |> ignore
        g.[10,18].Add 5 |> ignore

        g.[11,19].Add 6 |> ignore
        g.[11,20].Add 8 |> ignore
        g.[11,21].Add 5 |> ignore

        g.[12,22].Add 6 |> ignore
        g.[12,23].Add 8 |> ignore
        g.[12,24].Add 5 |> ignore


        g.[13,25].Add 2 |> ignore
        g.[16,25].Add 1 |> ignore
        g.[19,25].Add 4 |> ignore
        g.[22,25].Add 3 |> ignore

        g.[15,26].Add 2 |> ignore
        g.[18,26].Add 1 |> ignore
        g.[21,26].Add 4 |> ignore
        g.[24,26].Add 3 |> ignore

        g.[14,27].Add 2 |> ignore
        g.[17,27].Add 1 |> ignore
        g.[20,27].Add 4 |> ignore
        g.[23,27].Add 3 |> ignore

        g.[8,27].Add 7 |> ignore
                
        let states = 
            [|
                (9, new HashSet<_>([0]), new HashSet<_>([0;1;2;3;4;5;6;7]))                
                (5, new HashSet<_>([28]), new HashSet<_>([29;30]))
                (6, new HashSet<_>([8]), new HashSet<_>([25]))
                (7, new HashSet<_>([8]), new HashSet<_>([26]))
                (8, new HashSet<_>([8]), new HashSet<_>([27]))
            |]
        new Grammar (g, states)


    // s -> SCOR SCO | TR T | SCOR s SCO | TR s T 
    // 6 s
    let rdfQuery1 =
        let g = Array2D.init 6 6 (fun i j -> new HashSet<_>())
        g.[0,1].Add 4 |> ignore
        g.[1,2].Add 6 |> ignore
        g.[2,3].Add 3 |> ignore
        g.[1,3].Add 3 |> ignore
        g.[0,4].Add 2 |> ignore
        g.[4,5].Add 6 |> ignore
        g.[5,3].Add 1 |> ignore
        g.[4,3].Add 1 |> ignore
        let states = [|(6, new HashSet<_>([0]), new HashSet<_>([3]))|]
        new Grammar (g, states)

    // s -> b SCO | SCO
    // b -> SCOR b SCO | SCOR SCO
    // 6 s 
    // 7 b
    let rdfQuery2 =
        let g = Array2D.init 7 7 (fun i j -> new HashSet<_>())
        g.[0,1].Add 7 |> ignore
        g.[1,2].Add 3 |> ignore
        g.[0,2].Add 3 |> ignore
        g.[3,4].Add 4 |> ignore
        g.[4,5].Add 7 |> ignore
        g.[5,6].Add 3 |> ignore
        g.[4,6].Add 3 |> ignore
        let states = 
            [|
                (6, new HashSet<_>([0]), new HashSet<_>([2]))
                (7, new HashSet<_>([3]), new HashSet<_>([6]))
            |]
        new Grammar (g, states)
    
    // S -> a S b | eps
    // 1 a
    // 2 b
    // 3 S
    let grammar1 = 
        let g = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        g.[0,1].Add 1 |> ignore
        g.[1,2].Add 3 |> ignore
        g.[2,3].Add 2 |> ignore
        let states = [|(3, new HashSet<_>([0]), new HashSet<_>([0;3]))|]
        new Grammar (g, states)

    // E -> E (+|*E) | n
    // 1 +
    // 2 *
    // 3 E
    // 4 n
    let grammar2 = 
        let g = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        g.[0,1].Add 3 |> ignore
        g.[1,2].Add 1 |> ignore
        g.[1,2].Add 2 |> ignore
        g.[2,3].Add 3 |> ignore
        g.[0,3].Add 4 |> ignore
        let states = [|(3, new HashSet<_>([0]), new HashSet<_>([3]))|]
        new Grammar (g, states)    

    let testRNA () =
        let input = getRNAinput @"C:\gsv\projects\YC\YaccConstructor\testRNA1"
        let s = System.DateTime.Now
        let res = main input grammar_16s
        (System.DateTime.Now - s).TotalMilliseconds
        |> printfn "flie: %A time = %A" "111"
        let startNonTermCount = 
            let i = ref 0
            res |> Array2D.iteri (fun j k s -> 
                                     if s.Contains 5 
                                     then 
                                           incr i
                                //           printfn "%A,%A" j k 
                                )
            !i
        printfn "%A" startNonTermCount
//        for i in 0 .. 511 do
//            printfn ""
//            for j in 0 .. 511 do
//             if res.[i,j].Contains 5 
//             then printf "1;"
//             else printf "0;"

    let testRDF file query expectedResult = 
        let input = getRDFInput file
        let s = System.DateTime.Now
        let output = main input query
        (System.DateTime.Now - s).TotalMilliseconds
        |> printfn "flie: %A time = %A" file
        let startNonTermCount = 
            let i = ref 0
            output |> Array2D.iter (fun s -> if s.Contains 6 then incr i)
            !i
        printfn "%A" startNonTermCount
        Assert.AreEqual(expectedResult, startNonTermCount)

    member this.RdftoDot() = //rdfToDot()
        let getParserSource = 
            let grm = @"C:\gsv\projects\YC\YaccConstructor\1.yrd"
            let fe = new Yard.Frontends.YardFrontend.YardFrontend()
            let gen = new Yard.Generators.GLL.GLL()
            YC.API.generate (grm)
                     fe gen 
                     None
                     Seq.empty
                     [|""|]
                     [] :?> Yard.Generators.GLL.ParserCommon.ParserSourceGLL

        let gll = new Yard.Generators.GLL.GLL()
        let input = new AbstractAnalysis.Common.SimpleInputGraph<_>([|0|],[|0|],id)
        let batch i j =
            [
                new AbstractAnalysis.Common.ParserEdge<_>(i, j, int <| getParserSource.StringToToken "A")
                new AbstractAnalysis.Common.ParserEdge<_>(i, j, int <| getParserSource.StringToToken "B")
                new AbstractAnalysis.Common.ParserEdge<_>(i, j, int <| getParserSource.StringToToken "C")
                new AbstractAnalysis.Common.ParserEdge<_>(i, j, int <| getParserSource.StringToToken "D")
                ]
        input.AddVerticesAndEdgeRange(batch 0 0)
        //input.AddVerticesAndEdgeRange(batch 1 2)
        //input.AddVerticesAndEdgeRange(batch 2 3)
        //input.AddVerticesAndEdgeRange(batch 3 4)
        //input.AddVerticesAndEdgeRange(batch 4 5)
        //input.AddVerticesAndEdgeRange(batch 5 6)
        let ast = Yard.Generators.GLL.AbstractParser.buildAst getParserSource input
        ast.AstToDot "outSigmaStar.dot"


    [<OneTimeSetUp>]
    member this.Init () =
        basePath <- @"./data/RDF"

    [<Test>]
    member this._00_RNA() = 
        testRNA()

    [<Test>]
    member this._01_BracketsLinear() = 
        let input = Array2D.init 5 5 (fun i j -> new HashSet<_>())
        input.[0,1].Add 1 |> ignore
        input.[1,2].Add 1 |> ignore
        input.[2,3].Add 2 |> ignore
        input.[3,4].Add 2 |> ignore
        let output = main input grammar1
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,0].Contains 3)
        Assert.IsTrue(output.[1,1].Contains 3)
        Assert.IsTrue(output.[2,2].Contains 3)
        Assert.IsTrue(output.[3,3].Contains 3)
        Assert.IsTrue(output.[4,4].Contains 3)
        Assert.IsTrue(output.[0,4].Contains 3)

    [<Test>]
    member this._02_BracketsCycle() = 
        let input = Array2D.init 2 2 (fun i j -> new HashSet<_>())
        input.[0,0].Add 1 |> ignore
        input.[0,1].Add 2 |> ignore
        input.[1,0].Add 2 |> ignore 
        let output = main input grammar1
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,0].Contains 3)
        Assert.IsTrue(output.[1,1].Contains 3)
        Assert.IsTrue(output.[0,1].Contains 3)
        Assert.IsFalse(output.[1,0].Contains 3)

    [<Test>]
    member this._03_ArithGraph() = 
        let input = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        input.[0,1].Add 4 |> ignore
        input.[1,2].Add 1 |> ignore
        input.[2,3].Add 4 |> ignore
        input.[3,0].Add 2 |> ignore 
        let output = main input grammar2
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,1].Contains 3)
        Assert.IsTrue(output.[2,3].Contains 3)
        Assert.IsTrue(output.[0,3].Contains 3)
        Assert.IsTrue(output.[2,1].Contains 3)
        let startNonTermCount = 
            let i = ref 0
            output |> Array2D.iter (fun s -> if s.Contains 3 then incr i)
            !i
        Assert.AreEqual(4, startNonTermCount)

    [<Test>]
    member this._04_RDF_foaf_q1 () =
        testRDF "foaf.rdf" rdfQuery1 4118

    [<Test>]
    member this._05_RDF_wine_q1 () =
        testRDF "wine.rdf" rdfQuery1 66572

    [<Test>]
    member this._06_RDF_foaf_q2 () =
        testRDF "foaf.rdf" rdfQuery2 10

    [<Test>]
    member this._07_RDF_wine_q2 () =
        testRDF "wine.rdf" rdfQuery2 133

    [<Test>]
    member this._08_RDF_skos_q2 () =
        testRDF "skos.rdf" rdfQuery2 1

    [<Test>]
    member this._09_RDF_generations_q2 () =
        testRDF "generations.owl" rdfQuery2 0

    [<Test>]
    member this._10_RDF_travel_q2 () =
        testRDF "travel.owl" rdfQuery2 63

    [<Test>]
    member this._11_RDF_univ_bench_q2 () =
        testRDF "univ-bench.owl" rdfQuery2 81

    [<Test>]
    member this._12_RDF_atom_primitive_q2 () =
        testRDF "atom-primitive.owl" rdfQuery2 122

    [<Test>]
    member this._13_RDF_biomedical_measure_primitive_q2 () =
        testRDF "biomedical-mesure-primitive.owl" rdfQuery2 2871

    [<Test>]
    member this._14_RDF_people_pets_q2 () =
        testRDF "people_pets.rdf" rdfQuery2 37

    [<Test>]
    member this._15_RDF_funding_q2 () =
        testRDF "funding.rdf" rdfQuery2 1158

    [<Test>]
    member this._16_RDF_skos_q1 () =
        testRDF "skos.rdf" rdfQuery1 810

    [<Test>]
    member this._17_RDF_generations_q1 () =
        testRDF "generations.owl" rdfQuery1 2164

    [<Test>]
    member this._18_RDF_travel_q1 () =
        testRDF "travel.owl" rdfQuery1 2499

    [<Test>]
    member this._19_RDF_univ_bench_q1 () =
        testRDF "univ-bench.owl" rdfQuery1 2540

    [<Test>]
    member this._20_RDF_atom_primitive_q1 () =
        testRDF "atom-primitive.owl" rdfQuery1 15454

    [<Test>]
    member this._21_RDF_biomedical_measure_primitive_q1 () =
        testRDF "biomedical-mesure-primitive.owl" rdfQuery1 15156

    [<Test>]
    member this._22_RDF_people_pets_q1 () =
        testRDF "people_pets.rdf" rdfQuery1 9472

    [<Test>]
    member this._23_RDF_funding_q1 () =
        testRDF "funding.rdf" rdfQuery1 17634
        
            
[<EntryPoint>]
let f x =
    let t = new ``Tests for bottom-Up parser based on automata intersection``()
    t._00_RNA()
    //t._07_RDF_wine_q2 ()
    for i in 0 .. 5 do
        t._00_RNA()
    //t.RdftoDot ()
    0 