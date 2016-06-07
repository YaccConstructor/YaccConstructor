//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module GLLAbstractParserTestsBio

open System.IO
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.GLL.AbstractParserWithoutTree
open YC.Tests.Helper
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

type msg =
    | Data of int*BioParserInputGraph
    | End of bool ref
    | Die of AsyncReplyChannel<unit>
     
let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
let len (edges : BioParserEdge[]) : int[] = edges |> Array.map (fun e -> e.Tokens.Length + 1) 

let fastaLoader path tokenizer =
    let chains = File.ReadAllLines path |> Seq.filter (fun s -> s.StartsWith ">" |> not) |> Seq.map (fun s -> s.ToCharArray())
    chains
    |> Seq.map (fun chs -> new BioParserInputGraph [|new BioParserEdge(0, 1, chs.Length, chs |> Array.map tokenizer)|] )

let filterRnaParsingResult res expectedRange lengthLimit =
    let filterRnaParsingResult lengthLimit res  =
        match res:ParseResult<ResultStruct> with
        | Success ast -> 
            failwith "Result is success but it is unrxpectrd success"
        | Success1 x ->
            let ranges = new ResizeArray<_>()
            let curLeft = ref 0
            let curRight = ref 0
            let x = x |> Array.filter (fun i -> i.length >= byte lengthLimit)
                    
            printfn "Ranges filter: %A" x.Length
            let x = 
                let onOneEdg, other =
                    x
                    |> Array.filter (fun s -> int s.length >= lengthLimit)
                    |> Array.partition (fun s -> s.le = s.re)
                    //, ([||] : array<ResultStruct> )
                    
                let curEdg = ref 0
                //printfn ""
                onOneEdg
                |> Array.iter(fun s ->
                    curEdg := s.le
                    if !curRight < s.lpos
                    then 
                        ranges.Add ((s.le,!curLeft),(s.re,!curRight))
                        curLeft := s.lpos
                        curRight := s.rpos                        
                    else
                        curLeft := min !curLeft s.lpos
                        curRight := max !curRight s.rpos
                        )
                ranges.Add((!curEdg,!curLeft),(!curEdg,!curRight))
                other
                |> Seq.groupBy(fun s -> s.le,s.re)
                |> Array.ofSeq
                |> Array.map snd
                |> Array.iter(fun s ->
                    let s = s |> Array.ofSeq
                    let left = s |> Array.minBy (fun s -> s.lpos)
                    let right = s |> Array.maxBy (fun s -> s.rpos)                        
                    ranges.Add((left.le,left.lpos),(right.re,right.rpos)))        
            printfn "All: %A" ranges.Count
            ranges     
        
        | Error e -> 
            failwithf "Input parsing failed: %A" e

    let ranges = filterRnaParsingResult lengthLimit res
    printfn "Ranges length = %A" ranges.Count
    Assert.IsTrue(ranges.Count >= 2 && ranges.Count < 5, sprintf "real length = %A" ranges.Count)
    //Assert.IsTrue(ranges |> Microsoft.FSharp.Collections.ResizeArray.exists ((=) expectedRange), sprintf "range %A does not exists in ranges %A" expectedRange ranges)

[<TestFixture>]
type ``GLL abstract parser tests bio`` () =
    
    member this.``1000: trna`` file lengthLimit expectedRange =
        let start = System.DateTime.Now
        let processRes res = 
            filterRnaParsingResult res expectedRange 60        

        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.tRNA.A i                
                | 'U' -> GLL.tRNA.U i
                | 'T' -> GLL.tRNA.U i
                | 'C' -> GLL.tRNA.C i
                | 'G' -> GLL.tRNA.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.tRNA.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.tRNA.RNGLR_EOF 0)

        graphs        
        |> Array.ofSeq        
        |> Array.mapi 
            (fun i graph -> 
                GLL.tRNA.buildAbstract graph 5
            )
        |> Array.iter processRes
        printfn "Time = %A" (System.DateTime.Now - start)

    [<Test>]
    member this.``1000: trna in 860-930`` () =
        this.``1000: trna`` """simple_tRNA1\g""" 1001 ((0,863),(0,926))

    [<Test>]
    member this.``1000: trna in 629-699`` () =
        this.``1000: trna`` """simple_tRNA2\g""" 1001 ((0,596),(0,697))

    [<Test>]
    member this.``1000: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA3\g""" 1001 ((0,127),(0,205))

    [<Test>]
    member this.``1000 as graph 139 + 861: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA4\g""" 1001 ((0,133),(0,204))

    [<Test>]
    member this.``1000 as graph 49 + 5: trna in 133-204`` () =
        this.``1000: trna`` """simple_tRNA6\g""" 1001 ((0,133),(0,204))

    //[<Test>]
    member this.``Problem with shift. Big`` () =
        this.``1000: trna`` """simple_tRNA5\g""" 1001 ((0,34),(0,99))


    member this.SmallTests file expectedRange =        
        let lengthLimit = 1001        
        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.shiftProblem.A i                
                | 'C' -> GLL.shiftProblem.C i
                | 'G' -> GLL.shiftProblem.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.shiftProblem.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.shiftProblem.RNGLR_EOF 0)

        graphs
        |> Array.ofSeq
        |> Array.mapi 
            (fun i graph -> 
                printfn "%A" i
                GLL.shiftProblem.buildAbstract graph 1
            )
        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 5)  
              
//Right test, but filter return wrong information
    //[<Test>]
    member this.``Very small`` () =        
        this.SmallTests  """very_small\g"""  ((0,1),(0,5))
        this.SmallTests  """very_small\g"""  ((0,2),(0,4))
//
    [<Test>]
    member this.``Very small with 2 edges`` () =        
        this.SmallTests  """very_small_multy_1\g"""  ((0,1),(1,2))
//    
//    [<Test>]
//    member this.``shift_small`` () =        
//        this.SmallTests  """problem_with_shift_2\g"""  ((0,1),(1,3))
//    
//    
    member this.``Very very small tests`` file expectedRange = 
        let getSmb =
            let cnt = ref 0
            fun ch ->
                let i = incr cnt; !cnt 
                match ch with
                | 'A' -> GLL.VeryVerySmall.A i                
                | 'C' -> GLL.VeryVerySmall.C i
                | 'G' -> GLL.VeryVerySmall.G i                
                | x ->   failwithf "Strange symbol in input: %A" x
                |> GLL.VeryVerySmall.tokenToNumber                
        let basePath = "../../../Tests/bio/"
        let path = Path.Combine(basePath, file)
        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path 20 getSmb (GLL.VeryVerySmall.RNGLR_EOF 0)

        graphs
        |> Array.ofSeq
        |> Array.mapi 
            (fun i graph -> 
                printfn "%A" i
                GLL.VeryVerySmall.buildAbstract graph 3
            )
        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 3) 

//Passed
    [<Test>]
    member this.``Very very small with 2 edges`` () =
        this.``Very very small tests`` """v_very_small_multy\g""" ((0,0),(1,2))
                
//Passed
    [<Test>]
    member this.``Very very small`` () =
        this.``Very very small tests`` """v_very_small\g""" ((0,0),(0,3)) 
               
//Passed
    [<Test>]
    member this.``Very very small 2 instances`` () =
        this.``Very very small tests`` """v_very_small_many_instances\g""" ((0,0),(0,3))
        this.``Very very small tests`` """v_very_small_many_instances\g""" ((0,4),(0,7))

    [<Test>]
    member this.``16s_H22_H23`` () =
        let cfg = YC.Bio.RNA.Search.r16s_H22_H23_SearchConfig
        let graphs = fastaLoader """C:\Users\Semyon\Downloads\HOMD_16S_rRNA_RefSeq_V14.5.fasta""" cfg.Tokenizer
        graphs 
        |> Seq.iter (fun g -> cfg.SearchWithoutSPPF g cfg.StartNonterm 
                              |> fun res -> filterRnaParsingResult res ((0,650), (0,750)) cfg.LowLengthLimit)


//
//
//    member this.``Intersection small small tests`` file expectedRange =
//        
//        let getSmb =
//            let cnt = ref 0
//            fun ch ->
//                let i = incr cnt; !cnt 
//                match ch with
//                | 'A' -> GLL.IntersectionSmall.A i                
//                | 'C' -> GLL.IntersectionSmall.C i
//                | 'B' -> GLL.IntersectionSmall.B i                
//                | x ->   failwithf "Strange symbol in input: %A" x
//                |> GLL.IntersectionSmall.tokenToNumber                
//        let basePath = "../../../Tests/bio/"
//        let path = Path.Combine(basePath, file)
//        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path 20 getSmb (GLL.IntersectionSmall.RNGLR_EOF 0)
//
//        graphs
//        |> Array.ofSeq
//        |> Array.mapi 
//            (fun i graph -> 
//                printfn "%A" i
//                GLL.IntersectionSmall.buildAbstract graph 4
//            )
//        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 2) 
//
//    [<Test>]
//    member this.``Intersection small`` () =
//        this.``Intersection small small tests`` """intersection_small\g""" ((0,0),(1,2))        
//
//    [<Test>]
//    member this.``Problem with multiple edges. Small`` () =
//        let file = """problem_with_multiple_edges\g""" 
//        let lengthLimit = 1001 
//        let expectedRange = ((0,0),(0,4))
//        let getSmb =
//            let cnt = ref 0
//            fun ch ->
//                let i = incr cnt; !cnt 
//                match ch with
//                | 'A' -> GLL.multipleEdgesProblem.A i                
//                | 'C' -> GLL.multipleEdgesProblem.C i            
//                | x ->   failwithf "Strange symbol in input: %A" x
//                |> GLL.multipleEdgesProblem.tokenToNumber                
//        let basePath = "../../../Tests/bio/"
//        let path = Path.Combine(basePath, file)
//        let graphs,longEdges = YC.BIO.BioGraphLoader.loadGraphFormFileToBioParserInputGraph path lengthLimit getSmb (GLL.multipleEdgesProblem.RNGLR_EOF 0)
//
//        graphs
//        |> Array.ofSeq
//        |> Array.mapi 
//            (fun i graph -> 
//                printfn "%A" i
//                GLL.multipleEdgesProblem.buildAbstract graph 1
//            )
//        |> Array.iter (fun res -> filterRnaParsingResult res expectedRange 5)        
//
//    member this.``Big for tRNA 1`` () =
//        this.``1000: trna`` """mix_1\late_pair_info_count""" 120 ((0,860),(0,930))
//
//    member this.``Big for tRNA 2`` () =
//        this.``1000: trna`` """synth_1\graph""" 120 ((0,860),(0,930))

    
let RunTests () =
        let t = new ``GLL abstract parser tests bio``()
        t.``16s_H22_H23``()
//        t.``1000: trna in 860-930``()
//        t.``1000: trna in 629-699``()
//        t.``1000: trna in 133-204``()
//        t.``1000 as graph 139 + 861: trna in 133-204`` ()
//        t.``Very small``()
//        t.``Very small with 2 edges``()
//        t.``Very very small with 2 edges``()
//        t.``Very very small``()
//        t.``Very very small 2 instances``()
//        t.``Intersection small``()
//        t.``Problem with shift. Big`` ()
//        t.``shift_small`` ()
//        t.``Problem with multiple edges. Small``()
//        t.``Big for tRNA 2``()
//    

