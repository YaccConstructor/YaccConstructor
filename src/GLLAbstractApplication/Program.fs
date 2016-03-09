module GLLAbstractApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractAnalysis.Common
open YC.Tests.Helper
open Yard.Generators.GLL
open Yard.Generators.GLL.AbstractParser 
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open GLL.SimpleAmb
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.GLL.ParserCommon

//let outDir = @"../../../src/GLLAbstractApplication/"
//
//let run () =
//    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
//    let gen = new Yard.Generators.GLL.GLL()
//    let il = ref <| fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLAbstractApplication\SimpleAmb.yrd")
//    for constr in gen.Constraints do
//        let grammar = il.Value.grammar
//        if not <| constr.Check grammar then
//            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
//            il := {!il with grammar = constr.Fix grammar}
//
//    gen.Generate(!il,"-pos int -token int -abstract true -o SimpleAmb.yrd.fs")
//
//run () |> printfn "%A"

let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)



//let graphGenerator numberOfBlocks numberOfPath =
//    let final = 100
//    let qGraph = new ParserInputGraph<_>(0, final)
//    let mutable b = 1
//    let mutable e = 2
//    let mutable curB = 1
//    let mutable curE = 3
//    let chains = Array.zeroCreate 5
//    let ra1 = new ResizeArray<_>()
//    ra1.Add(GLL.SimpleAmb.DEC_NUMBER 0)
//    ra1.Add(GLL.SimpleAmb.L_plus_ 0)
//    ra1.Add(GLL.SimpleAmb.IDENT 0)
//    let ra2 = new ResizeArray<_>()
//    ra2.Add(GLL.SimpleAmb.IDENT 0)
//    ra2.Add(GLL.SimpleAmb.L_plus_ 0)
//    ra2.Add(GLL.SimpleAmb.IDENT 0)
//    let ra3 = new ResizeArray<_>()
//    ra3.Add(GLL.SimpleAmb.L_left_bracket_ 0)
//    ra3.Add(GLL.SimpleAmb.IDENT 0)
//    ra3.Add(GLL.SimpleAmb.L_plus_ 0)
//    ra3.Add(GLL.SimpleAmb.IDENT 0)
//    ra3.Add(GLL.SimpleAmb.L_right_bracket_ 0)
//    let ra4 = new ResizeArray<_>()
//    ra4.Add(GLL.SimpleAmb.L_null 0)
//    ra4.Add(GLL.SimpleAmb.L_null 0)
//    let ra5 = new ResizeArray<_>()
//    ra5.Add(GLL.SimpleAmb.STRING_CONST 0)
//    ra5.Add(GLL.SimpleAmb.L_plus_ 0)
//    ra5.Add(GLL.SimpleAmb.IDENT 0)
//    chains.[0] <- ra1
//    chains.[1] <- ra2
//    chains.[2] <- ra3
//    chains.[3] <- ra4
//    chains.[4] <- ra5    
//    (qGraph.AddVerticesAndEdge <| edg 0 1 (GLL.SimpleAmb.L_select 0)) |> ignore
//    for blocks = 0 to numberOfBlocks - 1 do
//        for i = 0 to numberOfPath - 1 do
//            let curChain = chains.[i]
//            for k = 0 to curChain.Count - 1 do
//                if k <> curChain.Count - 1 then
//                    qGraph.AddVerticesAndEdge <| edg curB curE (curChain.[k]) |> ignore  
//                    curB <- curE
//                    curE <- curE + 1
//                else
//                    qGraph.AddVerticesAndEdge <| edg curB e (curChain.[k]) |> ignore
//                    if i <> numberOfPath - 1 then
//                        curE <- curE
//                        curB <- b
//        if blocks <> numberOfBlocks - 1 then
//            b <- e
//            e <- curE               
//            qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.L_comma_ 0) |> ignore
//            b <- e
//            e <- e + 1
//            curB <- b
//            curE <- e + 1
//    b <- e
//    e <- curE               
//    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.L_from 0) |> ignore
//    b <- e
//    e <- e + 1
//    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.IDENT 0) |> ignore
//    b <- e
//    e <- e + 1
//    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.RNGLR_EOF 0) |> ignore
//    qGraph.FinalStates <- [|e|]
//    qGraph.PrintToDot "input.dot" (GLL.SimpleAmb.tokenToNumber >> GLL.SimpleAmb.numToString)
//    qGraph
    
let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
let len (edges : BioParserEdge<'token>[]) : int[] = edges |> Array.map (fun e -> e.Tokens.Length + 1) 
let edgB b e t = new BioParserEdge<_>(b, e, t) 

let inputGraph =
    
        let a1 = f [|GLL.SimpleAmb.A 1|] GLL.SimpleAmb.tokenToNumber
        let a2 = f [|GLL.SimpleAmb.C 1|] GLL.SimpleAmb.tokenToNumber
        let a3 = f [|GLL.SimpleAmb.B 1|] GLL.SimpleAmb.tokenToNumber
        let a4 = f [|GLL.SimpleAmb.RNGLR_EOF 2|] GLL.SimpleAmb.tokenToNumber
        let edges = [|
            edgB 0 1 a1;
            edgB 1 2 a2;
            edgB 1 2 a3;
            edgB 2 3 a4|] 
        let qGraph = new BioParserInputGraph<_>([|0|], 3, len edges, edges, 4)
        qGraph
let parser = GLL.SimpleAmb.buildAbstractAst
let r = parser inputGraph 10
match r with
| ParserCommon.ParseResult.Error _ ->
    printfn "Error"     
| ParserCommon.ParseResult.Success tree->
    printfn "%s" "sss"


//let f () =  
//    let parser = GLL.SimpleAmb.buildAbstractAst
//    for i = 0 to 100 do
//        let g = graphGenerator (1 + i) 2 
//        let start = System.DateTime.Now
//        let r = parser g
//        let finish = System.DateTime.Now - start
//        printfn "%i  : %A" (i+1) finish.TotalSeconds
//        match r with
//        | AbstractParser.Error _ ->
//            printfn "Error"     
//        | AbstractParser.Success tree->
//            ()//printfn "%s" "sss"
//    
//let th = new System.Threading.Thread(f, 10000000)
//th.Start()

