module GLLAbstractApplication
open System.IO
open System
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open GLL.aa

//open RNGLR.SimpleAmb
////let outDir = @"../../../src/GLLAbstractApplication/"
////
////let run () =
////    let fe = new Yard.Frontends.YardFrontend.YardFrontend()
////    let gen = new Yard.Generators.GLL.GLL()
////    let il = ref <| fe.ParseGrammar(@"C:\Users\User\recursive-ascent\src\GLLAbstractApplication\SimpleAmb.yrd")
////    for constr in gen.Constraints do
////        let grammar = il.Value.grammar
////        if not <| constr.Check grammar then
////            eprintfn "Constraint %s: applying %s..." constr.Name constr.Conversion.Name
////            il := {!il with grammar = constr.Fix grammar}
////
////    gen.Generate(!il,"-pos int -token int -abstract true -o SimpleAmb.yrd.fs")
////
////run () |> printfn "%A"
//
//let lbl tokenId = tokenId
//let edg f t l = new ParserEdge<_>(f,t,lbl l)
//
////let graphGenerator numberOfBlocks numberOfPath =
////    let final = 100
////    let qGraph = new ParserInputGraph<_>(0, final)
////    let mutable b = 1
////    let mutable e = 2
////    let mutable curB = 1
////    let mutable curE = 3
////    let chains = Array.zeroCreate 5
////    let ra1 = new ResizeArray<_>()
////    ra1.Add(GLL.SimpleAmb.DEC_NUMBER 0)
////    ra1.Add(GLL.SimpleAmb.L_plus_ 0)
////    ra1.Add(GLL.SimpleAmb.IDENT 0)
////    let ra2 = new ResizeArray<_>()
////    ra2.Add(GLL.SimpleAmb.IDENT 0)
////    ra2.Add(GLL.SimpleAmb.L_plus_ 0)
////    ra2.Add(GLL.SimpleAmb.IDENT 0)
////    let ra3 = new ResizeArray<_>()
////    ra3.Add(GLL.SimpleAmb.L_left_bracket_ 0)
////    ra3.Add(GLL.SimpleAmb.IDENT 0)
////    ra3.Add(GLL.SimpleAmb.L_plus_ 0)
////    ra3.Add(GLL.SimpleAmb.IDENT 0)
////    ra3.Add(GLL.SimpleAmb.L_right_bracket_ 0)
////    let ra4 = new ResizeArray<_>()
////    ra4.Add(GLL.SimpleAmb.L_null 0)
////    ra4.Add(GLL.SimpleAmb.L_null 0)
////    let ra5 = new ResizeArray<_>()
////    ra5.Add(GLL.SimpleAmb.STRING_CONST 0)
////    ra5.Add(GLL.SimpleAmb.L_plus_ 0)
////    ra5.Add(GLL.SimpleAmb.IDENT 0)
////    chains.[0] <- ra1
////    chains.[1] <- ra2
////    chains.[2] <- ra3
////    chains.[3] <- ra4
////    chains.[4] <- ra5    
////    (qGraph.AddVerticesAndEdge <| edg 0 1 (GLL.SimpleAmb.L_select 0)) |> ignore
////    for blocks = 0 to numberOfBlocks - 1 do
////        for i = 0 to numberOfPath - 1 do
////            let curChain = chains.[i]
////            for k = 0 to curChain.Count - 1 do
////                if k <> curChain.Count - 1 then
////                    qGraph.AddVerticesAndEdge <| edg curB curE (curChain.[k]) |> ignore  
////                    curB <- curE
////                    curE <- curE + 1
////                else
////                    qGraph.AddVerticesAndEdge <| edg curB e (curChain.[k]) |> ignore
////                    if i <> numberOfPath - 1 then
////                        curE <- curE
////                        curB <- b
////        if blocks <> numberOfBlocks - 1 then
////            b <- e
////            e <- curE               
////            qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.L_comma_ 0) |> ignore
////            b <- e
////            e <- e + 1
////            curB <- b
////            curE <- e + 1
////    b <- e
////    e <- curE               
////    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.L_from 0) |> ignore
////    b <- e
////    e <- e + 1
////    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.IDENT 0) |> ignore
////    b <- e
////    e <- e + 1
////    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.RNGLR_EOF 0) |> ignore
////    qGraph.FinalStates <- [|e|]
////    qGraph.PrintToDot "input.dot" (GLL.SimpleAmb.tokenToNumber >> GLL.SimpleAmb.numToString)
////    qGraph
////    
////let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
////let edgB b e t = new ParserEdge<_>(b, e, t) 
////
////let inputGraph =
////    
////        let a1 = f [|GLL.SimpleAmb.A 1|] GLL.SimpleAmb.tokenToNumber
////        let a2 = f [|GLL.SimpleAmb.C 1|] GLL.SimpleAmb.tokenToNumber
////        let a3 = f [|GLL.SimpleAmb.B 1|] GLL.SimpleAmb.tokenToNumber
////        let a4 = f [|GLL.SimpleAmb.RNGLR_EOF 2|] GLL.SimpleAmb.tokenToNumber
////        let edges = [|
////            edgB 0 1 a1;
////            edgB 1 2 a2;
////            edgB 1 2 a3;
////            edgB 2 3 a4|] 
////        let qGraph = new ParserInputGraph<_>([|0|], 3, edges, 4))
////        qGraph
////let parser = GLL.SimpleAmb.buildAbstractAst
////let r = parser inputGraph 10
////match r with
////| ParserCommon.ParseResult.Error _ ->
////    printfn "Error"     
////| ParserCommon.ParseResult.Success tree->
////    printfn "%s" "sss"
////    printfn "%s" "sss"
//    let final = 100
//    let qGraph = new ParserInputGraph<_>(0, final)
//    let mutable b = 0
//    let mutable e = 1
//    for blocks = 0 to numberOfBlocks - 1 do
//        qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.B 0) |> ignore
//        b <- e
//        e <- e + 1     
//    qGraph.AddVerticesAndEdge <| edg b e (GLL.SimpleAmb.RNGLR_EOF 0) |> ignore
//    qGraph.FinalStates <- [|e|]
//    qGraph.PrintToDot "input.dot" (GLL.SimpleAmb.tokenToNumber >> GLL.SimpleAmb.numToString)
//    qGraph
//    qGraph.FinalStates <- [|e|]
//    qGraph.PrintToDot "input.dot" (RNGLR.SimpleAmb.tokenToNumber >> GLL.SimpleAmb.numToString)
//
////let f () =  
////System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.LowLatency
////for i = 50 to 150 do
//let g = graphGenerator (100) 
////let g2 = graphGenerator2 (100) 
//let start = System.DateTime.Now
//let r = parser g
//let finish = System.DateTime.Now - start
//printfn " : %A"  finish.TotalSeconds
//let start2 = System.DateTime.Now
////let r2 = parser2 g2
//let finis2h = System.DateTime.Now - start
//printfn " : %A"  finish.TotalSeconds
//    
////let th = new System.Threading.Thread(f, 10000000)
////th.Start()
//
////let graphGenerator2 numberOfBlocks numberOfPath =
////    let final = 100
////    let qGraph = new ParserInputGraph<_>(0, final)
////    let mutable b = 1
////    let mutable e = 2
////    let mutable curB = 1
////    let mutable curE = 3
////    let chains = Array.zeroCreate 5
////    let ra1 = new ResizeArray<_>()
////    ra1.Add(RNGLR.SimpleAmb.DEC_NUMBER 0)
////    ra1.Add(RNGLR.SimpleAmb.L_plus_ 0)
////    ra1.Add(RNGLR.SimpleAmb.IDENT 0)
////    let ra2 = new ResizeArray<_>()
////    ra2.Add(RNGLR.SimpleAmb.IDENT 0)
////    ra2.Add(RNGLR.SimpleAmb.L_plus_ 0)
////    ra2.Add(RNGLR.SimpleAmb.IDENT 0)
////    let ra3 = new ResizeArray<_>()
////    ra3.Add(RNGLR.SimpleAmb.L_left_bracket_ 0)
////    ra3.Add(RNGLR.SimpleAmb.IDENT 0)
////    ra3.Add(RNGLR.SimpleAmb.L_plus_ 0)
////    ra3.Add(RNGLR.SimpleAmb.IDENT 0)
////    ra3.Add(RNGLR.SimpleAmb.L_right_bracket_ 0)
////    let ra4 = new ResizeArray<_>()
////    ra4.Add(RNGLR.SimpleAmb.L_null 0)
////    ra4.Add(RNGLR.SimpleAmb.L_null 0)
////    let ra5 = new ResizeArray<_>()
////    ra5.Add(RNGLR.SimpleAmb.STRING_CONST 0)
////    ra5.Add(RNGLR.SimpleAmb.L_plus_ 0)
////    ra5.Add(RNGLR.SimpleAmb.IDENT 0)
////    chains.[0] <- ra1
////    chains.[1] <- ra2
////    chains.[2] <- ra3
////    chains.[3] <- ra4
////    chains.[4] <- ra5    
////    (qGraph.AddVerticesAndEdge <| edg 0 1 (RNGLR.SimpleAmb.L_select 0)) |> ignore
////    for blocks = 0 to numberOfBlocks - 1 do
////        for i = 0 to numberOfPath - 1 do
////            let curChain = chains.[i]
////            for k = 0 to curChain.Count - 1 do
////                if k <> curChain.Count - 1 then
////                    qGraph.AddVerticesAndEdge <| edg curB curE (curChain.[k]) |> ignore  
////                    curB <- curE
////                    curE <- curE + 1
////                else
////                    qGraph.AddVerticesAndEdge <| edg curB e (curChain.[k]) |> ignore
////                    if i <> numberOfPath - 1 then
////                        curE <- curE
////                        curB <- b
////        if blocks <> numberOfBlocks - 1 then
////            b <- e
////            e <- curE               
////            qGraph.AddVerticesAndEdge <| edg b e (RNGLR.SimpleAmb.L_comma_ 0) |> ignore
////            b <- e
////            e <- e + 1
////            curB <- b
////            curE <- e + 1
////    b <- e
////    e <- curE               
////    qGraph.AddVerticesAndEdge <| edg b e (RNGLR.SimpleAmb.L_from 0) |> ignore
////    b <- e
////    e <- e + 1
////    qGraph.AddVerticesAndEdge <| edg b e (RNGLR.SimpleAmb.IDENT 0) |> ignore
////    b <- e
////    e <- e + 1
////    qGraph.AddVerticesAndEdge <| edg b e (RNGLR.SimpleAmb.RNGLR_EOF 0) |> ignore
////    qGraph.FinalStates <- [|e|]
////    //qGraph.PrintToDot "input.dot" (RNGLR.SimpleAmb.tokenToNumber >> GLL.SimpleAmb.numToString)
////    qGraph
//
//  
////let parser2 = RNGLR.SimpleAmb.buildAstAbstract
////for i = 50 to 150 do
////    let g = graphGenerator2 (1 + i) 4 
////    let start = System.DateTime.Now
////    let r = parser2 g
////    let finish = System.DateTime.Now - start
////    printfn "%i  : %A" (i+1) finish.TotalSeconds
////    match r with
////    | Yard.Generators.ARNGLR.Parser.Error _ ->
////        printfn "Error"     
////    | Yard.Generators.ARNGLR.Parser.Success tree->
////        ()
//

let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
let edgB b e l t = new BioParserEdge(b, e, l, t) 

(*
let inputGraph =
    let a1 = f [|GLL.aa.C 0; |] GLL.aa.tokenToNumber
    let a2 = f [|GLL.aa.C 1|] GLL.aa.tokenToNumber
    let a3 = f [|GLL.aa.C 2|] GLL.aa.tokenToNumber
    let a4 = f [|GLL.aa.C 3|] GLL.aa.tokenToNumber
    let a5 = f [|GLL.aa.C 4|] GLL.aa.tokenToNumber
    let a6 = f [|GLL.aa.A 5|] GLL.aa.tokenToNumber
   // let a7 = f [|GLL.aa.RNGLR_EOF 6|] GLL.aa.tokenToNumber
    let edges = [| edgB 0 1 1 a1;
                   edgB 1 2 1 a2;
                   edgB 2 3 1 a3;
                   edgB 3 4 1 a4;
                   edgB 4 5 1 a5;
                   edgB 5 6 1 a6|] 
    let qGraph = new BioParserInputGraph(edges)
    qGraph
*)
let inputGraph =
    //CC
    let a1 = f [|GLL.aa.C 0|] GLL.aa.tokenToNumber
    //CCCA
    let a2 = f [|GLL.aa.C 1; GLL.aa.A 0|] GLL.aa.tokenToNumber
    let edges = [| edgB 0 1 1 a1;
                   edgB 1 2 1 a2;
                   edgB 1 3 1 a2;
                   edgB 1 4 1 a2|] 
    let qGraph = new BioParserInputGraph(edges)
    qGraph
let res = buildAbstract inputGraph 0

match res with
| Success1 s -> Array.iter (printfn "Res%A") s
| Success _ -> printfn "Suc"
| _ -> printfn "Failed."