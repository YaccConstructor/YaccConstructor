module YC.FST.AbstractLexing.Tests.Calc

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open Yard.Utils.StructClass
open QuickGraph.FST.GraphBasedFst
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph.FSA.GraphBasedFsa
open System

let baseInputGraphsPath = "../../../Tests/AbstractLexing/DOT"

let transform x = (x, match x with |Smbl(y:char, _) when y <> (char 65535) -> Smbl(int y) |Smbl(y:char, _) when y = (char 65535) -> Smbl 65535 |_ -> Eps)
let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)

//let printSmb (x:char*Position<_>) = 
//        match x with
//        | (y, _) when y = char 65535 -> "eof"  
//        | _ -> (fst x).ToString() + "_br: " + (snd x).back_ref.ToString() + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

let calcTokenizationTest file eCount vCount countEdgesArray =
    let graph = loadDotToQG (path baseInputGraphsPath file)
    let graphFsa = approximateQG(graph)
    //graphFsa.PrintToDOT("../../../FST/FST/FSA.Tests/DOTfsa/test12FSA.dot", printSmb)
    let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
    //graphFst.PrintToDOT("../../../FST/FST/FSA.Tests/DOTfsa/test12FST.dot", printSmb)
    let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphFst    
    match res with
    | Success res ->
        //ToDot res @"../../../src/AbstractLexer.Interpreter.Tests/Tests/TestInterpretParserLexer.dot" (printBref printSmbString)
        checkArr (countEdges res) countEdgesArray
        checkGraph res eCount vCount            
    | Error e -> Assert.Fail(sprintf "Tokenization problem in test %s: %A" file e)
                             
[<TestFixture>]
type ``Lexer Calc Fst Tests`` () =            
    [<Test>]
    member this.``Load graph test from DOT`` () =
        let bidirectionalGraph = loadDotToQG (baseInputGraphsPath + "test_00.dot")
        let adjacencyGraph = new AdjacencyGraph<_,_>()
        for edge in bidirectionalGraph.Edges do
            adjacencyGraph.AddVerticesAndEdge(edge) |> ignore
        checkGraph adjacencyGraph 4 4

    [<Test>] 
    member this.``Calc. Simple number.`` () =
        calcTokenizationTest "test_0.dot" 2 3 [|2; 0|] 

    [<Test>]
    member this.``Calc. Simple sum.`` () =
        calcTokenizationTest "test_1.dot" 4 5 [|1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Start from PLUS.`` () =
        calcTokenizationTest "test_2.dot" 3 4 [|1; 2; 0|]      

    [<Test>] 
    member this.``Calc. Two-digit numbers sum.`` () =
        calcTokenizationTest "test_3.dot" 4 5 [|2; 1; 2; 0|]

    [<Test>] 
    member this.``Calc. Test with position.`` () =
        calcTokenizationTest "test_with_pos_0.dot" 2 3 [|2; 0|]

    [<Test>] 
    member this.``Calc. Test with position. Ident on two edgs`` () =
        calcTokenizationTest "test_with_pos_1.dot" 2 3 [|3; 0|]

    [<Test>] 
    member this.``Calc. Test with position. Ident on edgs with branch`` () =
        calcTokenizationTest "test_with_pos_2.dot" 2 3 [|4; 0|]

    [<Test>] 
    member this.``Calc. Test with position. Ident and plus on edgs with branch`` () =
        calcTokenizationTest "test_with_pos_3.dot" 5 5 [|3; 2; 0; 1; 0|]

    [<Test>] 
    member this.``Calc. Test with position. Ident on edgs with branch in begin.`` () =
        calcTokenizationTest "test_with_pos_4.dot" 2 3 [|3; 0|]      
         
    [<Test>] 
    member this.``Calc. Test with position. Ident on edgs with branch in begin_1.`` () =
        calcTokenizationTest "test_with_pos_5.dot" 2 3 [|5; 0|]

    [<Test>] 
    member this.``Calc. Positions. Simple binop.`` () =
        calcTokenizationTest "test_with_pos_6.dot" 4 5 [|1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Test with position. Two tokens on the one edge.`` () =
        calcTokenizationTest "test_with_pos_7.dot" 4 5 [|2; 1; 2; 0|]

    [<Test>] 
    member this.``Calc. Test with position. With branch and several tokens on the one edge``() =
        calcTokenizationTest "test_with_pos_8.dot" 8 8 [|3; 1; 1; 1; 1; 1; 0; 1|]

    [<Test>] 
    member this.``Calc. Test with position. Several tokens on the one edge``() =
        calcTokenizationTest "test_with_pos_9.dot" 6 7 [|2; 1; 1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Test with position. With branch and several tokens on the one edge_1``() =
        calcTokenizationTest "test_with_pos_10.dot" 8 8 [|3; 1; 1; 1; 1; 1; 0; 1|]

    [<Test>]
    member this.``Calc. Example with whitespace.`` () =
        calcTokenizationTest "test_21.dot" 4 5 [|1; 1; 1; 0|]
        
    [<Test>] 
    member this.``Calc. Example with whitespace 1.`` () =
        calcTokenizationTest "test_16.dot" 4 5 [|1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Example with whitespace 2.`` () =
        calcTokenizationTest "test_17.dot" 5 5 [|1; 1; 1; 0; 1|]
       
    [<Test>] 
    member this.``Calc. Example with whitespace 3.`` () =
        calcTokenizationTest "test_18.dot" 3 3 [|0; 1; 0|]

    [<Test>] 
    member this.``Calc. Example with whitespace 4.`` () =
        calcTokenizationTest "test_19.dot" 4 4 [|1; 1; 0; 1|]

    [<Test>] 
    member this.``Calc. Example with whitespace 5.`` () =
        calcTokenizationTest "test_20.dot" 1 2 [|0|]

    [<Test>] 
    member this.``Calc. Print info on edges.`` () =
        calcTokenizationTest "test_15.dot" 2 3 [|8; 0|]
        
    [<Test>] 
    member this.``Calc. Branched multy-digit numbers.`` () =     
         calcTokenizationTest "test_4_1.dot" 4 4  [|3; 1; 0; 0|]
        
    [<Test>]
    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
         calcTokenizationTest "test_4_2.dot" 5 5  [|3; 1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
        calcTokenizationTest "test_4_3.dot" 6 6  [|3; 1; 1; 1; 1; 0|]   

    [<Test>] 
    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
        calcTokenizationTest "test_4_4.dot" 8 7  [|3; 1; 1; 1; 2; 1; 0; 0|]  
        
    [<Test>] 
    member this.``Calc. Branched binop.`` () =
        calcTokenizationTest "test_5.dot" 6 6  [|1; 1; 1; 1; 1; 0|] 

    [<Test>] 
    member this.``Calc. Branched binop or negation.`` () =
        calcTokenizationTest "test_6.dot" 6 6 [|1; 1; 1; 2; 2; 0|]

    [<Test>] //strange 
    member this.``Calc. Complex branched 1.`` () =
        calcTokenizationTest "test_7.dot" 12 8  [|3; 1; 1; 1; 1; 1; 5; 1; 5; 1; 0; 0|] 

    [<Test>] 
    member this.``Calc. Complex branched 2.`` () =
        calcTokenizationTest "test_8.dot" 5 5  [|1; 1; 1; 0; 1|]

    [<Test>]
    member this.``Calc. Complex branched 3.`` () =
        calcTokenizationTest "test_9.dot" 8 8 [|1; 1; 1; 1; 2; 2; 1; 0|]
        
    [<Test>] 
    member this.``Calc. Complex 0`` () =
        calcTokenizationTest "test_12.dot" 7 7 [|1; 1; 2; 1; 1; 1; 0|]

    [<Test>] 
    member this.``Calc. Whitespace edge.`` () =
        calcTokenizationTest "test_10.dot" 5 5 [|4; 1; 0; 1; 0|]       

    [<Test>] 
    member this.``Calc. Test with space and idents on edge.`` () =
        calcTokenizationTest "test_with_space_0.dot" 3 4 [|1; 1; 0|]

    [<Test>] 
    member this.``Calc. Test with space with branch.`` () =
         calcTokenizationTest "test_with_space_1.dot" 5 5 [|3; 1; 0; 1; 0|]    

    [<Test>] 
    member this.``Calc. Calc with braces 2.`` () =
        calcTokenizationTest "calc_0.dot" 3 4 [|2; 1; 0|] 
        
    [<Test>] 
    member this.``Calc. Test with same tokens.`` () =
        calcTokenizationTest "test_same_tok.dot" 3 4 [|7; 1; 0|]                         

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Calc Fst Tests`` () 
//      let a = t.``Calc. Complex branched 3.``()
//      //printfn "%A" a      
//      1

