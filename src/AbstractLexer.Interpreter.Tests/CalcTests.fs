module YC.FST.AbstractLexing.Tests.Calc

open NUnit.Framework
open Microsoft.FSharp.Collections
open QuickGraph
open AbstractAnalysis.Common
open YC.FST.GraphBasedFst
open YC.FST.FstApproximation
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker

let baseInputGraphsPath = "../../../../Tests/AbstractLexing/DOT"

let calcTokenizationTest path eCount vCount =
    let graphAppr = loadDotToQG baseInputGraphsPath path
    let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
    checkGraph res eCount vCount
                             
[<TestFixture>]
type ``Lexer Calc Fst Tests`` () =            
    [<Test>]
    member this.``Load graph test from DOT`` () =
        let g = loadDotToQG baseInputGraphsPath "test_00.dot"
        checkGraph g 4 4

    [<Test>]
    member this.``Calc. Simple number.`` () =
        calcTokenizationTest "test_0.dot" 2 3     

    [<Test>]
    member this.``Calc. Simple sum.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0|]
        checkArr endPos [|1; 1; 1|]
        checkArr backref [|"1"; "+"; "2"|]

    [<Test>]
    member this.``Calc. Start from PLUS.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 3 4

    [<Test>]
    member this.``Calc. Two-digit numbers sum.`` () =
        calcTokenizationTest "test_3.dot" 4 5

    [<Test>]
    member this.``Calc. Two-digit numbers sum. Check back refs.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0|]
        checkArr endPos [|2; 1; 2|]
        checkArr backref [|"12"; "+"; "33"|]

    [<Test>]
    member this.``Calc. Test with position.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0|]
        checkArr endPos [|2|]
        checkArr backref [|"12"|]

    [<Test>]
    member this.``Calc. Test with position. Ident on two edgs`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3 
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0|]
        checkArr endPos [|2; 1|]
        checkArr backref [|"12"; "3"|]       

    [<Test>]
    member this.``Calc. Test with position. Ident on edgs with branch`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 3 3
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 0|]
        checkArr endPos [|2; 1; 2; 1|]
        checkArr backref [|"12"; "4"; "12"; "3"|]  

    [<Test>] 
    member this.``Calc. Test with position. Ident and plus on edgs with branch`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 5 5
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 0|]
        checkArr endPos [|2; 1; 2; 1|]
        checkArr backref [|"12"; "3"; "12"; "+"|] 

    //[<Test>] //DON'T WORK RIGHT!!
    member this.``Calc. Test with position. Ident on edgs with branch in begin.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_4.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref
        checkGraph res 3 2          
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 0|]
        checkArr endPos [|2; 1; 2; 1|]
        checkArr backref [|"12"; "4"; "12"; "3"|] 
         
    [<Test>]
    member this.``Calc. Test with position. Ident on edgs with branch in begin_1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_5.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 3 3 
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 0; 0|]
        checkArr endPos [|1; 1; 1; 1; 1|]
        checkArr backref [|"1"; "3"; "4"; "2"; "4"|] 

    [<Test>]
    member this.``Calc. Positions. Simple binop.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_6.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 1; 2|]
        checkArr endPos [|1; 2; 3|]
        checkArr backref [|"1*3"; "1*3"; "1*3"|] 

    [<Test>]
    member this.``Calc. Test with position. Two tokens on the one edge.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_7.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 1|]
        checkArr endPos [|2; 1; 3|]
        checkArr backref [|"12"; "+34"; "+34"|] 

    [<Test>]
    member this.``Calc. Test with position. With branch and several tokens on the one edge``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_8.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 8 8
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 1; 1; 0; 2; 1|]
        checkArr endPos [|2; 1; 1; 2; 2; 1; 3; 2|]
        checkArr backref [|"12"; "4+5"; "3+"; "4+5"; "3+"; "4+5"; "4+5"; "4+5"|] 

    [<Test>]
    member this.``Calc. Test with position. Several tokens on the one edge``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_9.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 6 7
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 1; 2; 0|]
        checkArr endPos [|2; 1; 2; 3; 1|]
        checkArr backref [|"12"; "*3+"; "*3+"; "*3+"; "4"|] 

    [<Test>]
    member this.``Calc. Test with position. With branch and several tokens on the one edge_1``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_10.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 8 8
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 1; 1; 0; 2; 1|]
        checkArr endPos [|2; 1; 1; 2; 2; 1; 3; 2|]
        checkArr backref [|"12"; "4-5"; "3+"; "4-5"; "3+"; "4-5"; "4-5"; "4-5"|] 

    //[<Test>] eps:eps
    member this.``Calc. Check break literals 1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_break_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3

    //[<Test>] eps:eps
    member this.``Calc. Check break literals 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_break_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3

    //[<Test>] eps:eps
    member this.``Calc. Check break literals 3.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_break_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5

    [<Test>]
    member this.``Calc. Example with whitespace.`` () =
        calcTokenizationTest "test_21.dot" 4 5
        
    [<Test>]
    member this.``Calc. Example with whitespace 1.`` () =
        calcTokenizationTest "test_16.dot" 4 5

    [<Test>]
    member this.``Calc. Example with whitespace 2.`` () =
        calcTokenizationTest "test_17.dot" 5 5

    [<Test>]
    member this.``Calc. Example with whitespace 3.`` () =
        calcTokenizationTest "test_18.dot" 3 3

    [<Test>]
    member this.``Calc. Example with whitespace 4.`` () =
        calcTokenizationTest "test_19.dot" 4 4 

    [<Test>]
    member this.``Calc. Example with whitespace 5.`` () =
        calcTokenizationTest "test_20.dot" 1 2

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Print info on edges.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_15.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 3 3       
        let startPos = positions res (fun p -> p.start_offset)
        let endPos = positions res (fun p -> p.end_offset)
        let backref = positions res (fun p -> p.back_ref)
        checkArr startPos [|0; 0; 0; 1; 1; 0; 2; 1|]
        checkArr endPos [|2; 1; 1; 2; 2; 1; 3; 2|]
        checkArr backref [|"12"; "4-5"; "3+"; "4-5"; "3+"; "4-5"; "4-5"; "4-5"|]   
        
    [<Test>]
    member this.``Calc. Branched multy-digit numbers.`` () =          
        calcTokenizationTest "test_4_1.dot" 4 4 
        
    [<Test>]
    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
        calcTokenizationTest "test_4_2.dot" 5 5

    [<Test>]
    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
        calcTokenizationTest "test_4_3.dot" 6 6       

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_4_4.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 3 3    
        //calcTokenizationTest "test_4_4.dot" 6 5          
        
    [<Test>] 
    member this.``Calc. Branched binop.`` () =
        calcTokenizationTest "test_5.dot" 6 6

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Branched binop or negation.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_6.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 7 7  
        //calcTokenizationTest "test_6.dot" 5 5

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Complex branched 1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_7.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 7 7 
        //calcTokenizationTest "test_7.dot" 8 5       

    [<Test>]
    member this.``Calc. Complex branched 2.`` () =
        calcTokenizationTest "test_8.dot" 5 5

    [<Test>]
    member this.``Calc. Complex branched 3.`` () =
        calcTokenizationTest "test_9.dot" 8 8
        
    [<Test>]
    member this.``Calc. Complex 0`` () =
        calcTokenizationTest "test_12.dot" 7 7  

    [<Test>] 
    member this.``Calc. Whitespace edge.`` () =
        calcTokenizationTest "test_10.dot" 5 5     
        
    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. test 100`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_100.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        //calcTokenizationTest "test_10.dot" 3 4         

    [<Test>]
    member this.``Calc. Test with space and idents on edge.`` () =
        calcTokenizationTest "test_with_space_0.dot" 3 4

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Test with space with branch.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_space_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 7 7
        //calcTokenizationTest "test_with_space_1.dot" 4 4

    //[<Test>] DON'T WORK RIGHT!!
    member this.``Calc. Calc with braces.`` () =
        calcTokenizationTest "calc_1.dot." 10 10

    [<Test>]
    member this.``Calc. Calc with braces 2.`` () =
        calcTokenizationTest "calc_0.dot." 4 4    
        
    //[<Test>] 
    member this.``Calc. Test with same tokens.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_same_tok.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\CalcTest.dot" printBref  
        checkGraph res 7 7                               

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Calc Fst Tests`` () 
//      let a = t.``Calc. Test with composition.``()
//      //printfn "%A" a      
//      1