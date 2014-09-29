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
    [<Test>] //checked
    member this.``Load graph test from DOT`` () =
        let g = loadDotToQG baseInputGraphsPath "test_00.dot"
        checkGraph g 4 4

    [<Test>] //checked
    member this.``Calc. Simple number.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 2 3     

    [<Test>] //checked
    member this.``Calc. Simple sum.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 4 5 

    [<Test>] //checked
    member this.``Calc. Start from PLUS.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        checkGraph res 3 4

    [<Test>] //checked
    member this.``Calc. Two-digit numbers sum.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc2.dot" printBref
        checkGraph res 4 5

    [<Test>] //checked
    member this.``Calc. Test with position.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc3.dot" printBref
        checkGraph res 2 3

    [<Test>] //checked
    member this.``Calc. Test with position. Ident on two edgs`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc4.dot" printBref
        checkGraph res 2 3    

    [<Test>] //checked
    member this.``Calc. Test with position. Ident on edgs with branch`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc5.dot" printBref
        checkGraph res 2 3

    [<Test>] //checked 
    member this.``Calc. Test with position. Ident and plus on edgs with branch`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc6.dot" printBref
        checkGraph res 5 5

    [<Test>] //checked
    member this.``Calc. Test with position. Ident on edgs with branch in begin.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_4.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc7.dot" printBref
        checkGraph res 2 3         
         
    [<Test>] //checked
    member this.``Calc. Test with position. Ident on edgs with branch in begin_1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_5.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc8.dot" printBref
        checkGraph res 2 3

    [<Test>] //checked
    member this.``Calc. Positions. Simple binop.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_6.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc9.dot" printBref
        checkGraph res 4 5

    [<Test>] //checked
    member this.``Calc. Test with position. Two tokens on the one edge.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_7.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc10.dot" printBref
        checkGraph res 4 5

    [<Test>] //checked
    member this.``Calc. Test with position. With branch and several tokens on the one edge``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_8.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc11.dot" printBref
        checkGraph res 8 8

    [<Test>] //checked
    member this.``Calc. Test with position. Several tokens on the one edge``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_9.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc12.dot" printBref
        checkGraph res 6 7

    [<Test>] //checked
    member this.``Calc. Test with position. With branch and several tokens on the one edge_1``() =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_pos_10.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc13.dot" printBref
        checkGraph res 8 8

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

    [<Test>] //checked
    member this.``Calc. Example with whitespace.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_21.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc14.dot" printBref
        checkGraph res 4 5
        
    [<Test>] //checked
    member this.``Calc. Example with whitespace 1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_16.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc15.dot" printBref
        checkGraph res 4 5

    [<Test>] //checked
    member this.``Calc. Example with whitespace 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_17.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc16.dot" printBref
        checkGraph res 5 5
       
    [<Test>] //checked
    member this.``Calc. Example with whitespace 3.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_18.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc17.dot" printBref
        checkGraph res 3 3

    [<Test>] //checked
    member this.``Calc. Example with whitespace 4.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_19.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc18.dot" printBref
        checkGraph res 4 4

    [<Test>] //checked
    member this.``Calc. Example with whitespace 5.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_20.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc19.dot" printBref
        checkGraph res 1 2

    [<Test>] //checked
    member this.``Calc. Print info on edges.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_15.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc20.dot" printBref  
        checkGraph res 2 3      
        
    [<Test>] //checked
    member this.``Calc. Branched multy-digit numbers.`` () =     
        let graphAppr = loadDotToQG baseInputGraphsPath "test_4_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc21.dot" printBref  
        checkGraph res 4 4   
        
    [<Test>] //checked
    member this.``Calc. Branched multy-digit numbers with Binop.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_4_2.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc22.dot" printBref  
        checkGraph res 5 5  

    [<Test>] //checked
    member this.``Calc. Branched multy-digit numbers sum 1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_4_3.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc23.dot" printBref  
        checkGraph res 6 6      

    [<Test>] //checked
    member this.``Calc. Branched multy-digit numbers sum 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_4_4.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc24.dot" printBref  
        checkGraph res 8 7        
        
    [<Test>] //checked
    member this.``Calc. Branched binop.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_5.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc25.dot" printBref  
        checkGraph res 6 6      

    [<Test>] //checked
    member this.``Calc. Branched binop or negation.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_6.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc26.dot" printBref  
        checkGraph res 6 6  

    //[<Test>] //strange 
    member this.``Calc. Complex branched 1.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_7.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc27.dot" printBref  
        checkGraph res 7 7 

    [<Test>] //checked
    member this.``Calc. Complex branched 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_8.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc28.dot" printBref  
        checkGraph res 5 5 

    [<Test>] //checked
    member this.``Calc. Complex branched 3.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_9.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc29.dot" printBref  
        checkGraph res 8 8 
        
    [<Test>] //checked
    member this.``Calc. Complex 0`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_12.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc30.dot" printBref  
        checkGraph res 7 7 

    [<Test>] //checked
    member this.``Calc. Whitespace edge.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_10.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc31.dot" printBref  
        checkGraph res 5 5   
        
    [<Test>] 
    member this.``Calc. test 100`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_100.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc32.dot" printBref         

    [<Test>] //checked
    member this.``Calc. Test with space and idents on edge.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_space_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc33.dot" printBref  
        checkGraph res 3 4

    [<Test>] //checked
    member this.``Calc. Test with space with branch.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_with_space_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc34.dot" printBref  
        checkGraph res 5 5

    //[<Test>] NOT CORRECT GRAPH!!
    member this.``Calc. Calc with braces.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "calc_1.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc35.dot" printBref  
        checkGraph res 10 10      

    [<Test>] //checked
    member this.``Calc. Calc with braces 2.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "calc_0.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc36.dot" printBref  
        checkGraph res 3 4   
        
    [<Test>] //checked
    member this.``Calc. Test with same tokens.`` () =
        let graphAppr = loadDotToQG baseInputGraphsPath "test_same_tok.dot"
        let res = YC.FST.AbstractLexing.CalcLexer.tokenize eof graphAppr
        ToDot res @"..\..\Tests\testParserCalc37.dot" printBref  
        checkGraph res 3 4                                

//[<EntryPoint>]
//let f x =
//      let t = new ``Lexer Calc Fst Tests`` () 
//      let a = t.``Calc. Start from PLUS.``()
//      //printfn "%A" a      
//      1

