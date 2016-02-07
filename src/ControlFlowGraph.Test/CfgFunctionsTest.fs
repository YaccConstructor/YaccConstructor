module ControlFlowGraph.Test.CfgFunctionsTest

open NUnit.Framework

open AbstractAnalysis.Common

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.Test.CommonHelper

open QuickGraph.FSA.GraphBasedFsa

[<TestFixture>]
type ``Find undefined variables``() =
    
    let parse = ExtendedCalcTest.Parser.buildAstAbstract
    let tokenToNumber = ExtendedCalcTest.Parser.tokenToNumber
    let leftSides = ExtendedCalcTest.Parser.leftSide
    let indToString = ExtendedCalcTest.Parser.numToString
    let tokenData = ExtendedCalcTest.Parser.tokenData
    let astToDot = ExtendedCalcTest.Parser.defaultAstToDot

    let fsa = new FSA<_>()
    let RNGLR_EOF = ExtendedCalcTest.Parser.RNGLR_EOF fsa
    let semicolonNumber = tokenToNumber <| ExtendedCalcTest.Parser.SEMICOLON fsa
    let assignNumber = tokenToNumber <| ExtendedCalcTest.Parser.ASSIGN fsa

    let nodeToType = dict["assign", Assignment;]
        
    let keywordToInt = dict [
                                Keyword.SEMICOLON, semicolonNumber;
                                Keyword.ASSIGN, assignNumber;
                            ]
        
    let varsNumbers = 
        [ExtendedCalcTest.Parser.X fsa; ExtendedCalcTest.Parser.Y fsa; ExtendedCalcTest.Parser.Z fsa]
        |> List.map tokenToNumber

    let isVariable tok = varsNumbers |> List.exists ((=) tok) 

    let tokToRealName = tokenToNumber >> indToString

    let x = ExtendedCalcTest.Parser.X fsa |> tokToRealName
    let y = ExtendedCalcTest.Parser.Y fsa |> tokToRealName
    let z = ExtendedCalcTest.Parser.Z fsa |> tokToRealName
        
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealName)

    let runTest (cfg : ControlFlow<_>) (expected : string list) prefix = 
        let errorList = 
            cfg.FindUndefVariable()
            |> List.map tokToRealName
        
        printfn "%A" errorList
        printfn "Expected: %d. Actual: %d." expected.Length errorList.Length 
        Assert.AreEqual(expected.Length, errorList.Length)
        
        let res =
            expected
            |> List.forall (fun var -> errorList |> List.exists ((=) var))

        Assert.True(res, "Not all expected variables are found")

    [<Test>]
    member test.``Elementary``() = 
        let qGraph = createParserInput' "X = Z; Y = X.dot"

        let expected = [z]
        let prefix = "`cfg undefined variables elementary"
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealName prefix
        //assert
        runTest cfg expected prefix

    [<Test>]
    member test.``X = X``() = 
        let qGraph = createParserInput' "X = X.dot"

        let expected = [x]
        let prefix = "`cfg undefined variables X = X"

        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealName prefix
        //assert
        runTest cfg expected prefix

    [<Test>]
    member test.``Undef: ambiguous``() =
        let qGraph = createParserInput' "Ambiguous.dot"
        
        let expected = [y; z]
        let prefix = "`cfg undefined variables ambiguous"
            
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealName prefix
        //assert
        runTest cfg expected prefix
            
    [<Test>]
    member test.``Undef: ambiguous 2``() =
        let qGraph = createParserInput' "Ambiguous3.dot"
        
        let expected = [y]
        let prefix = "`cfg undefined variables ambiguous2"
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealName prefix
        //assert
        runTest cfg expected prefix

    [<Test>]
    member this.``Cycle inside expression``() = 
        let qGraph = createParserInput' "X = 1 [+Y].dot"
        
        let prefix = "`cfg cycle inside expression"

        let expected = [y]
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealName prefix
        //assert
        runTest cfg expected prefix
        
//[<EntryPoint>]
let f x = 
    let functions =  ``Find undefined variables``()
    functions.Elementary()
    1