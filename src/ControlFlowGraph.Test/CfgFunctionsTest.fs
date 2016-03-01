module ControlFlowGraph.Test.CfgFunctionsTest

open NUnit.Framework

open AbstractAnalysis.Common

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.Test.CommonHelper

open QuickGraph.FSA.GraphBasedFsa

let assertResult (errorList : _ list) (expected : _ list) = 
        
    printfn "%A" errorList
    printfn "Expected: %d. Actual: %d." expected.Length errorList.Length 
    Assert.AreEqual(expected.Length, errorList.Length)
        
    let res =
        expected
        |> List.forall (fun var -> errorList |> List.exists ((=) var))

    Assert.True(res, "Not all expected variables are found")

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

    let nodeToType = dict
                        [
                            "assign", Assignment; 
                            "id", Identificator; 
                            "expr", Expression
                        ]
        
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

    let buildCfg' = buildCfg parse createCfg astToDot tokToRealName

    [<Test>]
    member test.``X = Z; Y = X;``() = 
        let qGraph = createParserInput' "X = Z; Y = X.dot"

        let expected = [z]
        let prefix = "`cfg undefined variables X = Z; Y = X"
        
        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected

    [<Test>]
    member test.``X = X``() = 
        let qGraph = createParserInput' "X = X.dot"

        let expected = [x]
        let prefix = "`cfg undefined variables X = X"

        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected


    [<Test>]
    member test.``Ambiguous``() =
        let qGraph = createParserInput' "Ambiguous.dot"
        
        let expected = [y; z]
        let prefix = "`cfg undefined variables ambiguous"
            
        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected

            
    [<Test>]
    member test.``Ambiguous 2``() =
        let qGraph = createParserInput' "Ambiguous3.dot"
        
        let expected = [y]
        let prefix = "`cfg undefined variables ambiguous2"

        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected


    [<Test>]
    member this.``Cycle inside expression``() = 
        let qGraph = createParserInput' "X = 1 [+Y].dot"
        
        let prefix = "`cfg cycle inside expression"

        let expected = [y]

        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected
     
type ``Scope test``() = 
    let parse = LetTest.Parser.buildAstAbstract
    let tokenToNumber = LetTest.Parser.tokenToNumber
    let leftSides = LetTest.Parser.leftSide
    let indToString = LetTest.Parser.numToString
    let tokenData = LetTest.Parser.tokenData
    let astToDot = LetTest.Parser.defaultAstToDot

    let fsa = new FSA<_>()
    let RNGLR_EOF = LetTest.Parser.RNGLR_EOF fsa
    let semicolonNumber = tokenToNumber <| LetTest.Parser.SEMICOLON fsa
    let assignNumber = tokenToNumber <| LetTest.Parser.ASSIGN fsa

    let nodeToType = dict["let_expr", Assignment; "id", Identificator; "expr", Expression]
        
    let keywordToInt = dict [
                                Keyword.SEMICOLON, semicolonNumber;
                                Keyword.ASSIGN, assignNumber;
                            ]
        
    let varsNumbers = 
        [LetTest.Parser.X fsa; LetTest.Parser.Y fsa; LetTest.Parser.Z fsa]
        |> List.map tokenToNumber

    let isVariable tok = varsNumbers |> List.exists ((=) tok) 

    let tokToRealName = tokenToNumber >> indToString

    let x = LetTest.Parser.X fsa |> tokToRealName
    let y = LetTest.Parser.Y fsa |> tokToRealName
    let z = LetTest.Parser.Z fsa |> tokToRealName
        
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let createParserInput' = createParserInputGraph LetTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealName)

    let buildCfg' = buildCfg parse createCfg astToDot tokToRealName

    
     
    [<Test>]
    member this.``Scope1``() = 
        let qGraph = createParserInput' "Scope.dot"
        
        let prefix = "`scope"

        let expected = [y]
        
        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected
    
    [<Test>]
    member this.``Scope2``() = 
        let qGraph = createParserInput' "Scope2.dot"
        
        let prefix = "`scope2"

        let expected = []
        
        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected

    [<Test>]
    member this.``Scope3``() = 
        let qGraph = createParserInput' "Scope3.dot"
        
        let prefix = "`scope3"

        let expected = []
        
        //act
        let cfg = buildCfg' qGraph prefix
        let errorList = 
            cfg.FindUndefinedVariables()
            |> List.map tokToRealName
        
        //assert
        assertResult errorList expected
        
//[<EntryPoint>]
let f x = 
    let functions =  ``Find undefined variables``()
    functions.``X = Z; Y = X;``()
    (*let scopeTest = ``Scope test``()
    scopeTest.Scope1()*)
    1