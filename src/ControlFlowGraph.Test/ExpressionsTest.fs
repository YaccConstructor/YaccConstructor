module ControlFlowGraph.Test.ExpressionTests

open NUnit.Framework

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Test.CommonHelper
open ControlFlowGraph.Test.ExpressionHelper

open QuickGraph.FSA.GraphBasedFsa

type PreviousAndNext(key : int, previous : int list, next : int list) =
    member this.Key = key
    member this.Previous = previous
    member this.Next = next

let assertCfg tokenToNumber (cfg : ControlFlow<_, _>) (expected : PreviousAndNext list) = 
    
    let processExpected graph (conf : PreviousAndNext) = 
        let inTags = getInTags tokenToNumber graph conf.Key

        let checkInEdges = 
            if Seq.isEmpty inTags 
            then 
                true
            else
                inTags
                |> Seq.exists (fun num -> conf.Previous |> List.exists ((=) num))
        Assert.True(checkInEdges, "There is incorrect in edge")

        let outTags = getOutTags tokenToNumber graph conf.Key
        let checkOutEdges = 
            if Seq.isEmpty inTags 
            then 
                true
            else
                outTags
                |> Seq.exists (fun num -> conf.Next |> List.exists ((=) num))
        Assert.True(checkOutEdges, "There is incorrect out edge")

    let processExpression (graph : CfgTokensGraph<_>) = 
        expected 
        |> List.iter (processExpected graph)
            
    cfg.Blocks
    |> Seq.map(fun block -> block.TokensGraph)
    |> Seq.iter processExpression

[<TestFixture>]
type ``Cycles inside expressions``() = 
    let parse = ExtendedCalcTest.Parser.buildAstAbstract
    let tokenToNumber = ExtendedCalcTest.Parser.tokenToNumber
    let leftSides = ExtendedCalcTest.Parser.leftSide
    let indToString = ExtendedCalcTest.Parser.numToString
    let tokenData = ExtendedCalcTest.Parser.tokenData
    let astToDot = ExtendedCalcTest.Parser.defaultAstToDot

    let fsa = new FSA<_>()
    let RNGLR_EOF = ExtendedCalcTest.Parser.RNGLR_EOF <| fsa
    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    
    let x = tokenToNumber <| ExtendedCalcTest.Parser.X fsa; 
    let y = tokenToNumber <|ExtendedCalcTest.Parser.Y fsa;
    let z = tokenToNumber <| ExtendedCalcTest.Parser.Z fsa;
    let assign = tokenToNumber <| ExtendedCalcTest.Parser.ASSIGN fsa;
    let num = tokenToNumber <| ExtendedCalcTest.Parser.NUMBER fsa;
    let plus = tokenToNumber <| ExtendedCalcTest.Parser.PLUS fsa;
    let minus = tokenToNumber <| ExtendedCalcTest.Parser.MINUS fsa;
    let mult = tokenToNumber <| ExtendedCalcTest.Parser.MULT fsa;
    let semi = tokenToNumber <| ExtendedCalcTest.Parser.SEMICOLON fsa

    let nodeToType = dict
                        [
                            "assign", Assignment;
                            "id", Identificator;
                            "expr", Expression;
                        ]

    let keywordToInt = dict [Keyword.SEMICOLON, semi;]

    let tokToRealString = tokenToNumber >> indToString
    let parserSource = new GeneratedStuffSource<_, _>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealString)
                
    let buildCfg' = buildCfg parse createCfg astToDot tokToRealString

    [<Test>]
    member this.``X = 1 [+Y]*``() = 
        let qGraph = createParserInput' "X = 1 [+Y].dot"

        let prefix = "`X = 1 [+Y]"
        let expected = 
            [
                new PreviousAndNext(y, [plus], [semi]);
                new PreviousAndNext(plus, [num; y], [y]);
                new PreviousAndNext(num, [assign], [plus; semi]);
            ]

        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToNumber cfg expected
        

    [<Test>]
    member this.``X = Y [+1]* - Z``() = 
        let qGraph = createParserInput' "X = Y [+1] - Z.dot"

        let expected = 
            [
                new PreviousAndNext(y, [assign], [plus; minus]);
                new PreviousAndNext(num, [plus], [plus; minus]);
                new PreviousAndNext(z, [minus], [semi]);
            ]

        let prefix = "`X = Y [+1] - Z"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToNumber cfg expected

    [<Test>]
    member this.``X = Y [+1[-Z]*]*``() = 
        let qGraph = createParserInput' "X = Y [+1[-Z]].dot"

        let expected = 
            [
                new PreviousAndNext(y, [assign], [plus; semi]);
                new PreviousAndNext(num, [plus], [plus; minus; semi]);
                new PreviousAndNext(z, [minus], [plus; minus; semi]);
            ]

        let prefix = "`X = Y [+1[-Z]]"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToNumber cfg expected

    [<Test>]
    member this.``X = Y [(+1) | (-Z)]*``() = 
        let qGraph = createParserInput' "X = Y [(+1) or (-Z)].dot"

        let expected = 
                [
                    new PreviousAndNext(y, [assign], [plus; minus; semi]);
                    new PreviousAndNext(num, [plus], [plus; minus; semi]);
                    new PreviousAndNext(z, [minus], [plus; minus; semi]);
                ]

        let prefix = "`X = Y [(+1) or (-Z)]"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToNumber cfg expected

//[<EntryPoint>]
let f x = 
    let cycleInsideExpress = new ``Cycles inside expressions``()
    cycleInsideExpress.``X = 1 [+Y]*``()
    0