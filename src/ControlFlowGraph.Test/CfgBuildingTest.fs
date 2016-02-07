module ControlFlowGraph.Test.CfgBuildingTest

open NUnit.Framework

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.Test.CommonHelper
open ControlFlowGraph.Test.BlockHelper

open QuickGraph.FSA.GraphBasedFsa

let returnTrue = fun _ -> true

let runTest (cfg : ControlFlow<_>) expectedBlocksCount expectedNodesCount checkEntry checkExit checks = 
            
    Assert.IsTrue(checkEntry cfg.Entry, "Incorrect entry node!")
    Assert.IsTrue(checkExit cfg.Exit, "Incorrect exit node")
            
    Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
    Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")

    let checkCondition condition = 
        let res = condition cfg.Blocks
        Assert.True(res, "Incorrect cfg was built")

    checks
    |> Seq.iter checkCondition

[<TestFixture>]
type ``Simple cases``() =
    let parse = ExtendedCalcTest.Parser.buildAstAbstract
    let tokenToNumber = ExtendedCalcTest.Parser.tokenToNumber
    let leftSides = ExtendedCalcTest.Parser.leftSide
    let indToString = ExtendedCalcTest.Parser.numToString
    let tokenData = ExtendedCalcTest.Parser.tokenData
    let astToDot = ExtendedCalcTest.Parser.defaultAstToDot 

    let fsa = new FSA<_>()
    let RNGLR_EOF = ExtendedCalcTest.Parser.RNGLR_EOF <| new FSA<_>()

    let xNumber = tokenToNumber <| ExtendedCalcTest.Parser.X fsa
    let yNumber = tokenToNumber <| ExtendedCalcTest.Parser.Y fsa
    let zNumber = tokenToNumber <| ExtendedCalcTest.Parser.Z fsa
    let semicolonNumber = tokenToNumber <| ExtendedCalcTest.Parser.SEMICOLON fsa

    let nodeToType = dict ["assign", Assignment;]
        
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    
    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealString)
        
    [<Test>]
    member test.``Elementary test``() =
        let qGraph = createParserInput' "Seq.dot"
        let nodeToChildren = dict [xNumber, [yNumber]; yNumber, [zNumber]; zNumber, [];]
        let myChildrenCheck = checkChildren tokenToNumber nodeToChildren

        let nodeToParents = dict [xNumber, []; yNumber, [xNumber]; zNumber, [yNumber];]
        let myParentsCheck = checkParent tokenToNumber nodeToParents

        let myConds = [myChildrenCheck; myParentsCheck]
        
        let expectedNodes = 4
        let expectedBlocks = 3
        let prefix = "`elementary"

        //act 
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //assert
        runTest cfg expectedBlocks expectedNodes returnTrue returnTrue myConds

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = createParserInput' "Ambiguous.dot"

        let expectedNodes = 4
        let expectedBlocks = 4

        let blockToChildren = dict [yNumber, [xNumber]; zNumber, [xNumber];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[xNumber, [yNumber; zNumber]]
            //dict [(*yNumber, [xNumber]; zNumber, [xNumber];*)]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents';]

        let prefix = "`ambiguous"
        //act 
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //assert
        runTest cfg expectedBlocks expectedNodes returnTrue returnTrue myChecks
        
    [<Test>]
    member this.``Ambiguous2 test``() = 
        let qGraph = createParserInput' "Ambiguous2.dot"

        let expectedNodes = 2
        let expectedBlocks = 2

        let blockToChildren = dict [yNumber, []; zNumber, [];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict [yNumber, []; zNumber, [];]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let prefix = "`ambiguous2"
        //act 
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //assert
        runTest cfg expectedBlocks expectedNodes returnTrue returnTrue myChecks
        
[<TestFixture>]
type ``If statements`` () =
    let parse = IfTest.Parser.buildAstAbstract
    let tokenToNumber = IfTest.Parser.tokenToNumber
    let leftSides = IfTest.Parser.leftSide
    let indToString = IfTest.Parser.numToString
    let tokenData = IfTest.Parser.tokenData
    let astToDot = IfTest.Parser.defaultAstToDot
    
    let fsa = new FSA<_>()
    let RNGLR_EOF = IfTest.Parser.RNGLR_EOF fsa

    let ifNumber = tokenToNumber <| IfTest.Parser.IF fsa
    let thenNumber = tokenToNumber <| IfTest.Parser.THEN fsa
    let elseNumber = tokenToNumber <| IfTest.Parser.ELSE fsa
    let semicolonNumber = tokenToNumber <| IfTest.Parser.SEMICOLON fsa
    
    let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                          ]
    
    let tokenToString = tokenToNumber >> indToString

    let keywordToInt = dict [
                                    Keyword.SEMICOLON, semicolonNumber;
                                    Keyword.IF, ifNumber;
                                    Keyword.THEN, thenNumber;
                                    Keyword.ELSE, elseNumber;
                            ]

    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createParserInput' = createParserInputGraph IfTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = ControlFlow (tree, parserSource, langSource, tokenToString)

    [<Test>]
    member test.``Simple If test``() =
        let qGraph = createParserInput' "Simple if.dot"

        let prefix = "`simple if"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5

        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokenToString prefix
        //assert
        runTest cfg expectedBlocksCount expectedNodesCount returnTrue returnTrue []

    [<Test>]
    member test.``Big If test``() =
        let qGraph = createParserInput' "Big if.dot"

        let prefix = "`big if"
        let expectedBlocksCount = 6
        let expectedNodesCount = 7
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokenToString prefix
        //assert
        runTest cfg expectedBlocksCount expectedNodesCount returnTrue returnTrue []

    [<Test>]
    member test.``If without else test``() =
        let qGraph = createParserInput' "If without else.dot"

        let prefix = "`if without else"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokenToString prefix
        //assert
        runTest cfg expectedBlocksCount expectedNodesCount returnTrue returnTrue []
            
    [<Test>]
    member test.``Inner if``() =
        let qGraph = createParserInput' "Inner if.dot"

        let prefix = "`inner if"
        let expectedBlocksCount = 7
        let expectedNodesCount = 8
        //act
        let cfg = buildCfg qGraph parse createCfg astToDot tokenToString prefix
        //assert
        runTest cfg expectedBlocksCount expectedNodesCount returnTrue returnTrue []
        
[<TestFixture>]
type ``Cycles``() = 
    let parse = SimpleTest.Parser.buildAstAbstract
    let tokenToNumber = SimpleTest.Parser.tokenToNumber
    let leftSides = SimpleTest.Parser.leftSide
    let indToString = SimpleTest.Parser.numToString
    let tokenData = SimpleTest.Parser.tokenData
    let astToDot = SimpleTest.Parser.defaultAstToDot

    let fsa = new FSA<_>()
    let RNGLR_EOF = SimpleTest.Parser.RNGLR_EOF <| new FSA<_>()

    let aNumber = tokenToNumber <| SimpleTest.Parser.A fsa
    let bNumber = tokenToNumber <| SimpleTest.Parser.B fsa
    let cNumber = tokenToNumber <| SimpleTest.Parser.C fsa
    let semicolonNumber = tokenToNumber <| SimpleTest.Parser.SEMICOLON fsa
        
    let nodeToType = dict["assign", Assignment;]
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString = tokenToNumber >> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)
    
    let createParserInput' = createParserInputGraph SimpleTest.Lexer.tokenize RNGLR_EOF

    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealString)

    //At least One token from the expected set must exist in the tokenSet set
    let myCond expected tokenSet = 
        expected
        |> Array.exists(fun num -> tokenSet |> Array.exists ((=) num))

    [<Test>]
    member this.``Cycle A+``() = 
        let qGraph = createParserInput' "A+.dot"

        let expectedNodes = 3
        let expectedBlocks = 2

        let checkEntryNode' = checkEntryNode tokenToNumber returnTrue
        let checkExitNode' = checkExitNode tokenToNumber returnTrue

        let blockToChildren = dict [aNumber, [aNumber];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict [aNumber, [aNumber];]
        let checkParents' = checkParent tokenToNumber blockToParents

        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle A+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A B*``() = 
        let qGraph = createParserInput' "A B(asteriks).dot"

        let expectedNodes = 3
        let expectedBlocks = 2

        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        
        let exitCond = myCond [|aNumber; bNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [aNumber, [bNumber];]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict [bNumber, [aNumber; bNumber];]
        let checkParents' = checkParent tokenToNumber blockToParents

        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle A B asteriks"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A B* C``() = 
        let qGraph = createParserInput' "A B(asteriks) C.dot"

        let expectedNodes = 3
        let expectedBlocks = 3

        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        
        let exitCond = myCond [|cNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [bNumber; cNumber];
                                    bNumber, [bNumber; cNumber];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                bNumber, [aNumber; bNumber];
                                cNumber, [aNumber; bNumber];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents

        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle A B asteriks C"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle (A | B)+``() = 
        let qGraph = createParserInput' "(A or B)+.dot"

        let expectedNodes = 3
        let expectedBlocks = 4

        //first block has two childs: A and B
        let entryCond = myCond [| aNumber; bNumber |]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        //last block has two childs: A and B
        let exitCond = myCond [| aNumber; bNumber |]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [aNumber; bNumber];
                                    bNumber, [aNumber; bNumber];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [aNumber; bNumber];
                                    bNumber, [aNumber; bNumber];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']
        let prefix = "`Cycle (A or B)+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A (B+ | C+)``() = 
        let qGraph = createParserInput' "A (B+ or C+).dot"

        let expectedNodes = 4
        let expectedBlocks = 6

        //first block has only one child: A
        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond [|bNumber; cNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [bNumber; cNumber];
                                    bNumber, [bNumber;];
                                    cNumber, [cNumber;];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    bNumber, [aNumber; bNumber];
                                    cNumber, [aNumber; cNumber];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle A (B+ or C+)"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks


    [<Test>]
    member this.``Cycle (AB)+``() = 
        let qGraph = createParserInput' "(AB)+.dot"

        let expectedNodes = 4
        let expectedBlocks = 3
        //first block has only one child: A
        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond [|bNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [bNumber;];
                                    bNumber, [aNumber;];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [bNumber;];
                                    bNumber, [aNumber;];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle (AB)+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle (AB)+C``() = 
        let qGraph = createParserInput' "(AB)+C.dot"

        let expectedNodes = 4
        let expectedBlocks = 4

        //first block has only one child: A
        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond [|cNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [bNumber;];
                                    bNumber, [aNumber; cNumber];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [bNumber;];
                                    bNumber, [aNumber;];
                                    cNumber, [bNumber;];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle (AB)+C"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle after cycle A+B+``() = 
        let qGraph = createParserInput' "A+B+.dot"

        //first block has only one child: A
        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        
        //last block has only one child: B
        let exitCond = myCond [|bNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [aNumber; bNumber;];
                                    bNumber, [bNumber];
                                   ]
        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [aNumber;];
                                    bNumber, [aNumber; bNumber;];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let expectedNodes = 4
        let expectedBlocks = 4
        let prefix = "`Cycle after cycle A+B+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle inside cycle (A+B)+``() = 
        let qGraph = createParserInput' "(A+B)+.dot"

        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond

        let exitCond = myCond [|bNumber|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let expectedNodes = 4
        let expectedBlocks = 4

        let blockToChildren = dict [
                                    aNumber, [aNumber; bNumber;];
                                    bNumber, [aNumber; ];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [aNumber; bNumber;];
                                    bNumber, [aNumber;];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let prefix = "`Cycle inside cycle (A+B)+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle inside cycle ((AB)+C)+``() = 
        let qGraph = createParserInput' "((AB)+C)+.dot"

        let entryCond = myCond [|aNumber|]
        let checkEntryNode' = checkEntryNode tokenToNumber entryCond
        
        let exitCond = myCond [| bNumber; cNumber;|]
        let checkExitNode' = checkExitNode tokenToNumber exitCond

        let blockToChildren = dict [
                                    aNumber, [bNumber;];
                                    bNumber, [aNumber; cNumber];
                                    cNumber, [aNumber;];
                                   ]

        let checkChildren' = checkChildren tokenToNumber blockToChildren

        let blockToParents = dict[
                                    aNumber, [bNumber; cNumber];
                                    bNumber, [aNumber;];
                                    cNumber, [bNumber;];
                                ]
        let checkParents' = checkParent tokenToNumber blockToParents
        let myChecks = [checkChildren'; checkParents']

        let expectedNodes = 5
        let expectedBlocks = 5
        let prefix = "`Cycle inside cycle ((AB)+C)+"
        //action
        let cfg = buildCfg qGraph parse createCfg astToDot tokToRealString prefix
        //asserts
        runTest cfg expectedBlocks expectedNodes checkEntryNode' checkExitNode' myChecks


//[<EntryPoint>]
let f x = 
    //let cfgBuilding = new ``Simple cases``()
    //cfgBuilding.``Ambiguous test``()
    //cfgBuilding.``Ambiguous2 test``()
    
    //let cycleBuilding = new ``Cycles``()
    //cycleBuilding.``Cycle (A | B)+``()
    //cycleBuilding.``Cycle (AB)+C``()
    //cycleBuilding.``Simple Cycle``()
    //cycleBuilding.Cycle()
//    cfgBuilding.``Ambiguous test``()
//    cfgBuilding.``Ambiguous2 test``()
    //let ifBuilding = new ``If statements``()
    //ifBuilding.``If without else test``()
    //let undefVariables = new ``Find undefined variables``()
//    undefVariables.``Undef: ambiguous``()
    1