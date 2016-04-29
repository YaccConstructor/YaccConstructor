module ControlFlowGraph.Test.CfgBuildingTest

open Microsoft.FSharp.Collections
open NUnit.Framework

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.Test.CommonHelper
open ControlFlowGraph.Test.BlockHelper

open QuickGraph.FSA.GraphBasedFsa

let returnTrue = fun _ -> true

let incorrectChildMsg = "At least one block has incorrect child"
let incorrectParentMsg = "At least one block has incorrect parent"
let missingBlockMsg = "Some blocks are lost"
let incorrectEntryMsg = "Incorrect entry node"
let incorrectExitMsg = "Incorrect exit node"

let createFSA character = 
    let startState = ResizeArray.singleton 0
    let finishState = ResizeArray.singleton 1
    let transitions = new ResizeArray<_>()
    transitions.Add(0, Smbl(character, Unchecked.defaultof<_>), 1)
    new FSA<_>(startState, finishState, transitions)

let runTest (cfg : ControlFlow<_, _>) checkEntry checkExit checks = 
            
    Assert.IsTrue(checkEntry cfg.Entry, incorrectEntryMsg)
    Assert.IsTrue(checkExit cfg.Exit, incorrectExitMsg)
            
    let checkCondition condition = 
        let func, msg = condition
        let res = func cfg.Blocks
        Assert.True(res, msg)

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
    let RNGLR_EOF = ExtendedCalcTest.Parser.RNGLR_EOF fsa

    let xVariable = ExtendedCalcTest.Parser.ID <| createFSA 'x'
    let yVariable = ExtendedCalcTest.Parser.ID <| createFSA 'y'
    let zVariable = ExtendedCalcTest.Parser.ID <| createFSA 'z'

    let semicolonNumber = tokenToNumber <| ExtendedCalcTest.Parser.SEMICOLON fsa

    let nodeToType = dict 
                        [
                            "assign", Assignment;
                            "id", Identificator;
                            "expr", Expression;
                        ]
        
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new GeneratedStuffSource<_, _>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    
    let createCfg tree = CfgBuilder.CfgBuilder.BuildCfg tree parserSource langSource tokToRealString
        
    let buildCfg' = buildCfg parse createCfg astToDot tokToRealString

    let areEqualTokens one two = 
        let oneNumber = tokenToNumber one
        let twoNumber = tokenToNumber two
        oneNumber = twoNumber

    [<Test>]
    member test.``Elementary test``() =
        let qGraph = createParserInput' "Seq.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue

        let nodeToChildren = dict [xVariable, [yVariable]; yVariable, [zVariable]; zVariable, [];]
        let myChildrenCheck = checkChildren areEqualTokens nodeToChildren

        let nodeToParents = dict [xVariable, []; yVariable, [xVariable]; zVariable, [yVariable];]
        let myParentsCheck = checkParent areEqualTokens nodeToParents

        let myConds = 
            [
                myChildrenCheck, incorrectChildMsg; 
                myParentsCheck, incorrectParentMsg;
            ]
        
        let prefix = "`elementary"

        //act 
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' myConds

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = createParserInput' "Ambiguous.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue

        let blockToChildren = dict [yVariable, [xVariable]; zVariable, [xVariable];]
        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[xVariable, [yVariable; zVariable]]
            //dict [(*yVariable, [xVariable]; zVariable, [xVariable];*)]
        let checkParents' = checkParent areEqualTokens blockToParents
        let myChecks = 
            [
                checkChildren', incorrectChildMsg;
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`ambiguous"
        //act 
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' myChecks
        
    [<Test>]
    member this.``Ambiguous2 test``() = 
        let qGraph = createParserInput' "Ambiguous2.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue

        let blockToChildren = dict [yVariable, []; zVariable, [];]
        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict [yVariable, []; zVariable, [];]
        let checkParents' = checkParent areEqualTokens blockToParents
        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`ambiguous2"
        //act 
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' myChecks
        
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
                                "cond", Identificator;
                          ]
    
    let tokenToString = tokenToNumber >> indToString

    let keywordToInt = dict [
                                    Keyword.SEMICOLON, semicolonNumber;
                                    Keyword.IF, ifNumber;
                                    Keyword.THEN, thenNumber;
                                    Keyword.ELSE, elseNumber;
                            ]

    let parserSource = new GeneratedStuffSource<_, _>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createParserInput' = createParserInputGraph IfTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = CfgBuilder.CfgBuilder.BuildCfg tree parserSource langSource tokenToString

    let buildCfg' = buildCfg parse createCfg astToDot tokenToString

    [<Test>]
    member test.``Simple If test``() =
        let qGraph = createParserInput' "Simple if.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue

        let prefix = "`simple if"
        
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' []

    [<Test>]
    member test.``Big If test``() =
        let qGraph = createParserInput' "Big if.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue
        let prefix = "`big if"
        
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' []

    [<Test>]
    member test.``If without else test``() =
        let qGraph = createParserInput' "If without else.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue

        let prefix = "`if without else"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' []
            
    [<Test>]
    member test.``Inner if``() =
        let qGraph = createParserInput' "Inner if.dot"

        let checkEntry' = checkEntryNode returnTrue
        let checkExit' = checkExitNode returnTrue
        let prefix = "`inner if"
        
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        runTest cfg checkEntry' checkExit' []
        
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

    let aToken = SimpleTest.Parser.A <| createFSA 'a'
    let bToken = SimpleTest.Parser.B <| createFSA 'b'
    let cToken = SimpleTest.Parser.C <| createFSA 'c'
    let semicolonNumber = tokenToNumber <| SimpleTest.Parser.SEMICOLON fsa
        
    let nodeToType = dict
                        [
                            "assign", Assignment;
                            "id", Identificator;
                        ]
    let keywordToInt = dict [Keyword.SEMICOLON, semicolonNumber;]

    let tokToRealString = tokenToNumber >> indToString
    let parserSource = new GeneratedStuffSource<_, _>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)
    
    let createParserInput' = createParserInputGraph SimpleTest.Lexer.tokenize RNGLR_EOF

    let createCfg tree = CfgBuilder.CfgBuilder.BuildCfg tree parserSource langSource tokToRealString

    let buildCfg' = buildCfg parse createCfg astToDot tokToRealString

    
    let areEqualTokens one two = 
        let oneNumber = tokenToNumber one
        let twoNumber = tokenToNumber two
        oneNumber = twoNumber

    //At least one token from the expected set must exist in the tokenSet set
    let myCond expected tokenSet = 
        expected
        |> Array.exists(fun token -> tokenSet |> Array.exists (areEqualTokens token))


    [<Test>]
    member this.``Cycle A+``() = 
        let qGraph = createParserInput' "A+.dot"

        let checkEntryNode' = checkEntryNode returnTrue
        let checkExitNode' = checkExitNode returnTrue

        let blockToChildren = dict [aToken, [aToken];]
        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict [aToken, [aToken];]
        let checkParents' = checkParent areEqualTokens blockToParents

        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle A+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A B*``() = 
        let qGraph = createParserInput' "A B(asteriks).dot"

        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond
        
        let exitCond = myCond [|aToken; bToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [aToken, [bToken];]
        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict [bToken, [aToken; bToken];]
        let checkParents' = checkParent areEqualTokens blockToParents

        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle A B asteriks"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A B* C``() = 
        let qGraph = createParserInput' "A B(asteriks) C.dot"

        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond
        
        let exitCond = myCond [|cToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [bToken; cToken];
                                    bToken, [bToken; cToken];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                bToken, [aToken; bToken];
                                cToken, [aToken; bToken];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents

        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle A B asteriks C"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle (A | B)+``() = 
        let qGraph = createParserInput' "(A or B)+.dot"

        //first block has two childs: A and B
        let entryCond = myCond [| aToken; bToken |]
        let checkEntryNode' = checkEntryNode entryCond

        //last block has two childs: A and B
        let exitCond = myCond [| aToken; bToken |]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [aToken; bToken];
                                    bToken, [aToken; bToken];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [aToken; bToken];
                                    bToken, [aToken; bToken];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents
        let checkExistence' = checkExistence areEqualTokens [aToken; bToken;]

        let myChecks = 
            [
                checkExistence', missingBlockMsg;
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]
        let prefix = "`Cycle (A or B)+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle A (B+ | C+)``() = 
        let qGraph = createParserInput' "A (B+ or C+).dot"

        //first block has only one child: A
        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond

        let exitCond = myCond [|bToken; cToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [bToken; cToken];
                                    bToken, [bToken;];
                                    cToken, [cToken;];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    bToken, [aToken; bToken];
                                    cToken, [aToken; cToken];
                                ]
        
        let checkParents' = checkParent areEqualTokens blockToParents

        let checkExistence' = checkExistence areEqualTokens [aToken; bToken; cToken;]

        let myChecks = 
            [
                checkExistence', missingBlockMsg;
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle A (B+ or C+)"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks


    [<Test>]
    member this.``Cycle (AB)+``() = 
        let qGraph = createParserInput' "(AB)+.dot"

        //first block has only one child: A
        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond

        let exitCond = myCond [|bToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [bToken;];
                                    bToken, [aToken;];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [bToken;];
                                    bToken, [aToken;];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents
        
        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle (AB)+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle (AB)+C``() = 
        let qGraph = createParserInput' "(AB)+C.dot"

        //first block has only one child: A
        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond

        let exitCond = myCond [|cToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [bToken;];
                                    bToken, [aToken; cToken];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [bToken;];
                                    bToken, [aToken;];
                                    cToken, [bToken;];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents

        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle (AB)+C"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle after cycle A+B+``() = 
        let qGraph = createParserInput' "A+B+.dot"

        //first block has only one child: A
        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond
        
        //last block has only one child: B
        let exitCond = myCond [|bToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [aToken; bToken;];
                                    bToken, [bToken];
                                   ]
        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [aToken;];
                                    bToken, [aToken; bToken;];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents
        
        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]
        
        let prefix = "`Cycle after cycle A+B+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle inside cycle (A+B)+``() = 
        let qGraph = createParserInput' "(A+B)+.dot"

        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond

        let exitCond = myCond [|bToken|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [aToken; bToken;];
                                    bToken, [aToken; ];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [aToken; bToken;];
                                    bToken, [aToken;];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents
        
        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle inside cycle (A+B)+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

    [<Test>]
    member this.``Cycle inside cycle ((AB)+C)+``() = 
        let qGraph = createParserInput' "((AB)+C)+.dot"

        let entryCond = myCond [|aToken|]
        let checkEntryNode' = checkEntryNode entryCond
        
        let exitCond = myCond [| bToken; cToken;|]
        let checkExitNode' = checkExitNode exitCond

        let blockToChildren = dict [
                                    aToken, [bToken;];
                                    bToken, [aToken; cToken];
                                    cToken, [aToken;];
                                   ]

        let checkChildren' = checkChildren areEqualTokens blockToChildren

        let blockToParents = dict[
                                    aToken, [bToken; cToken];
                                    bToken, [aToken;];
                                    cToken, [bToken;];
                                ]
        let checkParents' = checkParent areEqualTokens blockToParents
        
        let myChecks = 
            [
                checkChildren', incorrectChildMsg; 
                checkParents', incorrectParentMsg;
            ]

        let prefix = "`Cycle inside cycle ((AB)+C)+"
        //action
        let cfg = buildCfg' qGraph prefix
        //asserts
        runTest cfg checkEntryNode' checkExitNode' myChecks

//[<EntryPoint>]
let f x = 
    //let cfgBuilding = new ``Simple cases``()
    //cfgBuilding.``Ambiguous test``()
    //cfgBuilding.``Ambiguous2 test``()
    
    let cycleBuilding = new ``Cycles``()
    cycleBuilding.``Cycle A+``()
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