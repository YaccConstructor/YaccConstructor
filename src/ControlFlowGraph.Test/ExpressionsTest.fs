module ControlFlowGraph.Test.ExpressionTests

open Microsoft.FSharp.Collections
open NUnit.Framework
open System.Collections.Generic

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures
open ControlFlowGraph.CfgTokensGraph
open ControlFlowGraph.Test.CommonHelper
open ControlFlowGraph.Test.ExpressionHelper

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation

type PreviousAndNext<'a>(key : 'a, previous : 'a list, next : 'a list) =
    member this.Key = key
    member this.Previous = previous
    member this.Next = next

//lexer stuff
let alphabet = 
    ['a' .. 'z']
    |> List.append ['A' .. 'Z']
    |> List.append ['0'..'9']
    |> List.append [' '; '\t'; '\r'; '\n']
    |> List.append ['+'; '='; '-'; '*'; '/'; '('; ')'; '['; ']'; ]
    |> List.append [';'; ':'; '.'; ',';]

let getChar (x : Symb<char * Position<_>>) = 
    match x with
    | Smbl (y, _) -> y
    | _ -> invalidArg "x" "Unexpected symb in alphabet of FSA!"

let createNewSymbol x = Smbl(x, Unchecked.defaultof<_>)

let areSymbolsEqual one two = (fst one) = (fst two)

let fsaInfo : FsaParams<char, char * Position<string>> =
                {
                    Alphabet = new HashSet<_>(alphabet);
                    NewSymbol = createNewSymbol;
                    GetChar = getChar;
                    SymbolsAreEqual = areSymbolsEqual;
                    SeparatorSmbl1 = '~';
                    SeparatorSmbl2 = '^';
                }

let assertCfg tokenToFSA (cfg : ControlFlow<_, _>) (expected : PreviousAndNext<_> list) = 
    
    let processExpected graph (conf : PreviousAndNext<_>) = 
        
        let areEqualTokens one two =
            let fsa1 = tokenToFSA one
            let fsa2 = tokenToFSA two
            areEqualFSA fsa1 fsa2 fsaInfo
        
        let inTags = getInTags areEqualTokens graph conf.Key

        let checkInEdges = 
            if Seq.isEmpty inTags 
            then 
                true
            else
                inTags
                |> Seq.exists (fun num -> conf.Previous |> List.exists (areEqualTokens num))
        Assert.True(checkInEdges, "There is incorrect in edge")

        let outTags = getOutTags areEqualTokens graph conf.Key
        let checkOutEdges = 
            if Seq.isEmpty inTags 
            then 
                true
            else
                outTags
                |> Seq.exists (fun num -> conf.Next |> List.exists (areEqualTokens num))
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
    
    let tokenToFSA token = 
        let res = tokenData token
        res :?> FSA<_>

    let astToDot = ExtendedCalcTest.Parser.defaultAstToDot

    let fsa = new FSA<_>()
    let RNGLR_EOF = ExtendedCalcTest.Parser.RNGLR_EOF <| fsa
    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    
    let xVariable = ExtendedCalcTest.Parser.ID <| createFSA 'x'
    let yVariable = ExtendedCalcTest.Parser.ID <| createFSA 'y'
    let zVariable = ExtendedCalcTest.Parser.ID <| createFSA 'z'
    let assign = ExtendedCalcTest.Parser.ASSIGN fsa
    let num = ExtendedCalcTest.Parser.NUMBER fsa
    let plus = ExtendedCalcTest.Parser.PLUS fsa
    let minus = ExtendedCalcTest.Parser.MINUS fsa
    let mult = ExtendedCalcTest.Parser.MULT fsa
    let semi = ExtendedCalcTest.Parser.SEMICOLON fsa

    let nodeToType = dict
                        [
                            "assign", Assignment;
                            "id", Identificator;
                            "expr", Expression;
                        ]

    let keywordToInt = dict [Keyword.SEMICOLON, tokenToNumber semi;]

    let tokToRealString = tokenToNumber >> indToString
    let parserSource = new GeneratedStuffSource<_, string>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let createCfg tree = ControlFlow(tree, parserSource, langSource, tokToRealString)
                
    let buildCfg' = buildCfg parse createCfg astToDot tokToRealString

    [<Test>]
    member this.``X = 1 [+Y]*``() = 
        let qGraph = createParserInput' "X = 1 [+Y].dot"

        let prefix = "`X = 1 [+Y]"
        let expected = 
            [
                new PreviousAndNext<_>(yVariable, [plus], [semi]);
                new PreviousAndNext<_>(plus, [num; yVariable], [yVariable]);
                new PreviousAndNext<_>(num, [assign], [plus; semi]);
            ]

        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToFSA cfg expected
        

    [<Test>]
    member this.``X = Y [+1]* - Z``() = 
        let qGraph = createParserInput' "X = Y [+1] - Z.dot"

        let expected = 
            [
                new PreviousAndNext<_>(yVariable, [assign], [plus; minus]);
                new PreviousAndNext<_>(num, [plus], [plus; minus]);
                new PreviousAndNext<_>(zVariable, [minus], [semi]);
            ]

        let prefix = "`X = Y [+1] - Z"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToFSA cfg expected

    [<Test>]
    member this.``X = Y [+1[-Z]*]*``() = 
        let qGraph = createParserInput' "X = Y [+1[-Z]].dot"

        let expected = 
            [
                new PreviousAndNext<_>(yVariable, [assign], [plus; semi]);
                new PreviousAndNext<_>(num, [plus], [plus; minus; semi]);
                new PreviousAndNext<_>(zVariable, [minus], [plus; minus; semi]);
            ]

        let prefix = "`X = Y [+1[-Z]]"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToFSA cfg expected

    [<Test>]
    member this.``X = Y [(+1) | (-Z)]*``() = 
        let qGraph = createParserInput' "X = Y [(+1) or (-Z)].dot"

        let expected = 
                [
                    new PreviousAndNext<_>(yVariable, [assign], [plus; minus; semi]);
                    new PreviousAndNext<_>(num, [plus], [plus; minus; semi]);
                    new PreviousAndNext<_>(zVariable, [minus], [plus; minus; semi]);
                ]

        let prefix = "`X = Y [(+1) or (-Z)]"
        //act
        let cfg = buildCfg' qGraph prefix
        //assert
        assertCfg tokenToFSA cfg expected

//[<EntryPoint>]
let f x = 
    let cycleInsideExpress = new ``Cycles inside expressions``()
    cycleInsideExpress.``X = 1 [+Y]*``()
    0