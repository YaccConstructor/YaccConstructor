module ControlFlowGraph.Test.CfgFunctionsTest

open Microsoft.FSharp.Collections
open NUnit.Framework
open System.Collections.Generic

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.Test.CommonHelper

open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation

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

let fsaInfo : FsaParams<char, char * Position<_>> =
                {
                    Alphabet = new HashSet<_>(alphabet);
                    NewSymbol = createNewSymbol;
                    GetChar = getChar;
                    SymbolsAreEqual = areSymbolsEqual;
                    SeparatorSmbl1 = '~';
                    SeparatorSmbl2 = '^';
                }


let runTest tokenToFsa (errorList : _ list * _ list) (expected : _ list * _ list) = 
    
    let actualGuaranteed, actualPotential = errorList
    let expectedGuaranteed, expectedPotential = expected
        
    //printfn "%A" errorList
    Assert.AreEqual(expectedGuaranteed.Length, actualGuaranteed.Length, "Guaranteed errors")
    Assert.AreEqual(expectedPotential.Length, actualPotential.Length, "Potential errors")
        
    let areEqual one two = 
        let fsa1 = tokenToFsa one
        let fsa2 = tokenToFsa two
        areEqualFSA fsa1 fsa2 fsaInfo

    let check expected actual =
        expected
        |> List.forall (fun var -> actual |> List.exists (areEqual var))
        |> fun res -> Assert.True(res, "Not all expected variables are found")

    check expectedGuaranteed actualGuaranteed
    check expectedPotential actualPotential

[<TestFixture>]
type ``Find undefined variables``() =
    
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
        
    let varsNumbers = [ExtendedCalcTest.Parser.ID fsa] |> List.map tokenToNumber

    let isVariable tok = varsNumbers |> List.exists ((=) tok) 

    let tokToRealString = tokenToNumber >> indToString

    let xVariable = ExtendedCalcTest.Parser.ID <| createFSA 'x' 
    let yVariable = ExtendedCalcTest.Parser.ID <| createFSA 'y'
    let zVariable = ExtendedCalcTest.Parser.ID <| createFSA 'z'
    
    let parserSource = new GeneratedStuffSource<_, string>(tokenToNumber, indToString, leftSides, tokenData, fsaInfo)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let createParserInput' = createParserInputGraph ExtendedCalcTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = CfgBuilder.CfgBuilder.BuildCfg tree parserSource langSource tokToRealString

    let buildCfg' = buildCfg parse createCfg astToDot tokToRealString

    let act graph prefix = 
        let cfg = buildCfg' graph prefix
        let guaranteedErrors, potentialErrors = cfg.FindUndefinedVariables()

        guaranteedErrors, potentialErrors

    [<Test>]
    member test.``X = Z; Y = X;``() = 
        let qGraph = createParserInput' "X = Z; Y = X.dot"

        let expected = [zVariable], []
        let prefix = "`cfg undefined variables X = Z; Y = X"
        
        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected

    [<Test>]
    member test.``X = X``() = 
        let qGraph = createParserInput' "X = X.dot"

        let expected = [xVariable], []
        let prefix = "`cfg undefined variables X = X"

        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected


    [<Test>]
    member test.``Ambiguous``() =
        let qGraph = createParserInput' "Ambiguous.dot"
        
        let expected = [yVariable; zVariable], []
        let prefix = "`cfg undefined variables ambiguous"
            
        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected

            
    [<Test>]
    member test.``X = 1; [Y = 2]; X = Y + 2;``() =
        let qGraph = createParserInput' "Ambiguous3.dot"
        
        let expected = [yVariable], []
        let prefix = "`cfg undefined variables ambiguous2"

        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected

    [<Test>]
    member this.``Cycle inside expression``() = 
        let qGraph = createParserInput' "X = 1 [+Y].dot"
        
        let prefix = "`cfg cycle inside expression"

        let expected = [yVariable], []

        //act
        let errorList = act qGraph prefix
        
        //assert
        runTest tokenToFSA errorList expected

    [<Test>]
    member this.``{x, y} = 1; {z} = {x} + {y}``() = 
        let qGraph = createParserInput' "MaybeUndefinedError.dot"
        
        let prefix = "`MaybeUndefinedError"

        let expected = [], [xVariable; yVariable]

        //act
        let errorList = act qGraph prefix
        
        //assert
        runTest tokenToFSA errorList expected
    
     
type ``Scope test``() = 
    let parse = LetTest.Parser.buildAstAbstract
    let tokenToNumber = LetTest.Parser.tokenToNumber
    let leftSides = LetTest.Parser.leftSide
    let indToString = LetTest.Parser.numToString
    let tokenData = LetTest.Parser.tokenData

    let tokenToFSA token = 
        let res = tokenData token
        res :?> FSA<_>

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

    let tokToRealName = tokenToNumber >> indToString

    let x = LetTest.Parser.ID <| createFSA 'x' 
    let y = LetTest.Parser.ID <| createFSA 'y' 
    let z = LetTest.Parser.ID <| createFSA 'z'

    let varNumber = tokenToNumber x
    let isVariable tok = varNumber = tok
        
    let parserSource = new GeneratedStuffSource<_, string>(tokenToNumber, indToString, leftSides, tokenData, fsaInfo)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let createParserInput' = createParserInputGraph LetTest.Lexer.tokenize RNGLR_EOF
    let createCfg tree = CfgBuilder.CfgBuilder.BuildCfg tree parserSource langSource tokToRealName

    let buildCfg' = buildCfg parse createCfg astToDot tokToRealName

    let act graph prefix = 
        let cfg = buildCfg' graph prefix
        let guaranteedErrors, potentialErrors = cfg.FindUndefinedVariables()

        

        guaranteedErrors, potentialErrors

    [<Test>]
    member this.``Scope1``() = 
        let qGraph = createParserInput' "Scope.dot"
        
        let prefix = "`scope"

        let expected = [y], []
        
        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected
    
    [<Test>]
    member this.``Scope2``() = 
        let qGraph = createParserInput' "Scope2.dot"
        
        let prefix = "`scope2"

        let expected = [], []
        
        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected

    [<Test>]
    member this.``Scope3``() = 
        let qGraph = createParserInput' "Scope3.dot"
        
        let prefix = "`scope3"

        let expected = [], []
        
        //act
        let errors = act qGraph prefix
        
        //assert
        runTest tokenToFSA errors expected
        
//[<EntryPoint>]
let f x = 
    let functions =  ``Find undefined variables``()
    functions.Ambiguous()
    //functions.``X = Z; Y = X;``()
    //functions.``Ambiguous 2``()
    //functions.``X = Z; Y = X;``()
    (*let scopeTest = ``Scope test``()
    scopeTest.Scope1()*)
    1