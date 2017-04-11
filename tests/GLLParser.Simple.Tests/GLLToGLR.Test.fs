module GLLToGLR
open NUnit.Framework
open YaccConstructor.API
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.Conversions
open Yard.Generators.Common


let grammarFilesPath = @"C:\Users\ilya\Documents\projects\YaccConstructor\Tests\GLLParser.Simple.Tests\"

module GLLToGLR =
    open Microsoft.FSharp.Reflection
    open Yard.Generators.Common
    let getParserSource grammarFile =    
        generate grammarFile
                 "YardFrontend" "GLLGenerator" 
                 None
                 ["ExpandMeta"; "ExpandEbnf"; "ExpandInnerAlt"] 
                 [] :?> ParserSourceGLL
   
    let private tokenToString (token : 'a) =
        match FSharpValue.GetUnionFields(token, typeof<'a>) with
            | case, _ -> case.Name.ToUpper()

    let createGllInput tokens stringToGLLToken withErrors =
        let notEof =
            tokenToString >> ((<>) "RNGLR_EOF")
        let stringifiedTokens = 
            tokens
            |> Array.filter notEof
            |> Array.map (tokenToString >> stringToGLLToken)
        if withErrors
        then
            new LinearIputWithErrors(stringifiedTokens, stringToGLLToken "ERROR") :> IParserInput
        else
            new LinearInput(stringifiedTokens) :> IParserInput

    let termToIndex intToString withErrors (term : ASTGLLFSA.TerminalNode) =
        let tokenIndex = (term.Extension |> ASTGLL.getRightExtension) - 1
        if withErrors
        then
            if intToString <| int term.Name <> "ERROR" then 2 * tokenIndex else  2 * tokenIndex + 1
        else
            tokenIndex

    let toRnglTree (root : AstNode.AstNode) (createErrorToken: _ option) rules tokens withErrors =
        let tokensWithErrors =
            tokens
            |> Array.map (fun t -> Array.append [|t|] <| if withErrors && tokenToString t <> "RNGLR_EOF" then [|createErrorToken.Value t|] else Array.empty)
            |> Array.concat
        new AST.Tree<_>(tokensWithErrors, root, rules)


let testConversion grmmarFile tokens expected (createErrorToken: _ option) rules translate withErrors =
    let parser = GLLToGLR.getParserSource <| grammarFilesPath + grmmarFile
    let input = GLLToGLR.createGllInput tokens parser.StringToToken withErrors
    let tree = buildAst parser input
    let intToString i = if i = -1 then "-1" else parser.IntToString.[i]
    printfn "Before:\n%s" <| tree.StringRepr intToString
    if withErrors
        then tree.ChooseSingleAst <| (=)(parser.StringToToken "ERROR")
    printfn "After:\n%s" <| tree.StringRepr intToString
    let root = tree.SelectBiggestRoot()
    let glrRoot = gllNodeToGlr root parser.RightSideToRule intToString <| GLLToGLR.termToIndex intToString withErrors
    let glrTree = GLLToGLR.toRnglTree glrRoot createErrorToken rules tokens withErrors
    glrTree.PrintAst()

    let tokenToRange _ = 0, 0
    let translateArgs : AST.TranslateArguments<_,_> = {        
        tokenToRange = tokenToRange
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    let errorDict = new AST.ErrorDictionary<_>()
    let translated = translate translateArgs glrTree errorDict :> string list |> List.head
    printfn "%s" translated

    Assert.AreEqual (expected, translated)


[<TestFixture>]
type ``GLL to RNGLR conversion test`` () =
    [<Test>]
    member test.``Simple calc without erros in input``() =     
        let tokens = [
                    SimpleCalcWithoutErrors.NUM "1"
                    SimpleCalcWithoutErrors.TIMES ""
                    SimpleCalcWithoutErrors.NUM "2"
                    SimpleCalcWithoutErrors.TIMES ""
                    SimpleCalcWithoutErrors.NUM "3"
                    SimpleCalcWithoutErrors.PLUS ""
                    SimpleCalcWithoutErrors.NUM "4"
                    SimpleCalcWithoutErrors.TIMES ""
                    SimpleCalcWithoutErrors.NUM "5"
                    SimpleCalcWithoutErrors.RNGLR_EOF ""] |> Array.ofList
        testConversion
            @"SimpleCalcWithoutErrors.yrd"
            tokens
            "1 * 2 * 3 + 4 * 5"
            None
            SimpleCalcWithoutErrors.parserSource.Rules
            SimpleCalcWithoutErrors.translate
            false

    [<Test>]
    member test.``Simple calc with erros in input``() =     
        let tokens = [
                    SimpleCalcWithErrors.NUM "1"
                    SimpleCalcWithErrors.PLUS ""
                    SimpleCalcWithErrors.TIMES ""
                    SimpleCalcWithErrors.NUM "3"
                    SimpleCalcWithErrors.RNGLR_EOF ""] |> Array.ofList
        testConversion
            @"SimpleCalcWithErrors.yrd"
            tokens
            "1 + ERR"
            (Some SimpleCalcWithErrors.ERROR)
            SimpleCalcWithErrors.parserSource.Rules
            SimpleCalcWithErrors.translate
            true
