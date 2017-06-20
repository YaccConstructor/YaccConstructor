module GLLToGLR
open NUnit.Framework
open YC.API
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Microsoft.FSharp.Reflection
open Yard.Generators.Common
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core
open Yard.Core.Conversions.ExpandMeta
open Yard.Core.Conversions.ExpandEbnfStrict
open Yard.Core.Conversions.ExpandInnerAlt


/// for resharper test runner 
let needChangeDirectory = 
    (@"C:\Users\Artem Gorokhov\AppData\Local\JetBrains\Installations\ReSharperPlatformVs14" = System.IO.Directory.GetCurrentDirectory())
    || (@"C:\Users\artem\AppData\Local\JetBrains\Installations\ReSharperPlatformVs14" = System.IO.Directory.GetCurrentDirectory())

let grammarFilesPath = 
    if needChangeDirectory
    then @"C:/Code/YaccConstructor/tests/GLLParser.Simple.Tests/"
    else @"./GLLParser.Simple.Tests/"

let getParserSource grammarFile = 
    let fe = new YardFrontend()
    let gen = new GLL()
    let conv = [|new ExpandMeta(), new ExpandEbnf(), new ExpandInnerAlt()|] |> Seq.cast<Conversion>
    generate grammarFile
                fe gen
                None
                conv
                [|""|]
                [] :?> ParserSourceGLL
   
let private tokenToString (token : 'a) =
    match FSharpValue.GetUnionFields(token, typeof<'a>) with
        | case, _ -> case.Name.ToUpper()

let getInput tokens stringToGLLToken withErrors =
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



let testConversion grmmarFile tokens expected (createErrorToken: _ option) rules translate withErrors =
    let parser = getParserSource <| grammarFilesPath + grmmarFile
    let input = getInput tokens parser.StringToToken withErrors
    let tree = buildAst parser input
    if withErrors
    then tree.ChooseSingleAst <| (=)(parser.StringToToken "ERROR")

    let transArguments : ASTGLLFSA.TranslateArguments<_,_,_> = {
        tokenToRange = fun _ -> 0, 0
        zeroPosition = 0
        createErrorToken = createErrorToken
        intToString = fun i -> parser.IntToString.[i]
        rightToRule = parser.RightSideToRule
        rules = rules
        translate = translate
        withErrors = withErrors
    }

    let translated = tree.Translate tokens transArguments :> string list |> List.head
    Assert.AreEqual (expected, translated)


[<TestFixture>]
type ``GLL to RNGLR conversion test`` () =
    [<Test>]
    member test.``Simple calc without errors in input``() =     
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
    member test.``Simple calc with errors in input``() =     
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
