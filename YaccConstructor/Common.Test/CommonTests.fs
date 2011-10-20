module CommonTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open NUnit.Framework
open System.Linq

[<TestFixture>]
type ``Components loader tests`` () =
    [<Test>]
    member test.``All generators`` () =
        let allGenerators = 
            List.ofSeq GeneratorsManager.AvailableGenerators
            |> List.sort
        let expetedResult = 
            //["FParsecGenerator";"FsYaccPrinter";"GNESCCGenerator";"TreeDump";"YardPrinter"]
            ["FParsecGenerator";"FsYaccPrinter";"GNESCCGenerator";"YardPrinter"]
            |> List.sort
        Seq.iter (printfn "%A;") allGenerators
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult        
        Assert.AreEqual(allGenerators,expetedResult)
    
    [<Test>]
    member test.``All frontends`` () =
        let allFrontends = 
            List.ofSeq FrontendsManager.AvailableFrontends
            |> List.sort
        let expetedResult =
            ["AntlrFrontend";"FsYaccFrontend";"YardFrontend"]
            |> List.sort
        Seq.iter (printfn "%A;") allFrontends
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult        
        Assert.AreEqual(allFrontends,expetedResult)

        

    [<Test>]
    member test.``All conversions`` () =
        let allConversions = 
            List.ofSeq ConvertionsManager.AvailableConvertions
            |> List.sort
        let expetedResult =
            ["AddDefaultAC";"BuildAstSimple";"MergeAlter";"ExpandEbnf";"BuildAST";"LeaveLast"
             ;"ReplaceLiterals";"AddEOF";"ExpandBrackets";"ExpandMeta";"ExpandEbnfStrict"
             ;"ExpandAlter"]
            |> List.sort
        Seq.iter (printfn "%A;") allConversions
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult        
        Assert.AreEqual(allConversions,expetedResult)