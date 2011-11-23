module CommonTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open NUnit.Framework
open System.Linq
open System.IO

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

[<TestFixture>]
type ``Checker test`` () =
    let frontend = Yard.Frontends.YardFrontend.YardFrontend() :> IFrontend
    let basePath = @"..\..\..\..\Tests\Checkers\"

    [<Test>]
    member test.``Start rule exists. No start rule.`` () =
        Path.Combine(basePath, "no_start_rule.yrd")
        |> frontend.ParseGrammar
        |> IsStartRuleExists
        |> Assert.IsFalse

    [<Test>]
    member test.``Start rule exists. One start rule.`` () =
        Path.Combine(basePath, "one_start_rule.yrd")
        |> frontend.ParseGrammar
        |> IsStartRuleExists
        |> Assert.IsTrue

    [<Test>]
    member test.``Start rule exists. Two start rules.`` () =
        Path.Combine(basePath, "two_start_rules.yrd")
        |> frontend.ParseGrammar
        |> IsStartRuleExists
        |> Assert.IsTrue

    [<Test>]
    member test.``Single start rule. No start rule.`` () =
        Path.Combine(basePath, "no_start_rule.yrd")
        |> frontend.ParseGrammar
        |> IsSingleStartRule
        |> Assert.IsFalse

    [<Test>]
    member test.``Single start rule. One start rule.`` () =
        Path.Combine(basePath, "one_start_rule.yrd")
        |> frontend.ParseGrammar
        |> IsSingleStartRule
        |> Assert.IsTrue

    [<Test>]
    member test.``Single start rule. Two start rules.`` () =
        Path.Combine(basePath, "two_start_rules.yrd")
        |> frontend.ParseGrammar
        |> IsSingleStartRule
        |> Assert.IsFalse

    [<Test>]
    member test.``Undeclared nonterminals. Right grammar.`` () =
        let result =
            Path.Combine(basePath, @"UndeclaredNonterminals\MetaRules_Correct.yrd")
            |> frontend.ParseGrammar
            |> GetUndeclaredNonterminalsList
            |> List.sort
        let expetedResult = []
        Seq.iter (printfn "%A;") result
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult  
        Assert.AreEqual(result,expetedResult)

    [<Test>]
    member test.``Undeclared nonterminals. Wrong grammar.`` () =
        let result =
            Path.Combine(basePath, @"UndeclaredNonterminals\MetaRules_Uncorrect.yrd")
            |> frontend.ParseGrammar
            |> GetUndeclaredNonterminalsList
            |> List.sort
        let expetedResult = List.sort ["b"; "x"; "y"; "w"; "d"]
        Seq.iter (printfn "%A;") result
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult 
        Assert.AreEqual(result,expetedResult)