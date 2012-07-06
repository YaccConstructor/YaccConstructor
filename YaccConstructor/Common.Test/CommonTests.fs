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
        let GeneratorsManager = GeneratorsManager.GeneratorsManager()
        let allGenerators = 
            List.ofSeq GeneratorsManager.Available
            |> List.sort
        let expetedResult = 
            ["FParsecGenerator";"FsYaccPrinter";"GNESCCGenerator";"TreeDump";"YardPrinter"]
            //["FParsecGenerator";"FsYaccPrinter";"GNESCCGenerator";"YardPrinter"]
            |> List.sort
        Seq.iter (printfn "%A;") allGenerators
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult        
        Assert.AreEqual(allGenerators,expetedResult)
    


    [<Test>]
    member test.``All frontends`` () =
        let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager() 
        let allFrontends = 
            List.ofSeq FrontendsManager.Available
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
        let ConvertionsManager = ConvertionsManager.ConvertionsManager()
        let allConversions = 
            List.ofSeq ConvertionsManager.Available
            |> List.sort
        let expetedResult =
            ["AddDefaultAC"; "AddEOF"; "BuildAST"; "BuildAstSimple"; "EliminateLeftRecursion";
             "ExpandAlter"; "ExpandBrackets"; "ExpandEbnf"; "ExpandInnerAlt"; "ExpandMeta"; "LeaveLast"; "MergeAlter";
             "RemoveAC"; "ReplaceInline"; "ReplaceLiterals";]
            |> List.sort
        Seq.iter (printfn "%A;") allConversions
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult        
        Assert.AreEqual(allConversions,expetedResult)

    
    [<Test>]
    member test.``Get generators name`` () =
        let GeneratorsManager = GeneratorsManager.GeneratorsManager()
        let VerificatedGenerators  = [("FParsecGenerator",true);("FsYaccPrinter",true);("GNESCCGenerator",true);("TreeDump",true);("YardPrinter",true)]

        let genfun (x,y)  = 
            match (x |> GeneratorsManager.Component  ) with
                | Some _ -> true
                | None -> false
        
        let allGetingGenerators = List.map genfun VerificatedGenerators

        List.iter (fun vg ->  (vg |> snd |> printfn "%A : "); (vg |> fst |> printfn "%A;"))  VerificatedGenerators
        printfn "**********************"
        List.iter (printfn "%A;") allGetingGenerators 
        Assert.AreEqual(VerificatedGenerators |> List.map (fun vg ->   vg |> snd),allGetingGenerators)

[<TestFixture>]
type ``Checker test`` () =
    let frontend = Yard.Frontends.YardFrontend.YardFrontend() :> Frontend
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
    member test.``Undeclared nonterminals checker. Right grammar.`` () =
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
    member test.``Undeclared nonterminals checker. Wrong grammar.`` () =
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

    [<Test>]
    member test.``Unused nonterminals checker. Right grammar.`` () =
        Path.Combine(basePath, @"UnusedNonterminals\MetaRules_Correct.yrd")
        |> frontend.ParseGrammar
        |> IsUnusedRulesExists
        |> Assert.IsFalse

    [<Test>]
    member test.``Unused nonterminals checker. Wrong grammar.`` () =
        Path.Combine(basePath, @"UnusedNonterminals\MetaRules_Uncorrect.yrd")
        |> frontend.ParseGrammar
        |> IsUnusedRulesExists
        |> Assert.IsTrue