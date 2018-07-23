module CommonTests

open Yard.Core
open Yard.Core.IL
open Yard.Core.Checkers
open NUnit.Framework
open System.Linq
open System.IO
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.TreeDump
open Yard.Generators.YardPrinter
//open Yard.Generators.RIGLRGenerator
open Yard.Frontends.FsYaccFrontend
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions
open Yard.Core.Conversions.ExpandInline

let basePath = Path.Combine(__SOURCE_DIRECTORY__, "..", "data", "Checkers") + Path.DirectorySeparatorChar.ToString()

[<TestFixture>]
type ``Components loader tests`` () =
    [<Test>]
    member test.``All generators`` () =
        let generatorsManager = ([|new GLL(); new RNGLR(); new TreeDump(); new YardPrinter()(*, new RIGLR()*)|]: Generator []) |> Seq.ofArray |> Seq.cast<Generator>
        let generatorNames = Seq.map (fun (elem: Generator) -> elem.Name) generatorsManager
        let allGenerators =
            List.ofSeq generatorNames
            |> List.sort
        let expetedResult =
            ["GLLGenerator"; "RNGLRGenerator"; "TreeDump"; "YardPrinter"(*;"RIGLRGenerator"*)]
            |> List.sort
        Seq.iter (printfn "%A;") allGenerators
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult
        Assert.AreEqual(expetedResult |> List.sort, allGenerators |> List.sort)



    [<Test>]
    member test.``All frontends`` () =
        let frontendsManager = ([|new FsYaccFrontend(); new YardFrontend()|]: Frontend []) |> Seq.ofArray  |> Seq.cast<Frontend>
        let frontendNames = Seq.map (fun (elem: Frontend) -> elem.Name) frontendsManager
        let allFrontends =
            List.ofSeq frontendNames
            |> List.sort
        let expetedResult =
            ["FsYaccFrontend"; "YardFrontend"]
            |> List.sort
        Seq.iter (printfn "%A;") allFrontends
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult
        Assert.AreEqual(expetedResult, allFrontends)

    (*
    [<Test>]
    member test.``All conversions`` () =
        let conversionsManager = ([| new AddDefaultAC.AddDefaultAC(); new AddEOF.AddEOF(); new BuildAST.BuildAST(); new BuildAstSimple.BuildAstSimple(); new CNFandBNF.CNF();
                                    new CNFandBNF.BNFconj(); new CNFandBNF.BNFbool(); new EliminateLeftRecursion.EliminateLeftRecursion(); new RegularApproximation.RegularApproximation();
                                    new ExpandTopLevelAlt.ExpandTopLevelAlt(); new ExpandBrackets.ExpandBrackets(); new ExpandEbnfStrict.ExpandEbnf(); new ExpandInnerAlt.ExpandInnerAlt();
                                    new ExpandMeta.ExpandMeta(); new LeaveLast.LeaveLast(); new MergeAlter.MergeAlter(); new RemoveAST.RemoveAC(); new ExpandInline.ReplaceInline();
                                    new ReplaceLiterals.ReplaceLiterals(); new Linearize.Linearize(); new ExpandRepet.ExpandExpand(); new ExpandConjunction.ExpandConjunction()|]: Conversion [])
                                    |> Seq.ofArray |> Seq.cast<Conversion>
        let conversionNames = Seq.map (fun (elem : Conversion) -> elem.Name) conversionsManager
        let allConversions =
            List.ofSeq conversionNames
            |> List.sort
        let expetedResult =
             ["AddDefaultAC"; "AddEOF"; "BuildAST"; "BuildAstSimple"; "ToCNF"; "DeleteChainRule"; "DeleteEpsRule"; "SplitLongRule"; "RenameTerm";
              "RegularApproximation"; "EliminateLeftRecursion"; "ExpandTopLevelAlt"; "ExpandBrackets"; "ExpandEbnf"; "ExpandInnerAlt"; "ExpandMeta";
              "LeaveLast"; "MergeAlter"; "RemoveAC"; "ReplaceInline"; "ReplaceLiterals"; "Linearize"; "ExpandRepeat"; "ExpandConjunction"]
             |> List.sort
        Seq.iter (printfn "%A;") allConversions
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult
        Assert.AreEqual(expetedResult |> List.sort, allConversions |> List.sort)
    *)

    [<Test>]
    member test.``Get generators name`` () =
        let generatorsManager = ([|new RNGLR(); new TreeDump()|]: Generator[]) |> Seq.ofArray |> Seq.cast<Generator>
        let VerificatedGenerators  = ["RNGLRGenerator",true ; "TreeDump",true]

        let genfun (x,y)  =
            match (Seq.tryFind (fun (elem : Generator) -> elem.Name = x) generatorsManager) with
                | Some _ -> true
                | None   -> false

        let allGettingGenerators = List.map genfun VerificatedGenerators

        List.iter (fun vg ->  (vg |> snd |> printfn "%A : "); (vg |> fst |> printfn "%A;"))  VerificatedGenerators
        printfn "**********************"
        List.iter (printfn "%A;") allGettingGenerators
        Assert.AreEqual(VerificatedGenerators |> List.map (fun vg ->   vg |> snd),allGettingGenerators)

[<TestFixture>]
type ``Checker test`` () =
    let frontend = Yard.Frontends.YardFrontend.YardFrontend() :> Frontend

    let getUndecl path =
        path
        |> frontend.ParseGrammar
        |> GetUndeclaredNonterminalsList
        |> (fun (_,_,u) -> u)
        |> List.map snd
        |> List.concat
        |> List.map (fun r -> r.text)
        |> List.sort

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
    member test.``Undeclared nonterminals checker. Metarules. Right grammar.`` () =
        let result =
            Path.Combine(basePath, "UndeclaredNonterminals", "MetaRules_Correct.yrd")
            |> getUndecl
        let expetedResult = []
        Seq.iter (printfn "%s;") result
        printfn "**********************"
        Seq.iter (printfn "%s;") expetedResult
        Assert.AreEqual(result,expetedResult)

    [<Test>]
    member test.``Undeclared nonterminals checker. Metarules. Wrong grammar.`` () =
        let result =
            Path.Combine(basePath, "UndeclaredNonterminals", "MetaRules_Incorrect.yrd")
            |> getUndecl
        let expetedResult = List.sort ["b"; "x"; "y"; "w"; "d"]
        Seq.iter (printf "%s; ") result
        printfn ""
        printfn "**********************"
        Seq.iter (printf "%s; ") expetedResult
        Assert.AreEqual(result, expetedResult)

    [<Test>]
    member test.``Undeclared nonterminals checker. Simple. Right grammar.`` () =
        let result =
            Path.Combine(basePath, "UndeclaredNonterminals", "Simple_Correct.yrd")
            |> getUndecl
        let expetedResult = []
        Seq.iter (printfn "%A;") result
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult
        Assert.AreEqual(result,expetedResult)

    [<Test>]
    member test.``Undeclared nonterminals checker. Simple. Wrong grammar.`` () =
        let result =
            Path.Combine(basePath, "UndeclaredNonterminals", "Simple_Uncorrect.yrd")
            |> getUndecl
        let expetedResult = List.sort ["b"]
        Seq.iter (printfn "%A;") result
        printfn "**********************"
        Seq.iter (printfn "%A;") expetedResult
        Assert.AreEqual(result,expetedResult)

    [<Test>]
    member test.``Unused nonterminals checker. Metarules. Right grammar.`` () =
        Path.Combine(basePath, "UnusedNonterminals", "MetaRules_Correct.yrd")
        |> frontend.ParseGrammar
        |> IsUnusedRulesExists
        |> Assert.IsFalse

    [<Test>]
    member test.``Unused nonterminals checker. Metarules. Wrong grammar.`` () =
        Path.Combine(basePath, "UnusedNonterminals", "MetaRules_Uncorrect.yrd")
        |> frontend.ParseGrammar
        |> IsUnusedRulesExists
        |> Assert.IsTrue

    [<Test>]
    member test.``Metarules arguments count.`` () =
        let grammar = frontend.ParseGrammar <| Path.Combine(basePath, "Metarules_args_count.yrd")
        match (GetIncorrectMetaArgsCount grammar) with
        | [] -> Assert.Fail("Errors do exist!!")
        | x ->
            x |> List.iter (fun (m, r) ->
                printfn "%s:" (getModuleName m)
                r
                |> List.map (fun (rule, got, expected) -> sprintf "%s(%d,%d): %d (expected %d)" rule.text rule.startPos.line rule.startPos.column got expected)
                |> String.concat System.Environment.NewLine
                |> printfn "%s"
            )
