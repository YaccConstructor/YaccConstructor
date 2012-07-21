open NUnit.Framework
open Yard.Generators.CYKGenerator
open Yard.Frontends.YardFrontend
open System.IO

[<TestFixture>]
type CYKTests () =
    let generator = new Yard.Generators.CYKGenerator.CYKGeneartorImpl()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../../../Tests/CYK"

    [<Test>]
    member test.test1 () =
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_2.yrd"))
        let rules,start = generator.GenRulesList il
        let code = generator.Generate(il)
        let input = [|1us;2us|]
        let res = (new CYKCore()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy)
        Assert.IsTrue(true)
        //let expected = "E->EE  conflict E->EE  l1 E->TX  l1 T->a   X->*   E->TX  l1 T->a   X->*   E->TY  l2 T->a   Y->+  "
        //let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")
        //let result = parser.Recognize testGrammar_lbl1 "a*a*a+" |> fun (s:string) -> s.Replace( "\n", " ")
        //Assert.AreEqual(expected, result)

    [<Test>]
    member test.noLbl () =
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test.yrd"))
        let rules,start = generator.GenRulesList il
        let input = [|1us;1us|]
        let res = (new CYKCore()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy)
        let expected = "undefined 0 0" 
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.oneLbl() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_oneLbl.yrd"))
        let rules,start = generator.GenRulesList il
        let input = [|1us;2us|]
        let res = (new CYKCore()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy)
        let expected = "defined 1 0"
        Assert.AreEqual(expected,res)
        
    [<Test>]
    member test.twoLbls() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_twoLbls.yrd"))
        let rules,start = generator.GenRulesList il
        let input = [|1us;2us|]
        let res = (new CYKCore()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy)
        let expected = "defined 1 0"
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.twoLblsTwoDials() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_twoLblsTwoDial.yrd"))
        let rules,start = generator.GenRulesList il
        let code = generator.Generate(il)
        let input = [|1us;2us;2us|]
        let res = (new CYKCore()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy)
        let expected = "conflict 0 0"
        Assert.AreEqual(expected,res)

[<EntryPoint>]
let f _ =
    let tests = new CYKTests()
    let x1 = tests.noLbl()
    let x2 = tests.oneLbl()
    let x3 = tests.twoLbls()
    let x4 = tests.twoLblsTwoDials()
    0


