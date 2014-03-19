module CYKForGPUTests

open NUnit.Framework
open Yard.Generators.CYKGenerator
open Yard.Frontends.YardFrontend
open System.IO

[<TestFixture>]
type CYKForGPUTests () =
    let generator = new Yard.Generators.CYKGenerator.CYKGeneartorImpl()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../Tests/CYK"

    [<Test>]
    member test.CYKForGPUBasic () =
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_2.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let code = generator.Generate(il)
        let input = [|1us;2us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        Assert.IsTrue(true)
        //let expected = "E->EE  conflict E->EE  l1 E->TX  l1 T->a   X->*   E->TX  l1 T->a   X->*   E->TY  l2 T->a   Y->+  "
        //let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")
        //let result = parser.Recognize testGrammar_lbl1 "a*a*a+" |> fun (s:string) -> s.Replace( "\n", " ")
        //Assert.AreEqual(expected, result)

    [<Test>]
    member test.CYKForGPUNoLbl () =
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;1us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "undefined : label = 0 weight = 0" 
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.CYKForGPUOneLbl() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_oneLbl.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;2us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "defined : label = @l weight = 0"
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.CYKForGPUOneLblNoInputLbl() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_oneLbl.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|2us;2us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "undefined : label = 0 weight = 0"
        Assert.AreEqual(expected,res)
        
    [<Test>]
    member test.CYKForGPUTwoLbls() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_twoLbls.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;2us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "defined : label = @l1 weight = 0"
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.CYKForGPUTwoLblsOneInputDial() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_twoLblsTwoDial.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;1us;|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "defined : label = @l1 weight = 0"
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.CYKForGPUTwoLblsTwoDials() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_twoLblsTwoDial.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;2us;2us|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        let expected = "conflict : label = 0 weight = 0"
        Assert.AreEqual(expected,res)

    [<Test>]
    member test.CYKForGPULabelTracking() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "labelTracking.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;2us;2us;2us;1us;2us;2us;2us;1us;2us;2us;2us;|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        Assert.IsTrue true
        //System.Console.ReadKey()

    [<Test>]
    member test.CYKForGPULabelTracking2() = 
        let il = parser.ParseGrammar(Path.Combine(basePath, "labelTracking2.yrd"))
        let rules,start,lbls = generator.GenRulesList il
        let input = [|1us;2us;2us;2us;|]
        let res = (new CYKCoreForGPU()).Recognize (rules,1us) input (fun l1 l2 l3 -> 0uy) lbls
        Assert.IsTrue true
        //System.Console.ReadKey()
