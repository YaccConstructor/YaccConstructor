open NUnit.Framework
open Yard.Generators.CYKGenerator
open Yard.Frontends.YardFrontend
open System.IO

[<TestFixture>]
type CYK () =
    let generator = new Yard.Generators.CYKGenerator.CYKGeneartorImpl()
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let basePath = "../../../../../Tests/CYK"

    [<Test>]
    member test.test1 () =
        let il = parser.ParseGrammar(Path.Combine(basePath, "simple_test_2.yrd"))
        let result = new ResizeArray<_>(generator.GenRulesList il |> Array.ofList)
        let code = generator.Generate(il)
        let input = [|1us;2us|]
        let res = (new CYKCore()).Recognize (result,1us) input (fun l1 l2 l3 -> 0uy)
        Assert.IsTrue(true)
        //let expected = "E->EE  conflict E->EE  l1 E->TX  l1 T->a   X->*   E->TX  l1 T->a   X->*   E->TY  l2 T->a   Y->+  "
        //let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")
        //let result = parser.Recognize testGrammar_lbl1 "a*a*a+" |> fun (s:string) -> s.Replace( "\n", " ")
        //Assert.AreEqual(expected, result)

[<EntryPoint>]
let f _ =
    let x = (new CYK ()).test1()
    0


