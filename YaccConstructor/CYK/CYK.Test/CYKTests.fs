//open NUnit.Framework
//open CYK
//
//[<TestFixture>]
//type CYK () =
//    let parser = new CYKParser()
//    [<Test>]
//    member test.test1 () =
//        let expected = "E->EE  conflict E->EE  l1 E->TX  l1 T->a   X->*   E->TX  l1 T->a   X->*   E->TY  l2 T->a   Y->+  "
//        let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")
//        let result = parser.Recognize testGrammar_lbl1 "a*a*a+" |> fun (s:string) -> s.Replace( "\n", " ")
//        Assert.AreEqual(expected, result)
//
//[<EntryPoint>]
//let f _ =
//    let x = (new CYK ()).test1()
//    0


