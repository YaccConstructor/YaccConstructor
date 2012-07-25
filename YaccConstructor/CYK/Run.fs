module Run

open Yard.Generators.CYKGenerator

//грамматика, допускающая операции +, * и позволяющая группировать их с помощью скобок, преобразованная в НФХ
//let testGrammar1 = (Array.ofList <| (List.map ToBranch [("E","T","N1");("N1","N2","E");("E","F","N3");("N3","N4","T");("E","N5","N6");("N6","E","N7");("E1","N2","E");("T","F","N3");("T","N5","N6");("T1","N4","T");("F","N5","N6")]) @ (List.map ToLeaf [("N2",'+');("N4",'*');("E",'a');("N5",'(');("N7",')');("T",'a');("F",'a')]),"E")
//let testGrammar2 = ((List.map ToBranch [("S","A","B");("B","N1","N2");("N1","D1","D2")])@(List.map ToLeaf [("A",'a');("D2",'c');("B",'a');("D1",'b');("N2",'d')]),"S")
//System.Console.WriteLine("Test1:")
//recognize testGrammar1 "a*a+a"

//let testGrammar_lbl1 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"));("E","T","Y",Some("l2"));("E","E","E",None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Y",'+',None)]),"E")

let testGrammar_lbl2 = (Array.ofList <| (List.map ToBranch [("E","T","X",Some("l1"),Some(1));("E","T","Z",Some("l1"),Some(1));("E","T","Y",Some("l2"),Some(9));("E","E","E",None,None)]) @ (List.map ToLeaf [("T",'a',None);("X",'*',None);("Z",'*',None);("Y",'+',None)]),"E")

let str = List.init 20 (fun i -> "(a+a)*a") |> String.concat "+"
System.Console.WriteLine("Test2:")
let start = System.DateTime.Now
//recognize testGrammar1 str
printfn "Time = %A" (System.DateTime.Now - start)

(new CYKParser()).Recognize testGrammar_lbl2 "a*a+" //|> printfn "%s"

//System.Console.WriteLine("Test3:")
//recognize testGrammar2 "abcd"
System.Console.ReadLine() |> ignore
