module Yard.Generators.Common.AST.Test

open NUnit.Framework
open System
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open AbstractAnalysis.Common

type TokenType = NUM of int | PLUS of int | MULT of int | RNGLR_EOF of int
 
let directoryPath = "dot"
let path = sprintf "%s/%s" directoryPath "ast.dot"

let getEpsilon i = new Epsilon(-i-1)
let getTerminal i = new Terminal(i)
let getNodes arr = new Nodes(arr)
let getFamily prod nodes = new Family(prod, nodes)
let getAst family otherFamilies = new AST(family, otherFamilies)
let getTree<'T> tokens root rules = new Tree<'T>(tokens, root, rules)

let leftSide = [|2; 3; 0; 0; 1|]

let numToString = function
    | 0 -> "e"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "NUM"
    | 5 -> "PLUS"
    | 6 -> "RNGLR_EOF"
    | 7 -> "MULT"
    | _ -> ""

let epsilonTree = getTree<int> [||] (getEpsilon 0) [||]

let terminal0 = getTerminal 0
let terminal1 = getTerminal 1
let terminal2 = getTerminal 2
let terminal3 = getTerminal 3
let terminal4 = getTerminal 4
let terminal5 = getTerminal 5
let terminal6 = getTerminal 6
let terminal7 = getTerminal 7
let terminal8 = getTerminal 8

let tokens = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; PLUS 8; NUM 9;|]

let ast1 = getAst <| getFamily 3 (getNodes [|terminal0|]) <| null
let ast2 = getAst <| getFamily 2 (getNodes [|ast1; terminal1; terminal2|]) <| null
let ast3 = getAst 
           <| getFamily 2 (getNodes [|ast2; terminal3; terminal4|]) 
           <| [| getFamily 2 (getNodes [|ast2; terminal5; terminal6|]) |]
let ast4 = getAst <| getFamily 2 (getNodes [|ast3; terminal7; terminal8|]) <| null
let ast5 = getAst <| getFamily 0 (getNodes [|ast4|]) <| null
let ast6  = getAst <| getFamily 1 (getNodes [|ast5|]) <| null

let tree  = new Tree<TokenType>(tokens, ast6, [||], Some leftSide, Some numToString)

let tokens_1 = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; MULT 8; NUM 9;|]

let ast1_1 = getAst <| getFamily 3 (getNodes [|terminal0|]) <| null
let ast2_1 = getAst <| getFamily 2 (getNodes [|ast1_1; terminal1; terminal2|]) <| null
let ast3_1 = getAst 
           <| getFamily 2 (getNodes [|ast2_1; terminal3; terminal4|]) 
           <| [| getFamily 2 (getNodes [|ast2_1; terminal5; terminal6|]) |]
let ast4_1 = getAst <| getFamily 2 (getNodes [|ast3_1; terminal7; terminal8|]) <| null
let ast5_1 = getAst <| getFamily 0 (getNodes [|ast4_1|]) <| null
let ast6_1 = getAst <| getFamily 1 (getNodes [|ast5_1|]) <| null

let tree_1 = new Tree<TokenType>(tokens_1, ast6_1, [||], Some leftSide, Some numToString)


let tokens_2 = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; MULT 8; NUM 9;|]

let ast1_2 = getAst <| getFamily 3 (getNodes [|terminal0|]) <| null
let ast2_2 = getAst <| getFamily 4 (getNodes [|ast1_2; terminal1; terminal2|]) <| null
let ast3_2 = getAst <| getFamily 0 (getNodes [|ast1_2; terminal1; terminal2|]) <| null
let ast4_2 = getAst 
           <| getFamily 2 (getNodes [|ast2_2; terminal3; terminal4|]) 
           <| [| getFamily 2 (getNodes [|ast3_2; terminal5; terminal6|]) |]
let ast5_2 = getAst <| getFamily 2 (getNodes [|ast4_2; terminal7; terminal8|]) <| null
let ast6_2 = getAst <| getFamily 1 (getNodes [|ast5_2|]) <| null

let tree_2 = new Tree<TokenType>(tokens_2, ast6_2, [||], Some leftSide, Some numToString)

let tokens_3 = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; MULT 8; NUM 9;|]

let ast1_3 = getAst <| getFamily 2 (getNodes [|terminal0|]) <| null
let ast2_3 = getAst <| getFamily 4 (getNodes [|ast1_3; terminal1; terminal2|]) <| null
let ast3_3 = getAst <| getFamily 0 (getNodes [|ast1_3; terminal1; terminal2|]) <| null
let ast4_3 = getAst 
           <| getFamily 0 (getNodes [|ast2_3; terminal3; terminal4|]) 
           <| [| getFamily 0 (getNodes [|ast3_3; terminal5; terminal6|]) |]
let ast5_3 = getAst <| getFamily 0 (getNodes [|ast4_3; terminal7; terminal8|]) <| null
let ast6_3 = getAst <| getFamily 1 (getNodes [|ast5_3|]) <| null

let tree_3 = new Tree<TokenType>(tokens_3, ast6_3, [||], Some leftSide, Some numToString)

let tokens_4 = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; MULT 8; NUM 9;|]

let ast1_4 = getAst <| getFamily 0 (getNodes [|terminal0|]) <| null
let ast2_4 = getAst <| getFamily 4 (getNodes [|ast1_4; terminal1; terminal2|]) <| null
let ast3_4 = getAst <| getFamily 0 (getNodes [|ast1_4; terminal1; terminal2|]) <| null
let ast4_4 = getAst 
           <| getFamily 0 (getNodes [|ast2_4; terminal3; terminal4|]) 
           <| [| getFamily 0 (getNodes [|ast3_4; terminal5; terminal6|]) |]
let ast5_4 = getAst <| getFamily 0 (getNodes [|ast4_4; terminal7; terminal8|]) <| null
let ast6_4 = getAst <| getFamily 1 (getNodes [|ast5_4|]) <| null

ast1_4.other <- [| getFamily 0 (getNodes [|ast5_4|]) |]

let tree_4 = new Tree<TokenType>(tokens_4, ast6_4, [||], Some leftSide, Some numToString)

let tokens_5 = [|NUM 1; PLUS 2; NUM 3; PLUS 4; NUM 5; PLUS 6; NUM 7; MULT 8; NUM 9;|]

let ast1_5 = getAst <| getFamily 2 (getNodes [|terminal0|]) <| null
let ast1_2_5 = getAst <| getFamily 2 (getNodes [|terminal0|]) <| null
let ast2_5 = getAst <| getFamily 4 (getNodes [|ast1_5; ast1_2_5; terminal1; terminal2|]) <| null
let ast3_5 = getAst <| getFamily 0 (getNodes [|ast1_5; terminal1; terminal2|]) <| null
let ast4_5 = getAst 
           <| getFamily 0 (getNodes [|ast2_5; terminal3; terminal4|]) 
           <| [| getFamily 0 (getNodes [|ast3_5; terminal5; terminal6|]) |]
let ast5_5 = getAst <| getFamily 0 (getNodes [|terminal7; terminal8|]) <| null
let ast6_5 = getAst <| getFamily 0 (getNodes [|ast4_5; ast5_5; terminal7; terminal8|]) <| null
let ast7_5 = getAst <| getFamily 1 (getNodes [|ast6_5|]) <| null

let tree_5 = new Tree<TokenType>(tokens_5, ast7_5, [||], Some leftSide, Some numToString)

let tokenToNumber = function
    | NUM _ -> 4<token>
    | PLUS _ -> 5<token>
    | RNGLR_EOF _ -> 6<token>
    | MULT _ -> 7<token>

let tokenData = function
    | NUM x -> box x
    | PLUS x -> box x
    | MULT x -> box x
    | RNGLR_EOF x -> box x

[<TestFixture>]
type CommonAstTest () = 
    [<Test>]
    member this.FindNonterminalsByIndTest () =
        Assert.AreEqual(1, tree.FindNonterminalsByInd 3 |> Array.length)
        Assert.AreEqual(1, tree.FindNonterminalsByInd 2 |> Array.length)
        Assert.AreEqual(4, tree.FindNonterminalsByInd 0 |> Array.length)
        Assert.AreEqual(0, tree.FindNonterminalsByInd 10 |> Array.length)

    [<Test>]
    member this.FindNonterminalsByStringTest () =
        Assert.AreEqual(1, tree.FindNonterminalsByString "yard_start_rule" |> Array.length)
        Assert.AreEqual(1, tree.FindNonterminalsByString "s" |> Array.length)
        Assert.AreEqual(4, tree.FindNonterminalsByString "e" |> Array.length)
        Assert.AreEqual(0, tree.FindNonterminalsByString "nothing" |> Array.length)

    [<Test>]
    member this.GetTypeOfExpressionTest () =
        let typeOfExpr = tree.GetTypeOfExpression [|"s"; "e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "s"))


    [<Test>]
    member this.GetTypeOfExpressionTest2 () =
        let typeOfExpr = tree.GetTypeOfExpression [|"e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "e"))
        

    [<Test>]
    member this.GetTypeOfExpressionTest_1 () =
        let typeOfExpr = tree_1.GetTypeOfExpression [|"s"; "e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "s"))


    [<Test>]
    member this.GetTypeOfExpressionTest2_1 () =
        let typeOfExpr = tree_1.GetTypeOfExpression [|"e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "e"))

    [<Test>]
    member this.GetTypeOfExpressionTest_2 () =
        let typeOfExpr = tree_2.GetTypeOfExpression [|"s"; "error"|]
        Assert.IsTrue(typeOfExpr <> null 
                      && typeOfExpr.Length = 2 
                      && typeOfExpr |> Array.exists (fun x -> String.Equals("s", x))
                      && typeOfExpr |> Array.exists (fun x -> String.Equals("error", x)))

    [<Test>]
    member this.GetTypeOfExpressionTest2_2 () =
        let typeOfExpr = tree_2.GetTypeOfExpression [|"e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "e"))
        
    [<Test>]
    member this.GetTypeOfExpressionTest_3 () =
        let typeOfExpr = tree_3.GetTypeOfExpression [|"e"; "error"|]
        Assert.IsTrue(typeOfExpr <> null 
                      && typeOfExpr.Length = 2 
                      && typeOfExpr |> Array.exists (fun x -> String.Equals("e", x))
                      && typeOfExpr |> Array.exists (fun x -> String.Equals("error", x)))

    [<Test>]
    member this.GetTypeOfExpressionTest2_3 () =
        let typeOfExpr = tree_3.GetTypeOfExpression [|"e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 1 && String.Equals(typeOfExpr.[0], "e"))

    [<Test>]
    member this.CalculateStatisticsTest () =
        let (max, min, average) = tree_3.CalculateStatistics "e" 
        Assert.AreEqual(1, max)
        Assert.AreEqual(1, min)
        Assert.AreEqual(1.0, average)

    [<Test>]
    member this.CalculateStatisticsTest_1 () =
        let (max, min, average) = tree_3.CalculateStatistics "s" 
        Assert.AreEqual(3, max)
        Assert.AreEqual(2, min)
        Assert.AreEqual(2.5, average)

    [<Test>]
    member this.CalculateStatisticsTest_2 () =
        let (max, min, average) = tree_3.CalculateStatistics "error" 
        Assert.AreEqual(1, max)
        Assert.AreEqual(0, min)
        Assert.AreEqual(0.5, average)

    [<Test>]
    member this.CalculateStatisticsTest_3 () =
        let (max, min, average) = tree_3.CalculateStatistics "yard_start_rule"
        Assert.AreEqual(1, max)
        Assert.AreEqual(1, min)
        Assert.AreEqual(1.0, average)

    [<Test>]
    member this.CalculateStatisticsTest_4 () =
        let (max, min, average) = tree_2.CalculateStatistics "e"
        Assert.AreEqual(3, max)
        Assert.AreEqual(3, min)
        Assert.AreEqual(3.0, average)

    [<Test>]
    member this.CalculateStatisticsTest_5 () =
        let (max, min, average) = tree_2.CalculateStatistics "s"
        Assert.AreEqual(1, max)
        Assert.AreEqual(0, min)
        Assert.AreEqual(0.5, average)


    [<Test>]
    member this.CalculateStatisticsTest_6 () =
        let (max, min, average) = tree_5.CalculateStatistics "e"
        Assert.AreEqual(2, max)
        Assert.AreEqual(1, min)
        Assert.AreEqual(1.5, average)

    [<Test>]
    member this.CalculateStatisticsTest_7 () =
        let (max, min, average) = tree_5.CalculateStatistics "s"
        Assert.AreEqual(4, max)
        Assert.AreEqual(3, min)
        Assert.AreEqual(3.5, average)

    [<Test>]
    member this.GetTypeOfExpressionTest_4 () =
        let typeOfExpr = tree_4.GetTypeOfExpression [|"e"; "error"|]
        Assert.IsTrue(typeOfExpr <> null 
                      && typeOfExpr.Length = 1
                      && String.Equals("error", typeOfExpr.[0]))

    [<Test>]
    member this.GetTypeOfExpressionTest2_4 () =
        let typeOfExpr = tree_4.GetTypeOfExpression [|"e"|]
        Assert.IsTrue(typeOfExpr <> null && typeOfExpr.Length = 0)
        

//[<EntryPoint>]
let f x =
    if not <| System.IO.Directory.Exists directoryPath
    then 
        (*System.IO.Directory.GetFiles directoryPath |> Seq.iter System.IO.File.Delete
    else*) System.IO.Directory.CreateDirectory directoryPath |> ignore
    let t = new CommonAstTest () 

    tree_5.AstToDot numToString tokenToNumber (Some tokenData) leftSide path

    t.FindNonterminalsByIndTest ()
    t.FindNonterminalsByStringTest ()
    t.GetTypeOfExpressionTest ()
    t.GetTypeOfExpressionTest2 ()
    

    t.GetTypeOfExpressionTest_1 ()
    t.GetTypeOfExpressionTest2_1 ()

    t.GetTypeOfExpressionTest_2 ()
    t.GetTypeOfExpressionTest2_2 ()

    t.GetTypeOfExpressionTest_3 ()
    t.GetTypeOfExpressionTest2_3 ()
    
    t.GetTypeOfExpressionTest_4 ()
    t.GetTypeOfExpressionTest2_4 ()

    t.CalculateStatisticsTest ()
    t.CalculateStatisticsTest_1 ()
    t.CalculateStatisticsTest_2 ()
    t.CalculateStatisticsTest_3 ()
    t.CalculateStatisticsTest_4 ()
    t.CalculateStatisticsTest_5 ()
    t.CalculateStatisticsTest_6 ()
    t.CalculateStatisticsTest_7 ()
    0

