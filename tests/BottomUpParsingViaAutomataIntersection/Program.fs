module BottomUpParsingViaAutomataIntersection.Test

open NUnit.Framework
open YC.Parsers.BottomUp
open System.Collections.Generic

[<TestFixture>]
type ``Tests for bottom-Up parser based on automata intersection``() =
    
    // S -> a S b | eps
    // 1 a
    // 2 b
    // 3 S
    let grammar1 = 
        let g = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        g.[0,1].Add 1 |> ignore
        g.[1,2].Add 3 |> ignore
        g.[2,3].Add 2 |> ignore
        let states = [|(3, new HashSet<_>([0]), new HashSet<_>([0;3]))|]
        new Grammar (g, new ResizeArray<_>(states))

    // E -> E (+|*E) | n
    // 1 +
    // 2 *
    // 3 E
    // 4 n
    let grammar2 = 
        let g = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        g.[0,1].Add 3 |> ignore
        g.[1,2].Add 1 |> ignore
        g.[1,2].Add 2 |> ignore
        g.[2,3].Add 3 |> ignore
        g.[0,3].Add 4 |> ignore
        let states = [|(3, new HashSet<_>([0]), new HashSet<_>([3]))|]
        new Grammar (g, new ResizeArray<_>(states))    

    [<Test>]
    member this._01_BracketsLinear() = 
        let input = Array2D.init 5 5 (fun i j -> new HashSet<_>())
        input.[0,1].Add 1 |> ignore
        input.[1,2].Add 1 |> ignore
        input.[2,3].Add 2 |> ignore
        input.[3,4].Add 2 |> ignore
        let output = main input grammar1
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,0].Contains 3)
        Assert.IsTrue(output.[1,1].Contains 3)
        Assert.IsTrue(output.[2,2].Contains 3)
        Assert.IsTrue(output.[3,3].Contains 3)
        Assert.IsTrue(output.[4,4].Contains 3)
        Assert.IsTrue(output.[0,4].Contains 3)

    [<Test>]
    member this._02_BracketsCycle() = 
        let input = Array2D.init 2 2 (fun i j -> new HashSet<_>())
        input.[0,0].Add 1 |> ignore
        input.[0,1].Add 2 |> ignore
        input.[1,0].Add 2 |> ignore 
        let output = main input grammar1
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,0].Contains 3)
        Assert.IsTrue(output.[1,1].Contains 3)
        Assert.IsTrue(output.[0,1].Contains 3)
        Assert.IsFalse(output.[1,0].Contains 3)

    [<Test>]
    member this._03_ArithGraph() = 
        let input = Array2D.init 4 4 (fun i j -> new HashSet<_>())
        input.[0,1].Add 4 |> ignore
        input.[1,2].Add 1 |> ignore
        input.[2,3].Add 4 |> ignore
        input.[3,0].Add 2 |> ignore 
        let output = main input grammar2
        printfn "_______________"
        printfn "%A" output
        Assert.IsTrue(output.[0,1].Contains 3)
        Assert.IsTrue(output.[2,3].Contains 3)
        Assert.IsTrue(output.[0,3].Contains 3)
        Assert.IsTrue(output.[2,1].Contains 3)
        let startNonTermCount = 
            let i = ref 0
            output |> Array2D.iter (fun s -> if s.Contains 3 then incr i)
            !i
        Assert.AreEqual(4, startNonTermCount)

        
            
