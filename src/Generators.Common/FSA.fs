namespace Yard.Generators.Common.FSA

open System.Collections.Generic

open Yard.Generators.Common.FSA.Common

open Yard.Core.IL
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

type FSA(ruleList : Rule<Source,Source> list) =
    let time = ref System.DateTime.UtcNow
    let fsa =
        ruleList
        |> convertRulesToFSA
            //|> printDot @".\FSA1initialFSA.dot"
            |> (fun x -> //printfn "Convertion to fsa time: %A" (System.DateTime.UtcNow - !time)
                        //System.IO.File.WriteAllLines(@".\time.txt", [sprintf "Convertion to fsa time: %A" (System.DateTime.UtcNow - !time)])
                        time := System.DateTime.UtcNow
                        x)
        |> removeEpsilonEdges
            //|> printDot @".\FSA2withoutEpsilon.dot"
            |> (fun x -> //printfn "Epsilon edges removal time: %A" (System.DateTime.UtcNow - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Epsilon edges removal time: %A" (System.DateTime.UtcNow - !time)])
                        time := System.DateTime.UtcNow
                        x)
        |> toDFA              
            //|> printDot @".\FSA3determ.dot"
            |> (fun x -> //printfn "To dfa convetrion time: %A" (System.DateTime.UtcNow - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "To dfa convetrion time: %A" (System.DateTime.UtcNow - !time)])
                        time := System.DateTime.UtcNow
                        x)
        |> minimizeFSA
            //|> printDot @".\FSA4minimized.dot"
            |> (fun x -> //printfn "Minimization time: %A" (System.DateTime.UtcNow - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Minimization time: %A" (System.DateTime.UtcNow - !time)])
                        time := System.DateTime.UtcNow
                        x)

    member this.States = fsa.States
    //after convertion to dfa each startstates HashSet contains only one state
    member this.StartState = fsa.StartStates.[fsa.StartComponentNumber] |> Seq.find (fun x -> true)
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    member this.StateToNontermName = fsa.StateToNontermName
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    member this.RuleList = ruleList
