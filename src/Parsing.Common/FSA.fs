namespace YC.Parsing.Common.FSA

open YC.Parsing.Common.FSA.Common
open YC.Core.IL

type FSA(ruleList : Rule<Source,Source> list) =
    let time = ref System.DateTime.Now
    let fsa =
        ruleList
        |> convertRulesToFSA
            //|> printDot @".\FSA1initialFSA.dot"
            |> (fun x -> //printfn "Convertion to fsa time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.WriteAllLines(@".\time.txt", [sprintf "Convertion to fsa time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> removeEpsilonEdges
            //|> printDot @".\FSA2withoutEpsilon.dot"
            |> (fun x -> //printfn "Epsilon edges removal time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Epsilon edges removal time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> toDFA              
            //|> printDot @".\FSA3determ.dot"
            |> (fun x -> //printfn "To dfa convetrion time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "To dfa convetrion time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)
        |> minimizeFSA
            //|> printDot @".\FSA4minimized.dot"
            |> (fun x -> //printfn "Minimization time: %A" (System.DateTime.Now - !time)
                        //System.IO.File.AppendAllLines(@".\time.txt", [sprintf "Minimization time: %A" (System.DateTime.Now - !time)])
                        time := System.DateTime.Now
                        x)

    member this.States = fsa.States
    //after convertion to dfa each startstates HashSet contains only one state
    member this.StartState = fsa.StartStates.[fsa.StartComponentNumber] |> Seq.find (fun _ -> true)
    member this.FinalStates = fsa.FinalStates
    member this.NontermCount = fsa.StartStates.Length
    member this.StateToNontermName = fsa.StateToNontermName
    member this.LastStates = fsa.LastStates
    member this.StartStates = fsa.StartStates
    member this.PrintDot filePrintPath = printDot filePrintPath fsa
    member this.RuleList = ruleList
    member this.Alphabet = fsa.Alphabet
