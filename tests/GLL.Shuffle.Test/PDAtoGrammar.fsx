open System
open System.Collections.Generic
open System.Text


//let pathOld = sprintf "./grammars/BaselineDomain-%i.yrd" n
//let pathNew = sprintf "./grammarsNew/BaselineDomain-%i.yrd" n

let lines = System.IO.File.ReadAllLines("./pda.txt")

let states = lines.[0].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let stackSymb = lines.[1].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let alph = lines.[2].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)


let convTrans p a X gam r0 = 
    states
    |> Array.map(fun st -> 
        let head = "r" + p + X + st
        let tail = 
            states
            |> Array.map(fun stTo -> 
                a + " " + "r" + p + gam + stTo
            )
        head, head  + " : "+ (String.Join(" | ", [String.Join("|", tail); a]))
        )

let res = 
    lines.[3..]
    |> Array.filter(fun x -> x.Length > 1)
    |> Array.collect(fun x -> 
        let s = x.Split([|"="|], StringSplitOptions.RemoveEmptyEntries)
        let part1 = s.[0].Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        let p = part1.[0]
        let a = part1.[1]
        let X = part1.[2]
        let part2 = s.[1].Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        let r0 = part2.[0]
        let gam = part2.[1]
        convTrans p a X gam r0)

let sProd = 
    res
    |> Array.map(fun (head,_) -> 
        head)
    |> (fun x -> "[<Start>]\ns : " + String.Join("|", x) + "\n")

let prods = 
    res
    |> Array.map snd

let result = sProd + String.Join("\n", prods)

System.IO.File.AppendAllText("./gammarFromPda.yrd",result)
printfn "finished"