open System
open System.Collections.Generic
open System.Text

for n in [1..98] do
    let pathOld = sprintf "./grammars/BaselineDomain-%i.yrd" n
    let pathNew = sprintf "./grammarsNew/BaselineDomain-%i.yrd" n

    let lines =
        System.IO.File.ReadAllLines(pathOld).[1..]
        |> Array.map(fun x -> 
            let s = x.Split([|':'|], StringSplitOptions.RemoveEmptyEntries)
            let nonterm = s.[0].ToCharArray() |> Array.filter(fun x -> x <> ' ') |> (fun x -> new string(x))
            let disjuncts = 
                s.[1].Split([|'|'|])
                |> Array.map (fun disj -> 
                    disj.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
            nonterm, disjuncts)
    
    let newLines = 
        lines
        |> Array.map(fun (x, disj) ->        
            disj
            |> Array.collect( fun prod -> 
                let sq = ref [||]
                prod
                |> Array.map( fun symb -> 
                    let r = Array.append !sq [|symb|]
                    sq := r
                    r))
            |> (fun unrolled -> x, unrolled)
            )

    let s = new StringBuilder();
    s.Append("[<Start>]\n") |> ignore

    newLines
    |> Array.iter(fun (x, disj) ->
        s.Append(x + " : ") |> ignore
        disj
        |> Array.map( fun prod -> String.Join(" ", prod))
        |> (fun lines -> s.Append(String.Join(" | ", lines))|> ignore)
    
        s.Append("\n") |> ignore)

    System.IO.File.AppendAllText(pathNew,s.ToString())
printfn "finished"