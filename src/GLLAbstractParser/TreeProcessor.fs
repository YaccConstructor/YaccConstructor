module YC.TreeProcessor

open Microsoft.FSharp.Collections

open FSharpx.Collections.Experimental

open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

#nowarn "40"

type Message =
    | NodeToProcess of INode
    | End
    
type TreeProcessor<'TokenType> (parser : ParserSourceGLL<'TokenType>, tokens:BlockResizeArray<'TokenType>) =

    let rec getMostSuitableTree (node:INode) =     
        let visited = new System.Collections.Generic.Dictionary<_,_>()

        let rec go prob (node:INode) = 
            let subtreeSelector first others =
                let r1 = go prob first 
                let r2 = if others <> null then others |> ResizeArray.map (go prob) |> ResizeArray.toList else []                                
                (r1::r2) |> List.maxBy fst                                    
        
            match node with
            | :? NonTerminalNode as n ->
                let p,t = subtreeSelector n.First n.Others
                p, [NonTerm (parser.NumToString n.Name, n, t)]
            | :? IntermidiateNode as n ->
                subtreeSelector n.First n.Others
            | :? PackedNode as n ->
                let f,r = visited.TryGetValue n 
                if f 
                then r 
                else
                    let p1,t1 = go prob n.Left
                    let p2,t2 = go prob n.Right
                    let r = parser.Probabilities.[n.Production] * p1 * p2
                    let t = t1 @ t2
                    visited.Add(n,(r,t))
                    r,t
            | :? TerminalNode as n ->
                prob
                , if n.Name <> -1 
                  then [Term(parser.TokenToNumber tokens.[n.Name] |> parser.NumToString, n)]
                  else []            
            | _ -> prob, []

        let p, t = go 1.0 node
        p, List.head t

    let getAllTokens tree =
        //let isFirst = ref true
        let s = ref 0
        let rec go tree =
            match tree with
            | NonTerm (n,_,chlds) -> 
                if n.Contains "stem_28" then incr s
                List.collect go chlds
            | Term (s, n) -> 
                //tokens.[n.Name] |> parser.TokenData |> printfn "%A"
    //            if !isFirst
    //            then
    //                tokens.[n.Name] |> parser.TokenData |> printfn "%A"
    //                isFirst := false
                [(s,tokens.[n.Name] |> parser.TokenData)]
        go tree, !s

    member this.printerAgent onEnd = MailboxProcessor<Message>.Start(fun inbox ->
        let curLeft = ref 0
        let curRight = ref 0 
        let curP = ref 0.0
        let ranges = new ResizeArray<_>()
        let rec messageLoop = async{
            let! msg = inbox.Receive()
            match msg with
            | NodeToProcess n ->
                let nodeName = parser.NumToString (n :?> NonTerminalNode).Name           
                //printfn "name=%A" nodeName
                if nodeName = "folded"
                then 
                    let p, tree = getMostSuitableTree n
                    let str,nt = getAllTokens tree
                    //printfn "LL=%A" str.Length
                    if str.Length > 60 //&& nt > 5// && nt < 9
                    then 
                        let left = snd str.Head |> string |> int
                        let right = List.rev str |> List.head |> snd |> string |> int
                        printfn "L = %A; prob: %A; f %A t %A" str.Length p left right
                        if !curRight < left
                        then 
                            ranges.Add (!curLeft,!curRight, !curP)
                            curLeft := left
                            curRight := right
                            curP := p
                        else
                            curLeft := min !curLeft left
                            curRight := max !curRight right
                            curP := max !curP p
                    ()
                return! messageLoop  
            | End -> 
                ranges.[0] <- (!curLeft,!curRight, !curP)
                ranges.ToArray()
                |> Array.sortBy (fun(_,_,x) -> -1.0 * x)
                |> onEnd
            }
        messageLoop 
        )

