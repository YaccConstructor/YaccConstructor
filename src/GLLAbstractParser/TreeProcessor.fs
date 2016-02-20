module YC.TreeProcessor

open Microsoft.FSharp.Collections

open FSharpx.Collections.Experimental

open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon

#nowarn "40"

type Message(node : INode) =
    member this.Node = node    
    
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

    let rec getAllTokens tree =
        //let isFirst = ref true
        match tree with
        | NonTerm (_,_,chlds) -> List.collect getAllTokens chlds
        | Term (s, n) -> 
//            if !isFirst
//            then
//                tokens.[n.Name] |> parser.TokenData |> printfn "%A"
//                isFirst := false
            [s]

    member this.printerAgent = MailboxProcessor<Message>.Start(fun inbox -> 
        let rec messageLoop = async{
            let! msg = inbox.Receive() 
            let nodeName = parser.NumToString (msg.Node :?> NonTerminalNode).Name           
            if nodeName = "folded"
            then 
                let p, tree = getMostSuitableTree msg.Node
                let str = getAllTokens tree
                if str.Length > 50
                then printfn "L = %A; prob: %A" str.Length p 
                ()
            return! messageLoop  
            }
        messageLoop 
        )

