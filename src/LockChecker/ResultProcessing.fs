module ResultProcessing
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Frontends.YardFrontend
open YC.API
open AbstractParser
open System.Collections.Generic

let singlePathForRoot (root: INode) (intToString : Dictionary<_,_>) : seq<string> =
    let results = new Dictionary<INode, _>() 
    let rec getPath : INode -> seq<string> = function
        | :? IntermidiateNode as i ->
            let isGot,value = results.TryGetValue i
            if isGot
            then 
                Seq.empty
            else
                results.Add(i, null)
                getPath i.First
        | :? TerminalNode as t ->
            let res = new List<_>()
            if t.Name <> -1<token> 
            then 
                seq{yield (sprintf "%s %i %i" intToString.[int t.Name] (getLeftExtension t.Extension) (getRightExtension t.Extension))}
            else
                Seq.empty
        | :? PackedNode as p ->
            let rightPath = getPath p.Right
            let leftPath = getPath p.Left
            Seq.append leftPath rightPath
        | :? NonTerminalNode as n ->
            let isGot,value = results.TryGetValue n
            if isGot
            then 
                Seq.empty
            else
                results.Add(n, null)
                getPath n.First
        | :? EpsilonNode as eps ->
            Seq.empty
        | _ -> failwith "Unexpected node type. rly?"

    getPath root
(*
let allPathsForRoot (root: INode) (intToString : Dictionary<_,_>) : List<List<string>> =
    let results = new Dictionary<INode, List<_>>() 
    let rec getPaths : INode -> List<List<string>> * bool = function
        | :? IntermidiateNode as i -> 
            let isGot,value = results.TryGetValue i
            if isGot
            then
                value, true
            else
                let withHole = new List<_>()
                let withoutHole = new List<_>()
                results.Add(i, withoutHole)
                let f, isHoled = getPaths i.First
                if isHoled
                then
                    withHole.AddRange f
                else
                    withoutHole.AddRange f
                if (i.Others <> null)
                then
                    for o in i.Others do
                        let oth, isHoled = getPaths o
                        if isHoled
                        then
                            withHole.AddRange oth
                        else
                            withoutHole.AddRange oth
                
                
                f
        | :? TerminalNode as t ->
            let res = new List<List<_>>()
            if t.Name <> -1<token> 
            then
                let l = new List<_>()
                l.Add(sprintf "%s %i %i"
                         intToString.[int t.Name]
                         (getLeftExtension t.Extension)
                         (getRightExtension t.Extension)
                        )
                res.Add(l)
            res, false
        | :? PackedNode as p ->
            let rightPaths, rightIsHoled = getPaths p.Right
            let leftPaths, leftIsHoled = getPaths p.Left
            let result = new List<List<_>>()
            if (rightPaths.Count > 0)
            then
                if (leftPaths.Count > 0)
                then
                    for rp in rightPaths do
                        for lp in leftPaths do
                            let l = new List<_>()
                            l.AddRange lp
                            l.AddRange rp
                            result.Add(l)
                    result
                else
                    rightPaths
            else
                leftPaths
        | :? NonTerminalNode as n ->
            let isGot,value = results.TryGetValue n
            if isGot
            then 
                if (value = null)
                then
                    new List<_>()
                else
                    value
            else
                results.Add(n, null)
                let f = getPaths n.First
                if (n.Others <> null)
                then
                    for o in n.Others do
                        f.AddRange (getPaths o)
                results.Remove(n) |> ignore
                results.Add(n, f)
                f
            
        | :? EpsilonNode as eps ->
            new List<List<_>>()
        | _ -> failwith "Unexpected node type. rly?"

    getPaths root
*)
let getBadAsserts (root : INode) (intToString : Dictionary<_,string>) : string[] = 
    let results = new HashSet<string>()
    let isVisided = new HashSet<INode>()
    let isBad = ref false
    let rec getBadAssertsForNode : INode -> Unit = function
        | :? IntermidiateNode as i -> 
            if isVisided.Contains i |> not
            then
                isVisided.Add i |> ignore
                getBadAssertsForNode i.First
                if (i.Others <> null)
                then
                    for o in i.Others do
                        getBadAssertsForNode o
        | :? TerminalNode as t ->
            if t.Name <> -1<token> 
            then
                let name = intToString.[int t.Name]
                if (!isBad && name.StartsWith "A")
                then
                    results.Add name |> ignore
        | :? PackedNode as p ->
            getBadAssertsForNode p.Right
            getBadAssertsForNode p.Left
        | :? NonTerminalNode as n ->
            if isVisided.Contains n |> not
            then 
                isVisided.Add n |> ignore
                let name = intToString.[int n.Name]
                if name = "ba"
                then
                    isBad := true
                getBadAssertsForNode n.First
                if (n.Others <> null)
                then
                    for o in n.Others do
                        getBadAssertsForNode o
                isBad := false
        | :? EpsilonNode as eps -> ()
        | _ -> failwith "Unexpected node type. rly?"

    getBadAssertsForNode root
    results |> Array.ofSeq