module Yard.CallIdentification

open System.Collections
open Yard.Core.IL
open Yard.Core.IL.Production
open QuickGraph

type RuleDecl<'a,'b> =
    | Decl of Rule.t<'a,'b>
    | NotDecl of string

let createGraph(def : Definition.t<_,_>) = 
    let graph = new AdjacencyGraph<RuleDecl<_,_>, Edge<RuleDecl<_,_>>>()

    def.grammar |> List.iter (fun arg -> graph.AddVertex (Decl(arg)) |> ignore )

    let connect (left, (right,_)) = 
        let rightName = right.ToString()
        try     
            graph.AddEdge( new Edge<RuleDecl<_,_>>( left, graph.Vertices |> Seq.find (fun r -> match r with
                                                                                               | Decl r -> r.name = rightName
                                                                                               | NotDecl s -> s = rightName
                                                                                       ) ) ) |> ignore 
        with
        | KeyNotFoundException ->
            graph.AddVertex( NotDecl(rightName) ) |> ignore 
            graph.AddEdge( new Edge<RuleDecl<_,_>>( left, NotDecl(rightName) ) ) |> ignore 

    let rec search arg body = 
        let searchCurried body = search arg body
        match body with
        | PRef (name,_) -> connect(arg, name)
        | PMetaRef (name,_,exprList) ->  
            connect(arg, name)
            exprList |> List.iter searchCurried 
        | PSeq (exprList,_) -> exprList |> List.iter (fun r -> searchCurried r.rule)
        | PPerm exprList    -> exprList |> List.iter searchCurried
        | PRepet (expr,_,_)
        | PMany expr
        | PSome expr
        | POpt  expr -> searchCurried expr
        | PAlt (lExpr,rExpr) -> 
            searchCurried lExpr
            searchCurried rExpr
        | PLiteral _ 
        | PToken _  -> ()

    def.grammar |> List.iter (fun arg -> search (Decl(arg)) arg.body)
    
    graph


let IsUndeclaratedRulesExists(def : Definition.t<_,_>) =
  (createGraph def).Vertices |> Seq.exists (fun r -> match r with
                                                     | Decl r -> false
                                                     | NotDecl s -> true
                                            )