/// Loop info collecting functions specific for JavaScript
module JsLoopInfo

open System.Collections.Generic

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open ResharperCfgAdditionalInfo
open Utils

/// Creates mapping from loop nodes to it's body enter and loop exit nodes 
/// for a given IControlFlowGraph
let findLoopConditionExits (cfg: IControlFlowGraph) (astNodeToCfeDict: AstToCfgDict) =
    let jsCfgLoopsAssumptionMsg = "Assumption about Js cfg structure for loops failed"
    let loopConditionExits =
        let loopChooser (node: ITreeNode) (cfeSet: HashSet<IControlFlowElement>) =
            match node with
            | :? IForStatement ->
                cfeSet
                |> Seq.head
                |> fun e -> Some(e)
            | _ -> None
        /// Returns pair (loopBodyBeg, loopExit)
        let getExits (forStmtCfe: IControlFlowElement) =
            if forStmtCfe.Entries.Count <> 1
            then failwith jsCfgLoopsAssumptionMsg
            else 
                let parentCfe = forStmtCfe.Entries |> Seq.exactlyOne |> fun r -> r.Source
                if parentCfe.Exits.Count <> 2
                then failwith jsCfgLoopsAssumptionMsg
                else 
                    let exits = parentCfe.Exits |> Seq.map (fun r -> r.Target)
                    let exit1, exit2 = exits |> Seq.head, exits |> Seq.skip 1 |> Seq.exactlyOne
                    if exit1 = forStmtCfe
                    then exit2, forStmtCfe
                    else exit1, forStmtCfe
        astNodeToCfeDict 
        |> Seq.choose (fun (KeyValue(key, value)) -> loopChooser key value)
        |> Seq.map (fun cfe -> getExits cfe)
    let loopNodes = IControlFlowGraphUtils.findLoopNodes cfg
    let loopNodeToExits =
        let rec findLoopNode (elem: IControlFlowElement) =
            if Set.contains elem.Id loopNodes
            then elem
            else findLoopNode (elem.Entries |> Seq.head |> (fun edge -> edge.Source))
        loopConditionExits
        |> Seq.map (fun ((loopBodyBeg, loopExit) as p)-> findLoopNode loopBodyBeg, p)
        |> Dictionary.dictFromSeq
    loopNodeToExits