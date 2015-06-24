/// Loop info collecting functions specific for C#
module CsharpLoopInfo

open System.Collections.Generic

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open Utils
open GraphUtils
open IControlFlowGraphUtils
open ResharperCfgAdditionalInfo

/// Creates mapping from loop nodes to it's body enter and loop exit nodes 
/// for a given IControlFlowGraph
let findLoopConditionExits (cfg: IControlFlowGraph) (astNodeToCfeDict: AstToCfgDict) =
    let (|LoopTreeNode|_|) (node: ITreeNode) =
        match node with
        | :? IForStatement as forStmt -> Some(forStmt.Condition :> ITreeNode)
        | _ -> None
    let astConditionToPreLoopCfe =
        let loopChooser (node: ITreeNode) (cfeSet: HashSet<IControlFlowElement>) =
            match node with
            | LoopTreeNode(condition) ->
                cfeSet 
                |> Seq.sortBy (fun e -> e.Id) 
                |> Seq.head
                |> fun e -> Some(condition, e)
            | _ -> None
        astNodeToCfeDict 
        |> Seq.choose (fun (KeyValue(key, value)) -> loopChooser key value)
        |> Dictionary.dictFromSeq
    let astConditionToExits =
        let astConditionToCfeDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>()
        let processNode (e: IControlFlowElement) () =
            if e <> null && e.SourceElement <> null 
            && astConditionToPreLoopCfe.ContainsKey e.SourceElement
            then do Dictionary.addToSetInDict e.SourceElement e astConditionToCfeDict
        let algoParts = cfgExitsDfsParts processNode 
        do dfs algoParts cfg.EntryElement Set.empty () |> ignore
        let extractConditionExits (cfeSet: HashSet<IControlFlowElement>) =
            let setSize = cfeSet.Count
            if setSize < 2 then failwith "cond elems assumption failed"
            let condElems = 
                cfeSet 
                |> List.ofSeq 
                |> List.sortBy (fun e -> e.Id) 
                |> Seq.skip (setSize - 2)
            let bodyEnter = Seq.head <| condElems
            let loopExit = (Seq.head << Seq.skip 1) condElems
            (bodyEnter, loopExit)
        let astConditionToExitsDict = Dictionary()
        do astConditionToCfeDict
        |> Seq.iter 
            (fun (KeyValue(key, value)) -> 
                astConditionToExitsDict.[key] <- extractConditionExits value)
        astConditionToExitsDict
    let preLoopKeyToLoopKey (preLoopToExits: Dictionary<IControlFlowElement, 'Exits>) loopNodes =
        let rec findLoopNode (elem: IControlFlowElement) =
            if Set.contains elem.Id loopNodes
            then elem
            else findLoopNode (elem.Exits |> Seq.head |> (fun edge -> edge.Target))
        preLoopToExits
        |> Seq.map (fun (KeyValue(preLoopNode, exits)) -> findLoopNode preLoopNode, exits)
        |> Dictionary.dictFromSeq
    let preLoopToExits = Dictionary.mergeDicts astConditionToPreLoopCfe astConditionToExits
    let loopNodes = IControlFlowGraphUtils.findLoopNodes cfg
    preLoopKeyToLoopKey preLoopToExits loopNodes