module CSharpCFGInfo

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open System.Collections.Generic

[<CustomEquality; CustomComparison>]
type ControlFlowElemWrapper = 
    | ControlFlowElemWrapper of IControlFlowElement

    member this.Value = match this with ControlFlowElemWrapper(cfe) -> cfe

    override this.Equals other = 
        let unwrap = fun (w: ControlFlowElemWrapper) -> w.Value
        let failFunc () = false
        Utils.applyToMappedTypedArgs (=) unwrap this other failFunc

    override this.GetHashCode() = hash this.Value

    interface System.IComparable with
        member this.CompareTo other =
            let getId = fun (w: ControlFlowElemWrapper) -> w.Value.Id
            let failFunc() = invalidArg "otherObj" "cannot compare values of different types"
            Utils.applyToMappedTypedArgs compare getId this other failFunc

type LoopNodeInfo = { 
    EnterEdgeIndex: int
    BodyExitEdgeIndex: int }

type CSharpCFGInfo = {
    AstCfgMap: Map<int, Set<ControlFlowElemWrapper>>
    LoopNodes: Map<int, LoopNodeInfo> }

let emptyCSharpCfgInfo = { 
    AstCfgMap = Map.empty 
    LoopNodes = Map.empty }