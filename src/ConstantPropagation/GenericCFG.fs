module GenericCFG

open QuickGraph

type OperationType =
| Replace
| Concat
| Arbitrary of string
// and so on ...

type UpdaterType =
| Assign
| PlusAssign
// function with updatable arg passing

type CFGNodeType = 
| Declaration of string
| Updater of string * UpdaterType
| Operation of OperationType * int
| Literal of string
| VarRef of string
| LoopNode of int * int

type CFGNode = {
    Id: int
    IsHotspot: bool
    Type: CFGNodeType
}

module CFGNodeFuncs =
    let toString (node: CFGNode) =
        match node.Type with
        | Declaration(name) -> sprintf "decl(%s)" name
        | Updater(target, aType) -> 
            let typeStr =
                match aType with
                | Assign -> "assign"
                | PlusAssign -> "plusAssign"
            sprintf "%s(%s)" typeStr target
        | Operation(oType, operands) -> 
            match oType with
            | Replace -> "replace"
            | Concat -> "concat"
            | Arbitrary(name) -> sprintf "%s(%d)" name operands
        | Literal(value) -> sprintf "literal(%s)" value
        | VarRef(name) -> sprintf "varRef(%s)" name
        | LoopNode(_,_) -> "loopNode"

type GenericCFG = {
    Graph: BidirectionalGraph<CFGNode, Edge<CFGNode>>
}