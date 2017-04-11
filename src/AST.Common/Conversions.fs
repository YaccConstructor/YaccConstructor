module Yard.Generators.Common.Conversions

let gllNodeToGlr (node: ASTGLLFSA.INode) rightToRule intToString termToIndex =
    let rec handleNode (node : ASTGLLFSA.INode) =
        match node with
        | :? ASTGLLFSA.EpsilonNode -> 
            new AstNode.Epsilon(-1) :> AstNode.AstNode |> Some
        | :? ASTGLLFSA.TerminalNode as term ->
            new AstNode.Terminal(termToIndex term) :> AstNode.AstNode |> Some
        | :? ASTGLLFSA.NonTerminalNode as nonTerm ->
            let children = collectChildren nonTerm.First
            let childToString (child : ASTGLLFSA.INode) =
                 match child with
                 | :? ASTGLLFSA.TerminalNode as term -> intToString <| int term.Name
                 | :? ASTGLLFSA.NonTerminalNode as nonTerm -> intToString <| int nonTerm.Name
            let childrenString = children |> Seq.map childToString |> String.concat " "
            if Seq.isEmpty children
            then
                None
            else
                let nodes = new AstNode.Nodes(children |> Seq.map handleNode |> Seq.choose id |> Array.ofSeq)
                let family = new AstNode.Family(rightToRule childrenString, nodes)
                new AstNode.AST (Array.singleton <| family) :> AstNode.AstNode |>Some 
        | other -> failwith <| sprintf "Can not handle %A node" other
    and collectChildren (node: ASTGLLFSA.INode) =
        match node with
        | :? ASTGLLFSA.PackedNode as packed -> 
            Seq.append <|| (collectChildren packed.Left, collectChildren packed.Right)                
        | :? ASTGLLFSA.IntermidiateNode as inter ->
            inter.MapChildren collectChildren|> Seq.concat        
        | :? ASTGLLFSA.TerminalNode as term ->
            if int term.Name = -1
            then Seq.empty
            else term :> ASTGLLFSA.INode |> Seq.singleton
        | :? ASTGLLFSA.EpsilonNode ->
            Seq.empty 
        | otherNode -> Seq.singleton otherNode

    match handleNode node with
        | Some converted -> converted
        | None           -> failwith "Start rule can not be anepsilon rule"

