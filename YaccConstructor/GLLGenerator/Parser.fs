// Parser.fs contains base logic for table-based GLL parsing

module Yard.Generators.GLL.Parser
open Yard.Generators.GLL.AST

type Production = GrammarItem list

// startNonTerm: int that represents start Ntrm (yard_start_rule)
// eofToken: what to expect as the last token of input
// actionsTable: (lookahead terminal, nonterminal on parse stack topm, production(s) to use)
// productions: right sides of grammar rules
// tokens: input stream of grammar terminals (their int codes)
type ParserBase (startNonTerm, eofToken, actionsTable, productions : Production[], tokens : int[]) =
    // represents actions table: (lookahead terminal, nonterminal on parse stack top) -> production(s) to use
    let actions : int * int -> int list option =  (Map.ofArray actionsTable).TryFind

    member this.parse () =
        // expands the nonterminals following specified node in right-to-left traversals
        // until a terminal matching current input terminal is found in every traversal
        let rec expandFromNode currentInputPos currentInputTerm (currentSet : NodeWithHistory list) (nodeH : NodeWithHistory) =
            let node = nodeH.Node
            match node.Item with
            // found tree node matching current input terminal: add its next to found set
            | Trm trm when trm = currentInputTerm ->
                node.setItemPos currentInputPos
                let newTraversals = ResizeArray<Node list> (nodeH.Traversals.Count)
                for i in 0..nodeH.Traversals.Count-1 do
                    newTraversals.Add (node :: nodeH.Traversals.[i])
                let nextNodeH = NodeWithHistory(node.NextNode.Value, newTraversals)
                nextNodeH :: currentSet
            // found tree node not matching current input terminal
            | Trm trm ->
                currentSet
            | Ntrm ntrm ->
                match actions (currentInputTerm, ntrm) with
                // found nonterminal; expand all its available productions
                | Some productionIndices ->
                    let nextNode = node.NextNode.Value
                    let expandProduction productionIndex =
                        let production = productions.[productionIndex]
                        let newNextNode =
                            List.foldBack (fun (item, itemIndex) next -> Node (item, next, (productionIndex, itemIndex, node)))
                                          (List.zip production [0..production.Length-1])
                                          nextNode
                        NodeWithHistory(newNextNode, nodeH.Traversals)
                    let newNextNodesH = List.map expandProduction productionIndices
                    List.fold (expandFromNode currentInputPos currentInputTerm) currentSet newNextNodesH
                // found nonterminal but no productions
                | None ->
                    currentSet
        let mergeCurrentTreeNodes nodes =
            nodes |> Seq.distinct |> List.ofSeq

        // prepare initial structure
        let fakeEndNode = Node (Trm eofToken, None)
        let eofNode = Node (Trm eofToken, Some fakeEndNode)
        let rootNode = Node (Ntrm startNonTerm, Some eofNode)
        let rootNodeH = NodeWithHistory (rootNode, ResizeArray<Node list> ())
        rootNodeH.Traversals.Add ([])
        
        // tree leafs to match with current terminal in the input buffer
        let mutable currentTreeNodes = [rootNodeH]
        let mutable pos = 0

        while currentTreeNodes.Length > 0 && pos < tokens.Length do
            currentTreeNodes <-
                List.fold (expandFromNode pos tokens.[pos]) [] currentTreeNodes
                |> mergeCurrentTreeNodes
            pos <- pos + 1

        pos = tokens.Length && List.exists (fun (nodeH:NodeWithHistory) -> nodeH.Node = fakeEndNode) currentTreeNodes
