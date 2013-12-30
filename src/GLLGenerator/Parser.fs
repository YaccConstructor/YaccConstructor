// Parser.fs contains base logic for table-based GLL parsing

module Yard.Generators.GLL.Parser
open Yard.Generators.GLL.AST

type Production = GrammarItem list

// startNonTerm: int that represents start Ntrm (yard_start_rule)
// eofToken: what to expect as the last token of input
// actionsTable: (lookahead terminal, nonterminal on parse stack topm, production(s) to use)
// productions: right sides of grammar rules
// tokens: input stream of grammar terminals (their int codes)
type ParserBase (startNonTerm, eofToken, actionsTable, productions : Production[], tokens : int seq) =
    // represents actions table: (lookahead terminal, nonterminal on parse stack top) -> production(s) to use
    let actions : int * int -> int list option =  (Map.ofArray actionsTable).TryFind

    let verbose depth x=
        printf "%s" <| String.replicate depth " "
        printfn "%s" x

    member this.parse () =
        // expands the nonterminals following specified node in right-to-left traversals
        // until a terminal matching current input terminal is found in every traversal
        let rec expandFromNode depth currentInputPos currentInputTerm (currentSet : NodeWithHistory list) (nodeH : NodeWithHistory) =
            let node = nodeH.Node
            match node.Item with
            // found tree node matching current input terminal: add its next to found set
            | Trm trm when trm = currentInputTerm ->
                verbose depth "*"
                let newTraversals =
                    match nodeH.Traversals with
                    | IncompleteLink tails -> Link (node, tails)
                    | x -> Link (node, [x])
                let nextNodeH = NodeWithHistory(node.NextNode, newTraversals)
                nextNodeH :: currentSet
            // found tree node not matching current input terminal
            | Trm trm ->
                currentSet
            | Ntrm ntrm ->
                match actions (currentInputTerm, ntrm) with
                // found nonterminal; expand all its available productions
                | Some productionIndices ->
                    verbose depth <| System.String.Join (" ", productionIndices)
                    let nextNode = node.NextNode
                    let expandProduction productionIndex =
                        let production = productions.[productionIndex]
                        let newNextNode =
                            List.foldBack (fun (item, itemIndex) next -> Node (item, Some next, Some node, productionIndex, itemIndex))
                                          (List.zip production [0..production.Length-1])
                                          nextNode
                        NodeWithHistory(newNextNode, nodeH.Traversals)
                    let newNextNodesH = List.map expandProduction productionIndices
                    List.fold (expandFromNode (depth+1) currentInputPos currentInputTerm) currentSet newNextNodesH
                // found nonterminal but no productions
                | None ->
                    currentSet
        let mergeCurrentTreeNodes nodes =
            nodes |> Seq.distinct |> List.ofSeq

        // prepare initial structure
        let eofNode = Node (Trm eofToken, None, None, 0, 0)
        let rootNode = Node (Ntrm startNonTerm, Some eofNode, None, 0, 0)
        let rootNodeH = NodeWithHistory (rootNode, Empty)
        
        // tree leafs to match with current terminal in the input buffer
        let mutable currentTreeNodes = [rootNodeH]
        let mutable pos = 0

        let tokensEnumerator = tokens.GetEnumerator ()
        while currentTreeNodes.Length > 0 && tokensEnumerator.MoveNext () do
            verbose 0 <| sprintf "Current input: %d" tokensEnumerator.Current
            currentTreeNodes <-
                List.fold (expandFromNode 0 pos tokensEnumerator.Current) [] currentTreeNodes
            let countBeforeMerge = currentTreeNodes.Length
            currentTreeNodes <- mergeCurrentTreeNodes currentTreeNodes

            let mergedNodesCount = countBeforeMerge - currentTreeNodes.Length
            if mergedNodesCount > 0 then verbose 0 <| sprintf "Merged out: %d" mergedNodesCount
            let nodesCount = currentTreeNodes |> Seq.distinctBy (fun nodeH -> nodeH.Node) |> Seq.length
            if currentTreeNodes.Length <> nodesCount then failwith "merge was incorrect"

            pos <- pos + 1

        List.exists (fun (nodeH:NodeWithHistory) -> nodeH.Node = eofNode) currentTreeNodes
