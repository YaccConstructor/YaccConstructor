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
        let rec expandFromNode currentInputTerm (currentSet : Node list) (node : Node) =
            match node.Item with
            // found tree node matching current input terminal: add its next to found set
            | Trm trm when trm = currentInputTerm ->
                let nextNode = node.NextNode.Value
               // node.NextNode <- None
                nextNode :: currentSet
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
                        List.foldBack (fun (item, itemIndex) next -> Node (item, next, (productionIndex, itemIndex, node)))
                                        (List.zip production [0..production.Length-1])
                                        nextNode
                    let newNextNodes = List.map expandProduction productionIndices
                    List.fold (expandFromNode currentInputTerm) currentSet newNextNodes
                // found nonterminal but no productions
                | None ->
                    currentSet

        // prepare initial structure
        let fakeEndNode = Node (Trm eofToken, None)
        let eofNode = Node (Trm eofToken, Some fakeEndNode)
        let rootNode = Node (Ntrm startNonTerm, Some eofNode)

        // tree leafs to match with current terminal in the input buffer
        let mutable currentTreeNodes : Node list = [rootNode]
        let mutable pos = 0

        while currentTreeNodes.Length > 0 && pos < tokens.Length do
            currentTreeNodes <-
                List.fold (expandFromNode tokens.[pos]) [] currentTreeNodes
                |> Seq.distinct |> List.ofSeq
            let posLocal = pos
            List.iter (fun (node:Node) -> node.setItemPos posLocal) currentTreeNodes
            pos <- pos + 1

        pos = tokens.Length && List.exists ((=) fakeEndNode) currentTreeNodes
