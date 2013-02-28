// Parser.fs contains base logic for table-based GLL parsing

module Yard.Generators.GLL.Parser
open Yard.Generators.GLL.AST

type Production = GrammarItem list

// startNonTerm: int that represents start Ntrm
// eofToken: what to expect as the last token of input
// actionsTable: (lookahead terminal, nonterminal on parse stack topm, production(s) to use)
// productions: right sides of grammar rules
// tokens: input stream of grammar terminals (their int codes)
type ParserBase (startNonTerm, eofToken, actionsTable, productions : Production[], tokens : int[]) =
    // represents actions table: (lookahead terminal, nonterminal on parse stack top) -> production(s) to use
    let actions : int * int -> int list option = (Map.ofArray actionsTable).TryFind

    member this.parse () =
        // expands the nonterminals following specified node in right-to-left traversals
        // until a terminal matching current input terminal is found in every traversal
        let expandToCurrentInput currentInputTerm currentSet (startNode : Node) =
            let rec expandFromNode (previousNode : Node) (currentSet : Node list) (node : Node) =
                match node.Item with
                | Trm trm when trm = currentInputTerm ->
                    // found tree node matching current input terminal: add it to found set                    
                    node :: currentSet
                | Trm _ ->
                    // found tree node not matching current input terminal
                    // TODO: go right-to-left and clean 
                    currentSet
                | Ntrm ntrm ->
                    match actions (currentInputTerm, ntrm) with
                    // found nonterminal; expand all its available productions
                    | Some productionIndices ->
                        let nextNode = Seq.nth 0 node.NextNodes // nonterms always have 1 next node
                        let expandProduction productionIndex =
                            let production = productions.[productionIndex]
                            List.foldBack (fun (item, itemIndex) next -> Node (item, next, (productionIndex, itemIndex, node)))
                                          (List.zip production [0..production.Length-1])
                                          nextNode
                        let newNextNodes = List.map expandProduction productionIndices
                        previousNode.reassignNext node newNextNodes
                        List.fold (expandFromNode previousNode) currentSet newNextNodes
                    // found nonterminal but no productions
                    | None -> currentSet
            Seq.fold (expandFromNode startNode) currentSet startNode.NextNodes
        
        // prepare initial structure
        let fakeEndNode = Node (Trm eofToken)
        let rootNode = Node (Ntrm startNonTerm)
        rootNode.addNext fakeEndNode
        let fakeStartNode = Node (Trm eofToken)
        fakeStartNode.addNext rootNode        

        // tree leafs to match with current terminal in the input buffer
        let mutable currentTreeTerminals : Node list = [fakeStartNode]
        let mutable pos = 0

        while currentTreeTerminals.Length > 0 && pos < tokens.Length do            
            currentTreeTerminals <-
                List.fold (expandToCurrentInput tokens.[pos]) [] currentTreeTerminals
                |> Seq.distinct |> List.ofSeq
            let posLocal = pos
            List.iter (fun (node:Node) -> node.ItemPos <- posLocal) currentTreeTerminals
            pos <- pos + 1

        pos = tokens.Length && List.exists ((=) fakeEndNode) currentTreeTerminals
