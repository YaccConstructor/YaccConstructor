// Parser.fs contains base logic for table-based GLL parsing

module Yard.Generators.GLL.Parser
open Yard.Generators.GLL.AST

type ParseStack = GrammarItem list

/// <summary>
/// Elementary descriptor consists of the following:
/// s: parse stack
/// j: position in the input array
/// </summary>
type ElementaryDescriptor = ParseStack * int

// startNonTerm: int that represents start Ntrm
// eofToken: what to expect as the last token of input
// actionsTable: (lookahead terminal, nonterminal on parse stack topm, production(s) to use)
// productions: right sides of grammar rules
// tokens: input stream of grammar terminals (their int codes)
type ParserBase (startNonTerm, eofToken, actionsTable, productions : ParseStack[], tokens : int[]) =
    // states set R
    let mutable parseStates : ElementaryDescriptor list = []
    // represents actions table: (lookahead terminal, nonterminal on parse stack top) -> production(s) to use
    let actions : int * int -> int list option = (Map.ofArray actionsTable).TryFind

    member this.parse () =
        // jumps to next available nondeterministic execution branch
        let rec continueExecution () =
            match parseStates with
            | [] -> false
            | state::states ->
                parseStates <- states
                parse state
        // matches buffer input and stack top against parse table and takes actions
        and parse (stack, pos) =
            match stack with
            | (Ntrm nonterm)::stackRest ->
                match actions (tokens.[pos], nonterm) with
                | Some productionIndices -> 
                    let addProduction productionIndex =
                        let newStack = List.append productions.[productionIndex] stackRest
                        parseStates <- (newStack, pos) :: parseStates
                    List.iter addProduction productionIndices                    
                | None -> ()
                continueExecution ()
            | (Trm term)::stackRest ->
                if tokens.[pos] = term
                then parse (stackRest, pos + 1)
                else continueExecution ()
            | [] -> pos = tokens.Length || continueExecution ()
        parse ([Ntrm startNonTerm; Trm eofToken], 0)
