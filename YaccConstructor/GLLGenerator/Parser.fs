// Parser.fs contains base logic for table-based GLL parsing

module Yard.Generators.GLL.Parser

type ParseStackItem = Terminal of int | Nonterminal of int
type ParseStack = ParseStackItem list

/// <summary>
/// Elementary descriptor consists of the following:
/// s: parse stack
/// j: position in the input array
/// </summary>
type ElementaryDescriptor = ParseStack * int

type ParserBase (startNonTerm, eofToken, actions, productions, tokens) =
    // states set R
    let mutable _parseStates : ElementaryDescriptor list = []
    // right sides of grammar rules
    let _productions : ParseStack[] = productions
    // represents actions table: (lookahead terminal, nonterminal on parse stack top) -> production(s) to use
    let _actions : int * int -> int list option = (Map.ofArray actions).TryFind
    // input stream of grammar terminals
    let _tokens : int[] = tokens

    member this.parse () =
        // jumps to next available nondeterministic execution branch
        let rec continueExecution () =
            match _parseStates with
            | [] -> false
            | state::states ->
                _parseStates <- states
                parse state
        // matches buffer input and stack top against parse table and takes actions
        and parse (stack, pos) =
            match stack with
            | (Nonterminal nonterm)::stackRest ->
                match _actions (_tokens.[pos], nonterm) with
                | Some productionIndices -> 
                    let addProduction productionIndex =
                        let newStack = List.append _productions.[productionIndex] stackRest
                        _parseStates <- (newStack, pos) :: _parseStates
                    List.iter addProduction productionIndices                    
                | None -> ()
                continueExecution ()
            | (Terminal term)::stackRest ->
                if _tokens.[pos] = term
                then parse (stackRest, pos + 1)
                else continueExecution()
            | [] -> pos = _tokens.Length || continueExecution ()
        parse ([Nonterminal startNonTerm; Terminal eofToken], 0)
