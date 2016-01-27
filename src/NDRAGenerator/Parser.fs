namespace Yard.Generators.NDRA

open LanguagePrimitives
open Yard.Generators.Common.FinalGrammar
open System.IO

type Productions = Map<int, int[] list>

type Item = {
    //
    Nonterminal : int;
    //
    Production : int[];
    //
    Position : int;
} with
    member this.CurrentSymbol
        with get () =
            let position = int this.Position
            if position = Array.length this.Production then None
            else Some this.Production.[position]

type Items = Set<Item>

type ParserState = Set<Item * int>
                
type Parser(grammarPath : string) =
    let parser = new Yard.Frontends.YardFrontend.YardFrontend()
    let il = parser.ParseGrammar(grammarPath)
    let grammar = new FinalGrammar(il.grammar.[0].rules, true)
    // Convert productions from FinalGrammar to Productions type
    let convertProductions =
        let mutable productions : Productions = Map.empty
        for i = 0 to grammar.rules.rulesCount-1 do
            let leftSide = grammar.rules.leftSide i
            let list = productions.TryFind leftSide
            match list with
            | None -> productions <- productions.Add(leftSide, [grammar.rules.rightSide i])
            | Some x ->
                productions <- productions.Remove(leftSide)
                productions <- productions.Add(leftSide, (grammar.rules.rightSide i) :: x)
        productions
    let productions : Productions = convertProductions
    let rec closureImpl (productions : Productions) items pendingItems : Items =
        match pendingItems with
        | [] ->
            items
        | _ ->
            // Process the worklist.
            let items, pendingItems =
                ((items, []), pendingItems)
                ||> List.fold (fun (items, pendingItems) (item : Item) ->
                    // Add the current item to the item set.
                    let items = Set.add item items                

                    // If the position is at the end of the production, or if the current symbol
                    // is a terminal, there's nothing that needs to be done for this item.
                    match item.CurrentSymbol with
                    | None -> 
                        items, pendingItems
                    | Some (x) ->
                        if not (grammar.indexator.isNonTerm x) then
                            items, pendingItems
                        else
                            // For all productions of this nonterminal, create a new item
                            // with the parser position at the beginning of the production.
                            // Add these new items into the set of items.
                            let pendingItems =
                                /// The productions of this nonterminal.
                                let nontermProductions = Map.find x productions

                                (pendingItems, nontermProductions)
                                ||> List.fold (fun pendingItems production ->
                                    let newItem = {
                                        Nonterminal = x;
                                        Production = production;
                                        Position = GenericZero; }

                                    // Only add this item to the worklist if it hasn't been seen yet.
                                    if Set.contains newItem items then pendingItems
                                    else newItem :: pendingItems)
                                
                            // Return the updated item set and worklist.
                            items, pendingItems)

            // Recurse to continue processing.
            // OPTIMIZE : It's not really necessary to reverse the list here -- we could just as easily
            // process the list in reverse but for now we'll process it in order to make the algorithm
            // easier to understand/trace.
            closureImpl productions items (List.rev pendingItems)

    /// Computes the closure of a set of items.
    let closure (productions : Productions) items : Items =
        // Call the recursive implementation, starting with the specified initial item set.
        Set.difference (closureImpl productions Set.empty (Set.toList items)) items

    /// Moves the 'dot' (the current parser position) past the
    /// specified symbol for each item in a set of items.
    let goto symbol items closure (productions : Productions) : Items =
        (Set.empty, Set.union items closure)
        ||> Set.fold (fun updatedItems (item : Item) ->
            // If the next symbol to be parsed in the production is the
            // specified symbol, create a new item with the position advanced
            // to the right of the symbol and add it to the updated items set.
            match item.CurrentSymbol with
            | Some sym when sym = symbol ->
                let updatedItem =
                    { item with
                        Position = item.Position + 1; }
                Set.add updatedItem updatedItems

            | _ ->
                updatedItems)


    let final (item : Item) : bool =
        item.CurrentSymbol = None

    let lhs (item : Item) : int =
        item.Nonterminal

    let pop (item : Item) : Item =
        { item with
            Position = item.Position - 1}

    let eps (item : Item) : bool =
        item.Position = 0 && item.CurrentSymbol = None

    let findEpsilonNonterminals (items : Items) : Set<int> =
        let nonterminals : Set<int> ref = ref Set.empty
        Set.iter (fun x -> if eps x then nonterminals := (!nonterminals).Add x.Nonterminal) items
        !nonterminals

    let findFinalItems (items : Items) position : ParserState =
        let finalItems : ParserState ref = ref Set.empty
        Set.iter (fun x -> if final x then finalItems := (!finalItems).Add (x, position)) items
        !finalItems
    member this.Parse (input : string[]) =
        let rec afterApplying (items : Items) (symbol : int) position : ParserState =
            if position >= Array.length input || items.IsEmpty then Set.empty
            else
                let closure = closure productions items
                let gotoItems = goto symbol items closure productions
                let nextItems = beforeApplying gotoItems position
                let result : ParserState ref = ref Set.empty
                let recursionItems : ParserState ref = ref Set.empty
                Set.iter (fun (a, b) -> 
                    let pop = pop a
                    if items.Contains pop then result := (!result).Add (pop, b)
                    if closure.Contains pop then recursionItems := (!recursionItems).Add (a, b)) nextItems
                Set.iter (fun (a, b) -> result := Set.union !result (afterApplying items (lhs a) b)) !recursionItems
                !result
        and beforeApplying (items : Items) position : ParserState =
            if position >= Array.length input || items.IsEmpty then Set.empty
            else
                let closure = closure productions items
                let result : ParserState ref = ref Set.empty
                if position < Array.length input - 1 then
                    try
                        result := afterApplying items (grammar.indexator.literalToIndex input.[position + 1]) (position + 1)
                    with
                    | :? System.Collections.Generic.KeyNotFoundException ->
                        try
                        result := afterApplying items (grammar.indexator.termToIndex input.[position + 1]) (position + 1)
                        with
                        | :? System.Collections.Generic.KeyNotFoundException -> ()
                result := Set.union !result (findFinalItems items position)
                let firstNonterminals = findEpsilonNonterminals closure
                Set.iter (fun x -> result := Set.union !result (afterApplying items x position)) firstNonterminals
                !result
        let startItem = {
                    Nonterminal = grammar.rules.leftSide grammar.startRule;
                    Production = grammar.rules.rightSide grammar.startRule;
                    Position = GenericZero;}
        let mutable items : Items = Set.empty
        items <- items.Add startItem
        Set.map (fun (_, b) -> b + 1) (beforeApplying items -1)
       