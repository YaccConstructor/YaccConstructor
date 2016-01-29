namespace Yard.Unger

open LanguagePrimitives
open Yard.Generators.Common.FinalGrammar
open System.IO
open System.Collections
open System.Collections.Generic


type ProductionRule (leftHandSides : int, rightHandSides : int[]) = 
       
    let  leftHandSide : int = leftHandSides
    let rightHandSide : int[] = rightHandSides
    new() = new ProductionRule(0, [||])
    member this.getLeftHandSide() : int = leftHandSide
    member this.getRightHandSide() : int[] = rightHandSide

    member this.equals(rule : ProductionRule) = ((rule.getLeftHandSide() = leftHandSide) && (rule.getRightHandSide() = rightHandSide))

type Goal(r : ProductionRule, p : int, l : int) = 
    let mutable rule : ProductionRule = r
    let mutable position : int = p
    let mutable length : int = l

    new() = new Goal(new ProductionRule(), 0, 0)

    member this.getRule() = rule
    member this.getPosition() = position
    member this.getLength() = length
    member this.equals(goal : Goal) = ((goal.getPosition() = position) && (goal.getLength() = length) && (goal.getRule().equals(rule)))


type OperationTuple(g : Goal, rp : int, ir : int) = 
    let mutable goal = g
    let mutable rhsPosition = rp
    let mutable inRec = ir

    new() = new OperationTuple(new Goal(),0,0)
    
    member this.getGoal() = goal
    member this.getRhsPosition() = rhsPosition
    member this.getInRec() = inRec

    member this.incrementRhsPosition(value : int) = rhsPosition <- rhsPosition + value;
    member this.incrementInRec( value : int) = inRec <- inRec + value
    member this.decrementRhsPosition(value : int) = rhsPosition <- rhsPosition - value
    member this.decrementInRec (value : int) = inRec <- inRec - value

type Grammar(grammarPath : string) = 

    let ilParser = new Yard.Frontends.YardFrontend.YardFrontend()
    let il = ilParser.ParseGrammar(grammarPath)
    let grammar = new FinalGrammar(il.grammar.[0].rules, true)
        
    let mutable productionRules = []
    let mutable startSymbol : int = grammar.rules.leftSide grammar.startRule
    let mutable nonTerms = []
    let eor = -100

    member this.retgr = grammar
    member this.Grammar() = 
        for i in 0..grammar.rules.rulesCount-1 do
             let lhs = grammar.rules.leftSide i 
             productionRules <- List.append  [new ProductionRule(lhs, (Array.append (grammar.rules.rightSide i) [|eor|]))]  productionRules 
             if  (nonTerms.IsEmpty) || (not (List.exists(fun x -> x = lhs) nonTerms)) then nonTerms <- List.append nonTerms [lhs]

    member this.getRulesForSymbol(symbol : int) = 
        let mutable rulesForSymbol = []
        for i in 0..productionRules.Length-1 do
            let rule : ProductionRule = productionRules.[i]
            if (rule.getLeftHandSide() = symbol) then
                rulesForSymbol <- List.append [rule] rulesForSymbol
        rulesForSymbol
        
    member this.getStartSymbol = startSymbol
    member this.isNonTerminal (symbol : int) = List.exists (fun x-> x = symbol) nonTerms

type Parser(grammar : Grammar)=

    let mutable operationalStack  = new Stack<OperationTuple>()
    let mutable derivationStack = new Stack<ProductionRule>()
    let mutable parsingForest : ProductionRule[] list = []


    let mutable grammar : Grammar = grammar
    let mutable word : List<int> = List[]

    let eor : int = -100

    let copy input =
        let rec copy acc input =
            match input with
            |[] -> List.rev acc
            |x::xs -> copy (x::acc) xs
        copy [] input

    let convertSymbolToString (symbol : int) =
        if symbol < grammar.retgr.indexator.nonTermCount
        then grammar.retgr.indexator.indexToNonTerm symbol
        elif symbol >= grammar.retgr.indexator.termsStart && symbol <= grammar.retgr.indexator.termsEnd
        then grammar.retgr.indexator.indexToTerm symbol
        else grammar.retgr.indexator.indexToLiteral symbol
                
    let convList (lst : int[]) =
        let mutable l = []
        for i in 0..lst.Length-2 do    
            l <- List.append l [convertSymbolToString lst.[i]] 
        l 
    
    // convert parsingForest ProductionRules from int to string
    let pfs (parsingForest : ProductionRule[] list) =
        let mutable parsingForestString = []
        let su = (parsingForest.Length) - 1
        for i in 0..su do
            let mutable uns = []
            for j in 0..parsingForest.[i].Length-1 do
                let mutable lft = convertSymbolToString(parsingForest.[i].[j].getLeftHandSide())
                uns <- List.append [(lft, convList(parsingForest.[i].[j].getRightHandSide()))] uns 
            parsingForestString <- List.append parsingForestString [uns]
        parsingForestString    
        
    member this.parse(inpWord : string[]) =
        let wordConvert =
            let mutable w : List<int>= new List<int>()
            for i in 0..inpWord.Length-1 do
                try 
                       w.Add(grammar.retgr.indexator.literalToIndex inpWord.[i]) 
                with
                     | :? System.Collections.Generic.KeyNotFoundException ->
                            try
                                w.Add(grammar.retgr.indexator.termToIndex inpWord.[i])
                            with
                                | :? System.Collections.Generic.KeyNotFoundException -> ()
            w
        grammar.Grammar()
        word <- wordConvert
        this.tryAllRulesFor(grammar.getStartSymbol,0,word.Count)
        let mutable ret = true
        let  parsingForestString = pfs parsingForest

        
        parsingForestString
         
    member this.tryAllRulesFor(symbol : int, position : int, lenght : int) = 

        let rulesForSymbol = grammar.getRulesForSymbol(symbol)

        for i in 0..rulesForSymbol.Length-1 do
            this.tryRule(rulesForSymbol.[i],position,lenght)

    member this.tryRule(rule : ProductionRule, position : int, lenght : int) =
        let goal : Goal = new Goal(rule, position, lenght)

        if (not (this.isToBeAvoided(goal)) ) then
            operationalStack.Push(new OperationTuple(goal , -1, 0))
            derivationStack.Push(rule)
            this.doTopOfStack()
            derivationStack.Pop() |> ignore
            operationalStack.Pop() |> ignore
 

    member this.doTopOfStack()=
        let  s : OperationTuple = operationalStack.Peek()
        let goal : Goal  = s.getGoal()
        let nextRhsSymbol : int = goal.getRule().getRightHandSide().[s.getRhsPosition() + 1]
        if (nextRhsSymbol = eor) then
            if (s.getInRec() = goal.getLength()) then
                this.doNextOnStack()
        elif ((s.getInRec() <  goal.getLength()) && (nextRhsSymbol = word.[goal.getPosition() + s.getInRec()])) then
            s.incrementRhsPosition(1)
            s.incrementInRec(1)
            this.doTopOfStack()
            s.decrementRhsPosition(1)
            s.decrementInRec(1)
        elif (grammar.isNonTerminal(nextRhsSymbol)) then
            this.tryAllLengthsFor(nextRhsSymbol, goal.getPosition() + s.getInRec(), goal.getLength() - s.getInRec())

    member this.doNextOnStack()=
        let s : OperationTuple = operationalStack.Peek()
        operationalStack.Pop() |> ignore
        if (operationalStack.Count = 0 ) then
            parsingForest <- derivationStack.ToArray() :: parsingForest


        else 
            let s1 : OperationTuple = operationalStack.Peek()
            s1.incrementRhsPosition(1)
            s1.incrementInRec(s.getGoal().getLength())
            this.doTopOfStack()
            s1.decrementInRec(s.getGoal().getLength())
            s1.decrementRhsPosition(1)
        operationalStack.Push(s)

    
    member this.tryAllLengthsFor(nonTerminal : int, position :  int , length : int )=

        for i in 0..length  do 
            this.tryAllRulesFor(nonTerminal, position, i)
    
    member this.isToBeAvoided(goal : Goal)=
        let mutable k = false
        for i in 0..(operationalStack.Count-1) do
            if (operationalStack.ToArray().[i].getGoal().equals(goal) = true) then  k <- true
        k