module Yard.Frontends.CompressionFrontend.Compression

open System.Collections.Generic
open System.IO
open System.Collections
open Yard.Core.IL

type Rule =
    val number : int
    val mutable count : int
    val lst: LinkedList<Symbol>
    val mutable index : int
    new (number) = {number = number; count = 0; lst = LinkedList<Symbol>(); index = -1}
    member x.IncCount() =
        x.count <- x.count + 1
    member x.DecCount() =
        x.count <- x.count - 1
    member x.putInd(n) = 
        x.index <- n
    
and Symbol = Terminal of int | NonTerminal of int * Rule with
    member x.isNonTerminal() = 
        match x with
        | Terminal _ -> false
        | _ -> true
    member x.Value = 
        match x with
        | Terminal v -> v
        | NonTerminal (v, _) -> v
    member x.Rule =
        match x with 
        | Terminal _ -> failwith "that was a Terminal"
        | NonTerminal (_, r) -> r

let compress (text: string) (separator: char) : Definition<Source.t, Source.t> =

    let special = int separator
    let numTerminals = 100000
    let mutable numRules = 0
    let Digrams = new Dictionary<(Symbol * Symbol), (LinkedListNode<Symbol> * int)>()
    let Rules = new Dictionary<int, Rule>()
    let firstRule = new Rule(numRules)
    numRules <- numRules + 1
    Rules.Add(firstRule.number, firstRule)
    
    let expand (r: Rule) =
        let underused = r.lst.First.Value.Rule
        Digrams.Remove(r.lst.First.Value, r.lst.First.Next.Value) |> ignore
        r.lst.RemoveFirst()
        while underused.lst.First <> null do
            r.lst.AddFirst(underused.lst.Last.Value) |> ignore
            underused.lst.RemoveLast()
            //replacing digrams to another rule
            Digrams.Remove(r.lst.First.Value, r.lst.First.Next.Value) |> ignore
            Digrams.Add((r.lst.First.Value, r.lst.First.Next.Value), (r.lst.First, r.number))
        Rules.Remove(underused.number) |> ignore

    let rec handle (stack: Stack<LinkedListNode<Symbol> * int>) = 
        
        let substitute (n: int) (s: LinkedListNode<Symbol>) (r: Rule) =
            r.IncCount()
            //remove old digrams
            if s.Previous <> null then Digrams.Remove(s.Previous.Value, s.Value) |> ignore
            if s.Next.Next <> null then Digrams.Remove(s.Next.Value, s.Next.Next.Value) |> ignore
            
            if s.Value.isNonTerminal() then s.Value.Rule.DecCount()
            if s.Next.Value.isNonTerminal() then s.Next.Value.Rule.DecCount()
            s.List.AddAfter(s.Next, NonTerminal(numTerminals + r.number, r)) |> ignore
            
            stack.Push(s.Next.Next, n)
            stack.Push(s.Previous, n)
            s.List.Remove(s.Next)
            s.List.Remove(s)
            
        let matchDigrams (newD: LinkedListNode<Symbol>) (n: int) (found: LinkedListNode<Symbol>) (rule: Rule) =
            if rule.lst.Count = 2
            then
                //reuse the rule
                substitute n newD rule
                if rule.lst.First.Value.isNonTerminal() && rule.lst.First.Value.Rule.count = 1 
                then expand rule
            else
                //create new rule
                let r = new Rule(numRules)
                numRules <- numRules + 1
                Rules.Add(r.number, r)
                r.lst.AddLast(newD.Value) |> ignore
                r.lst.AddLast(newD.Next.Value) |> ignore
                Digrams.Remove(newD.Value,newD.Next.Value) |> ignore
                Digrams.Add((newD.Value,newD.Next.Value), (r.lst.First, r.number))
                if r.lst.First.Value.isNonTerminal() then r.lst.First.Value.Rule.IncCount()
                if r.lst.Last.Value.isNonTerminal() then r.lst.Last.Value.Rule.IncCount()

                substitute (rule.number) found r
                substitute n newD r
            
                if r.lst.First.Value.isNonTerminal() && r.lst.First.Value.Rule.count = 1
                then expand r
        
        let s = fst (stack.Peek())
        let n = snd (stack.Pop())

        if not (s = null || s.Next = null || s.Value.Value = special || s.Next.Value.Value = special)
        then
            if Digrams.ContainsKey(s.Value, s.Next.Value)
                then
                    let node = Digrams.Item(s.Value, s.Next.Value)
                    if fst node <> s && fst node <> s.Next && s <> (fst node).Next
                    then matchDigrams s n (fst node) (Rules.Item(snd node))
                else
                    Digrams.Add((s.Value, s.Next.Value), (s, n))

        if stack.Count > 0 then handle stack

    for i in text do
        int i |> Terminal |> firstRule.lst.AddLast |> ignore
        let stc = new Stack<LinkedListNode<Symbol> * int>()
        stc.Push(firstRule.lst.Last.Previous, 0) 
        stc|> handle
    //for rules names
    let mutable INDEX  = 1
    let Index (x: Rule) = 
        if x.index = -1
        then
            x.putInd(INDEX)
            INDEX <- INDEX + 1
        (x.index).ToString()
    //separate firstRule by special for making alternatives
    let tmp = LinkedList<Symbol>()
    let newFirst = new Rule(0)
    for sym in Rules.Item(0).lst do
        if sym.Value = special
        then
            if tmp.Count > 0
            then
                let r = new Rule(numRules)
                numRules <- numRules + 1
                for s in tmp do
                    r.lst.AddLast(s) |> ignore
                tmp.Clear()
                newFirst.lst.AddLast(NonTerminal(r.number, r)) |> ignore
        else tmp.AddLast(sym) |> ignore
    if tmp.Count > 0
    then
        let r = new Rule(numRules)
        numRules <- numRules + 1
        for s in tmp do
            r.lst.AddLast(s) |> ignore
        tmp.Clear()
        newFirst.lst.AddLast(NonTerminal(numTerminals + r.number, r)) |> ignore
    Rules.Remove(0) |> ignore
    if newFirst.lst.Count > 0 then Rules.Add(newFirst.number, newFirst)
    //to IL format
    let g : Grammar<Source.t, Source.t>= 
        [{openings = []; 
        allPublic = false; 
        name = None; 
        rules = 
            //modules
            [for i in Rules.Keys ->
                let makeRule (p: LinkedList<Symbol>) = 
                        let oneElem (x: Symbol) = 
                            if x.isNonTerminal() 
                            then PRef(Source.t ("sq_" + Index x.Rule), None)
                            else PToken(Source.t ((char(x.Value)).ToString()))
                        if p.Count = 1 then oneElem p.First.Value
                        else
                            PSeq([for x in p ->
                                    {omit = false; 
                                    binding = None; 
                                    checker = None;
                                    rule = oneElem x
                                    }],
                                None, None)

                if i = 0
                then
                    let rec makeAlt (x : LinkedListNode<Symbol>) =
                        match x.Next with
                        |null -> makeRule x.Value.Rule.lst
                        |y -> PAlt(makeRule x.Value.Rule.lst, makeAlt y)
                    {name = Source.t "sq_0";
                    args = [];
                    isStart = true;
                    isInline = false;
                    metaArgs = [];
                    isPublic = false;
                    body = makeAlt (Rules.Item(i).lst.First)}
                else
                    {name = Source.t ("sq_" + Index (Rules.Item(i))); 
                    args = []; 
                    isStart = false; 
                    isInline = false; 
                    metaArgs = []; 
                    isPublic = false; 
                    body = makeRule (Rules.Item(i).lst)}
        ]}]
    {info = {fileName = ""}; head = None; foot = None; grammar = g; options = Map.empty; tokens = Map.empty} 


let compressList (s: string list) =
    compress (String.concat "$" s) '$'