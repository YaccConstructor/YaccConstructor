module Yard.Frontends.CompressionFrontend.Compression

open System.Collections.Generic
open System.IO
open System.Collections
open Yard.Core.IL
open Production

type Rule =
    val number : int
    val mutable count : int
    val lst: LinkedList<Symbol>
    new (number) = { number = number; count = 0; lst = LinkedList<Symbol>() }
    member x.IncCount() =
        x.count <- x.count + 1
    member x.DecCount() =
        x.count <- x.count - 1

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

let compress (text: string) (separator: char) : Definition.t<Source.t, Source.t> =

    let special = int separator
    let numTerminals = 100000
    let mutable numRules = 0
    let Rules = new Dictionary<int, Rule>()
    let firstRule = new Rule(numRules)
    numRules <- numRules + 1
    Rules.Add(firstRule.number, firstRule)
    
    let rec check (s: LinkedListNode<Symbol>) =
    
        let matchDigrams (newD: LinkedListNode<Symbol>) (found: LinkedListNode<Symbol>) (rule: Rule) =
            let expand (r: Rule) =
                let underused = r.lst.First.Value.Rule
                r.lst.RemoveFirst()
                while underused.lst.First <> null do
                    r.lst.AddFirst(underused.lst.Last.Value) |> ignore
                    underused.lst.RemoveLast()
                Rules.Remove(underused.number) |> ignore
        
            if rule.lst.Count = 2
            then
                //reuse the rule
                substitute newD rule
                if rule.lst.First.Value.isNonTerminal() && rule.lst.First.Value.Rule.count = 1 
                then expand rule
            else
                //create new rule
                let r = new Rule(numRules)
                numRules <- numRules + 1
                Rules.Add(r.number, r)
                r.lst.AddLast(newD.Value) |> ignore
                r.lst.AddLast(newD.Next.Value) |> ignore
                if r.lst.First.Value.isNonTerminal() then r.lst.First.Value.Rule.IncCount()
                if r.lst.Last.Value.isNonTerminal() then r.lst.Last.Value.Rule.IncCount()

                substitute found r
                substitute newD r
            
                if r.lst.First.Value.isNonTerminal() && r.lst.First.Value.Rule.count = 1
                then expand r
      
        //check if there is another such digram
        if s = null || s.Next = null || s.Value.Value = special || s.Next.Value.Value = special
        then false
        else
            let rec nextSym (f: LinkedListNode<Symbol>) i = 
                if f.Value = s.Value && f.Next.Value = s.Next.Value && not(f = s || f.Next = s)
                then
                    matchDigrams s f (Rules.Item(i))
                    true
                elif f.Next <> null && f.Next.Next <> null then nextSym f.Next i
                else false
            let rec nextRule i =
                if Rules.ContainsKey(i) && nextSym (Rules.Item(i)).lst.First i
                then true
                elif i < numRules then nextRule (i + 1)
                else false
            nextRule 0
                        
    and substitute (s : LinkedListNode<Symbol>) (r : Rule) =
        r.IncCount()
        if s.Value.isNonTerminal() then s.Value.Rule.DecCount()
        if s.Next.Value.isNonTerminal() then s.Next.Value.Rule.DecCount()
        s.List.AddAfter(s.Next, NonTerminal(numTerminals + r.number, r)) |> ignore
        let prevNode = s.Previous
        s.List.Remove(s.Next)
        s.List.Remove(s)
        if prevNode <> null && not(check prevNode) then check prevNode.Next |> ignore


    for i in text do
        int i |> Terminal |> firstRule.lst.AddLast |> ignore
        firstRule.lst.Last.Previous |> check |> ignore
    
    //separate firstRule by special
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
        newFirst.lst.AddLast(NonTerminal(r.number, r)) |> ignore
    Rules.Remove(0) |> ignore
    if newFirst.lst.Count > 0 then Rules.Add(newFirst.number, newFirst)
    let g : Grammar.t<Source.t, Source.t>= 
        [{openings = []; 
        allPublic = false; 
        name = None; 
        rules = 
            //modules
            [for i in Rules.Keys ->
                let addRule (p: LinkedList<Symbol>) = 
                        if p.Count = 1
                        then
                            let x = p.First.Value
                            if x.isNonTerminal() 
                            then PRef(Source.t ("sq_" + (x.Value).ToString()), None)
                            else PToken(Source.t ((char(x.Value)).ToString()))
                        else
                            PSeq([for x in p ->
                                    {omit = false; 
                                    binding = None; 
                                    checker = None;
                                    rule = (if x.isNonTerminal() 
                                            then PRef(Source.t ("sq_" + (x.Value).ToString()), None)
                                            else PToken(Source.t ((char(x.Value)).ToString())))
                                    }],
                                None, None)

                if i = 0
                then
                    let rec alt (x : LinkedListNode<Symbol>) =
                        match x.Next with
                        |null -> addRule x.Value.Rule.lst
                        |y -> PAlt(addRule x.Value.Rule.lst, alt y)
                    {name = Source.t "sq_0";
                    args = [];
                    isStart = true;
                    isInline = false;
                    metaArgs = [];
                    isPublic = false;
                    body = alt (Rules.Item(i).lst.First)}
                else
                    {name = Source.t ("sq_" + i.ToString()); 
                    args = []; 
                    isStart = false; 
                    isInline = false; 
                    metaArgs = []; 
                    isPublic = false; 
                    body = addRule (Rules.Item(i).lst)}
        ]}]
    {info = {fileName = ""}; head = None; foot = None; grammar = g; options = Map.empty; tokens = Map.empty} 


let compressList (s: string list) =
    compress (String.concat "$" s) '$'