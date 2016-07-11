module Yard.Generators.LLK.Parser
open System.Collections.Generic
open System.Collections
open System.IO
open System
open System.Linq

type Parser(tokenToNumber       : string -> int
         , isTerminal           : string -> bool
         , table                : int[][]
//         , startRule            : int
         , startNT              : int
         , numIsTerminal        : int -> bool
//         , numIsNonTerminal     : int -> bool
         , k                    : int
         , chainToNum           : String -> int
         , rules                : int[][]
                    ) =
    let stack = new Stack<int>()

    member this.TokenToNumber   = tokenToNumber
    member this.IsTerminal      = isTerminal
    member this.Table           = table
//    member this.StartRule       = startRule
    member this.StartNT         = startNT
    member this.NumIsTerminal   = numIsTerminal
//    member this.numIsNonTerminal= numIsNonTerminal
    member this.K               = k
    member this.ChainToNum      = chainToNum
    member this.Rules           = rules

    member this.TakeK(tokens: List<String>) =
        tokens.GetRange(0, min tokens.Count this.K)
    member this.PushRule(rule: int) = 
        Console.WriteLine(rule - 1)
        let r1, r2 = this.Rules.[rule - 1].[1], this.Rules.[rule - 1].[2]
        if not (r2 = 0) then stack.Push(r2)
        stack.Push(r1)

    member this.convertToString(tokens: List<String>) =
        tokens.Aggregate("", fun acc el -> acc + this.TokenToNumber(el).ToString() + "$")




    member this.Parse(tokens: List<String>)  =
        stack.Clear();
        let mutable tokens = tokens
        let mutable answ = true
        let mutable lookAhead = this.TakeK(tokens)
        stack.Push(this.StartNT)
        while lookAhead.Count > 0  do
            if stack.Count = 0
            then answ <- false
                 lookAhead.Clear()
                 tokens.Clear()
            else
                let y = stack.Pop()
                if this.NumIsTerminal(y)
                then if not (y = this.TokenToNumber(lookAhead.[0]))
                     then answ <- false
                          lookAhead.Clear()
                          tokens.Clear()
                     else
                          tokens <- tokens.GetRange(1, tokens.Count - 1)
                          lookAhead <- this.TakeK(tokens)
                else
                    let test = this.convertToString(lookAhead)
                    let chainNum = chainToNum(this.convertToString(lookAhead))
                    if (chainNum = -1) || (table.[y].[chainNum] = 0)
                    then  answ <- false
                          lookAhead.Clear()
                          tokens.Clear()
                    else
                        this.PushRule(table.[y].[chainNum])
        answ <- answ && stack.Count = 0
        answ
