//  States.fs builds graph, related with states of parser
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Generators.RNGLR.Item

open System.Collections.Generic
open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR

type Kernel = int
type Item = Kernel * Set<int>
type TempState = Kernel list
type State = Kernel[] * Set<int>[]

type KernelInterpreter () =
    member this.toKernel (prod,pos) = (prod <<< 16) ||| pos
    member this.incPos kernel = kernel + 1
    member this.getProd kernel = kernel >>> 16
    member this.getPos kernel = kernel <<< 16 >>> 16
    member this.kernelsOfState = fst
    member this.lookAheadsOfState = snd
    member this.symbol (grammar : FinalGrammar) kernel =
        let rule = this.getProd kernel
        let pos = this.getPos kernel
        if grammar.rules.length rule = pos then grammar.indexator.eofIndex
        else grammar.rules.symbol rule pos

type KernelIndexator (grammar : FinalGrammar) =
    let kernels = 
        let kernelInterpreter = new KernelInterpreter()
        let initKernel = kernelInterpreter.toKernel(grammar.startRule, 0)
        let was : bool[] = Array.zeroCreate grammar.indexator.fullCount
        let rec dfs nonTerminal =
            was.[nonTerminal] <- true
            for rule in grammar.rules.rulesWithLeftSide nonTerminal do
                for symbol in grammar.rules.rightSide rule do
                    if (not was.[symbol]) then
                        dfs symbol
                
        dfs grammar.rules.startSymbol
        [|for i in 0..grammar.indexator.fullCount-1 do
            if was.[i] then
                for rule in grammar.rules.rulesWithLeftSide i do
                    for pos = 0 to grammar.rules.length rule - 1 do
                        yield kernelInterpreter.toKernel (rule, pos)
        |]

    let kernelToIdx = kernels |> Array.mapi (fun i x -> (x,i)) |> dict

    member this.indexToKernel index = kernels.[index]
    member this.kernelToIndex kernel = kernelToIdx.Item kernel

let items (grammar : FinalGrammar) (kernelIndexator : KernelIndexator) =
    let nextIndex =
        let num = ref -1
        fun () -> incr num; !num
    let result = new Dictionary<Kernel[], Virtex<int,int>>()
    let kernelInterpreter = new KernelInterpreter()
    let curSymbol kernel = kernelInterpreter.symbol grammar kernel
    let closure (kernels : Kernel[]) =
        let was : bool[] = Array.zeroCreate grammar.indexator.fullCount
        let mutable result = Set.ofArray kernels
        let queue = new Queue<_>()
        let enqueue symbol = 
            if not was.[symbol] then
                was.[symbol] <- true
                queue.Enqueue symbol
        for k in kernels do
            enqueue <| curSymbol k
        while queue.Count <> 0 do
            let nonterm = queue.Dequeue()
            for rule in grammar.rules.rulesWithLeftSide nonterm do
                result <- result.Add <| kernelInterpreter.toKernel (rule,0)
                enqueue <| grammar.rules.symbol rule 0
        Array.ofSeq result |> Array.sort
    let rec dfs initKernels =
        let kernels = initKernels |> closure
        if result.ContainsKey kernels then
            result.Item kernels
        else
            let virtex = new Virtex<int,int>(nextIndex())
            result.Add(kernels, virtex)
            // adding edges
            for i in 0.. grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    let destKernels =
                        [|for k in kernels do
                            if curSymbol k = i then yield kernelInterpreter.incPos k|]
                    if destKernels.Length > 0 then
                        virtex.addEdge(new Edge<_,_>(dfs destKernels, i))
            virtex
    let initKernel = kernelInterpreter.toKernel(grammar.startRule, 0)
    [| initKernel |] |> dfs
(*
let buildLookaheads (grammar : FinalGrammar) =
    let res : Set<int>[] = Array.zeroCreate grammar.indexator.fullCount
    // for each rule we look if we have already visited it
    let was : bool[] = Array.zeroCreate grammar.rules.rulesCount
    // look on all rules with left side equals to this nonTerminal
    let rec dfs nonTerminal symbol =
        res.[nonTerminal] <- Set.add symbol res.[nonTerminal]
        for rule in grammar.rules.rulesWithLeftSide nonTerminal do
            let production = grammar.rules.rightSide rule
            // if we look on rule for the first time, we must collect for each symbol it's follow sets
            if (not was.[rule]) then
                was.[rule] <- true
                for i in 0..grammar.rules.length rule-1 do
                    for s in grammar.followSet.[rule].[i] do
                        if not <| Set.contains s res.[production.[i]] then
                            dfs production.[i] s
            // look on epsilon-tail of rule
            for i in grammar.epsilonTailStart.[rule] - 1 .. grammar.rules.length rule - 1 do
                if not <| Set.contains symbol res.[production.[i]] then
                    dfs production.[i] symbol
                
    dfs grammar.rules.startSymbol grammar.indexator.eofIndex
    ()
*)