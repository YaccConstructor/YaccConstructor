//  Parser.fs contains methods, needed to build an AST
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

module Yard.Generators.RNGLR.Parser

open Yard.Generators.RNGLR
open System.Collections.Generic

type ParseResult =
    | Success of MultiAST
    | Error of int * string

let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let tokenNums =
        tokens
        |> Seq.map parserSource.TokenToNumber
        |> (fun s -> seq {yield! s; yield parserSource.EofIndex})
        |> Seq.toArray
    let tokensCount = tokenNums.Length - 1
    let startState = 0
    if tokensCount = 0 then
        if parserSource.AccStates.[startState] then
            Success <| ASTTyper.createEpsilonTree parserSource.StartRule
        else
            Error (0, "This grammar cannot accept empty string")
    else
        //let mutable stateToAst = new Dictionary<_,_>()
        let mutable stateToVirtex = new Dictionary<_,_>()
        let reductions = new Queue<_>() |> ref
        let mutable pushes : ResizeArray<_> = new ResizeArray<_>()
        let astNodes = new Dictionary<_,_>()

        let inline addVirtex (pushes : ResizeArray<_>) (dict : Dictionary<_,_>) state num edgeOpt addNonZero =
            if dict.ContainsKey state then dict.[state]
            else
                let v = new Virtex<_,_>(state, num)
                dict.[state] <- v
                if num < tokensCount && parserSource.Gotos.[state].[tokenNums.[num]].IsSome then
                    pushes.Add (v, parserSource.Gotos.[state].[tokenNums.[num]].Value) |> ignore
                for action in parserSource.ZeroReduces.[state].[tokenNums.[num]] do
                    let prod, pos = fst action, snd action
                    (!reductions).Enqueue (v, prod, pos, None)
                    |> ignore
                v
            |> (fun v ->
                    if addNonZero then 
                        for action in parserSource.Reduces.[state].[tokenNums.[num]] do
                            let prod, pos = fst action, snd action
                            (!reductions).Enqueue (v, prod, pos, edgeOpt)
                            |> ignore
                    v)

        // init, by adding the first virtex in the first set
        ignore <| addVirtex pushes stateToVirtex startState 0 None true

        let makeReductions (stateToVirtex : Dictionary<_,_>) pushes num =
            while reductions.Value.Count > 0 do
                let virtex, prod, pos, edgeOpt = reductions.Value.Dequeue()
                let nonTerm = parserSource.LeftSide.[prod]
                let addChildren node (path : MultiAST[]) prod =
                    let astExists = 
                        !node |> List.exists
                            (fun ast -> ast.ruleNumber = prod && ast.children.Equals path)
                    if not astExists then
                        node := {ruleNumber = prod; children = path}::!node
                let handlePath (revPath : MultiAST list) (final : Virtex<_,_>) =
                    let ast = 
                        if astNodes.ContainsKey (nonTerm, snd final.label) then
                            let ast = ref []
                            astNodes.[(nonTerm, snd final.label)] <- ast
                            ast
                        else astNodes.[(nonTerm, snd final.label)]
                    let state = parserSource.Gotos.[fst final.label].[nonTerm].Value
                    let edge = new Edge<_,_>(final, ast)
                    let newVirtex = addVirtex pushes stateToVirtex state num (Some edge) (pos > 0)
                    if not (newVirtex.outEdges |> Seq.exists (fun e -> e.dest.label = final.label)) then
                        newVirtex.addEdge edge
                    if pos > 0 then addChildren ast (revPath |> List.rev |> Array.ofList) prod

                let rec walk length (virtex : Virtex<_,_>) path =
                    if length = 0 then handlePath path virtex
                    else
                        for e in virtex.outEdges do
                            walk (length - 1) e.dest (e.label::path)
                ()
                let epsilonPart =
                    let mutable res = []
                    for i = parserSource.Rules.[prod].Length-1 downto pos+1 do
                        res <- (ASTTyper.createEpsilonTree parserSource.Rules.[prod].[i]) ::res
                    res
                if pos = 0 then handlePath epsilonPart virtex
                else walk (pos - 1) (edgeOpt.Value : Edge<_,_>).dest (edgeOpt.Value.label::epsilonPart)

        let shift num pushes stateToVirtex =
            if num <> tokensCount then
                let newPushes = new ResizeArray<_>()
                let newAstNode = ASTTyper.createTerminalTree tokenNums.[num]
                let newStateToVirtex = new Dictionary<_,_>()
                for push in pushes do
                    let virtex, state = fst push, snd push
                    let edge = new Edge<_,_>(virtex, newAstNode)
                    let newVirtex = addVirtex newPushes newStateToVirtex state (num + 1) (Some edge) true
                    newVirtex.addEdge edge
                newPushes, newStateToVirtex
            else
                null, null
        let mutable errorIndex = -1
        for i = 0 to tokensCount do
            if errorIndex = -1 then
                if stateToVirtex.Count = 0 then
                    errorIndex <- i
                else
                    astNodes.Clear()
                    let symbol = tokenNums.[i]
                    let res = shift i pushes stateToVirtex
                    pushes <- fst res
                    stateToVirtex <- snd res
                    ()
        if errorIndex <> -1 then Error (errorIndex, "Parse error")
        else
            let res = ref None
            for value in stateToVirtex do
                if parserSource.AccStates.[value.Key] then
                    res := Some <| Success (value.Value.outEdges.[0].label)
            match !res with
            | None -> Error (0, "There is no accepting state")
            | Some res -> res
    |> ignore
    ()