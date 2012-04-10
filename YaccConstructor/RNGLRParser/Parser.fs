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
open Yard.Generators.RNGLR.AST
open System.Collections.Generic

type ParseResult<'TokenType> =
    | Success of MultiAST<'TokenType>
    | Error of int * string

let buildAst<'TokenType when 'TokenType:equality> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let tokensArr = tokens |> Array.ofSeq
    let tokenNums =
        tokens
        |> Seq.map parserSource.TokenToNumber
        |> Seq.takeWhile (fun t -> t <> parserSource.EofIndex)
        |> (fun s -> seq {yield! s; yield parserSource.EofIndex})
        |> Seq.toArray
    let tokensCount = tokenNums.Length - 1
    let startState = 0
    if tokensCount = 0 then
        if parserSource.AccStates.[startState] then
            NonTerm (ref [Epsilon], ref -1) |> Success
        else
            Error (0, "This grammar cannot accept empty string")
    else
        let stateToVirtex = ref <| new Dictionary<_,_>()
        let reductions = ref <| new Queue<_>()
        let pushes = ref <| new ResizeArray<_>()
        let astNodes = new Dictionary<_,_>()

        let vertices = ref []
        let addVirtex state num edgeOpt addNonZero =
            let dict = stateToVirtex.Value
            if dict.ContainsKey state then dict.[state]
            else
                //printfn "v(%d,%d)" state num
                let v = new Virtex<_,_>(state, num)
                dict.[state] <- v
                if num < tokensCount && parserSource.Gotos.[state].[tokenNums.[num]].IsSome then
                    pushes.Value.Add (v, parserSource.Gotos.[state].[tokenNums.[num]].Value) |> ignore
                for prod in parserSource.ZeroReduces.[state].[tokenNums.[num]] do
                    (!reductions).Enqueue (v, prod, 0, None)
                    |> ignore
                vertices := v::!vertices
                v
            |> (fun v ->
                    if addNonZero then 
                        for action in parserSource.Reduces.[state].[tokenNums.[num]] do
                            let prod, pos = fst action, snd action
                            (!reductions).Enqueue (v, prod, pos, edgeOpt)
                            |> ignore
                    v)

        // init, by adding the first virtex in the first set
        ignore <| addVirtex startState 0 None true

        let makeReductions num =
            while reductions.Value.Count > 0 do
                let virtex, prod, pos, edgeOpt = reductions.Value.Dequeue()
                let nonTerm = parserSource.LeftSide.[prod]
                let compareChildren (ast1 : MultiAST<_>[]) (ast2 : MultiAST<_>[]) =
                    let n = ast1.Length
                    if ast2.Length <> n then false
                    else
                        let equalLeaf (x : MultiAST<_>) (y : MultiAST<_>) =
                            match x, y with
                            | Term x, Term y -> true
                            | NonTerm (l1,_), NonTerm (l2,_) ->
                                obj.ReferenceEquals(l1, l2)
                                || List.forall2 (fun (x:AST<_>) (y:AST<_>) -> isEpsilon x && isEpsilon y) !l1 !l2
                            | _ -> false
                        Array.forall2 (fun x y -> obj.ReferenceEquals(x,y) || equalLeaf x y) ast1 ast2

                let inline addEpsilon node nonTerm =
                    let astExists = (getFamily node).Value |> List.exists isEpsilon
                    if not astExists then
                        getFamily node := Epsilon::!(getFamily node)

                let inline addChildren node (path : MultiAST<_>[]) prod =
                    let astExists = 
                        (getFamily node).Value
                        |> List.exists
                            (function
                             | Inner (number,children) -> number = prod && compareChildren children path
                             | _ -> false )
                    if not astExists then
                        (getFamily node) := (Inner (prod, path))::(getFamily node).Value
                let handlePath (path : MultiAST<_> list) (final : Virtex<_,_>) =
                    let ast = 
                        if not <| astNodes.ContainsKey (nonTerm, snd final.label) then
                            let ast = NonTerm (ref [], ref -1)
                            astNodes.[(nonTerm, snd final.label)] <- ast
                            ast
                        else astNodes.[(nonTerm, snd final.label)]
                    let state = parserSource.Gotos.[fst final.label].[nonTerm].Value
                    let edge = new Edge<int*int, MultiAST<_>>(final, ast)
                    //printfn "r %A->%A->%A (prod: %d,%d) " virtex.label final.label (state,num) prod pos
                    let newVirtex = addVirtex state num (Some edge) (pos > 0)
                    if not (newVirtex.outEdges |> Seq.exists (fun e -> e.dest.label = final.label)) then
                        newVirtex.addEdge edge
                    let isAllEpsilon=
                        List.forall
                           (function
                            | Term _ -> false
                            | NonTerm (x,_) -> x.Value |> List.forall isEpsilon)
                    if path = [] || isAllEpsilon path then addEpsilon ast parserSource.LeftSide.[prod]
                    else addChildren ast (path |> Array.ofList) prod

                let rec walk length (virtex : Virtex<_,_>) path =
                    if length = 0 then handlePath path virtex
                    else
                        for e in virtex.outEdges do
                            walk (length - 1) e.dest (e.label::path)
                
                if pos = 0 then
                    handlePath [] virtex
                else 
                    let epsilonPart =
                        let mutable res = []
                        for i = parserSource.Length.[prod] - 1 downto pos do
                            res <- (NonTerm (ref [Epsilon], ref -1)) ::res
                        res
                    walk (pos - 1) (edgeOpt.Value : Edge<_,_>).dest (edgeOpt.Value.label::epsilonPart)

        let shift num =
            if num <> tokensCount then
                let oldPushes = pushes.Value
                let newAstNode = Term tokensArr.[num]
                pushes := new ResizeArray<_>()
                stateToVirtex := new Dictionary<_,_>()
                for push in oldPushes do
                    let virtex, state = fst push, snd push
                    let edge = new Edge<_,_>(virtex, newAstNode)
                    //printfn "p %A" (virtex.label, state)
                    let newVirtex = addVirtex state (num + 1) (Some edge) true
                    newVirtex.addEdge edge
        let mutable errorIndex = -1
        for i = 0 to tokensCount do
            if errorIndex = -1 then
                if stateToVirtex.Value.Count = 0 then
                    errorIndex <- i
                else
                    astNodes.Clear()
                    let symbol = tokenNums.[i]
                    makeReductions i
                    shift i
                    ()
        if errorIndex <> -1 then Error (errorIndex - 1, "Parse error")
        else
            let res = ref None
            printfn "accs: %A" [for i = 0 to parserSource.AccStates.Length-1 do
                                    if parserSource.AccStates.[i] then yield i]
            let addTreeTop res = NonTerm (ref [Inner (parserSource.StartRule, [|res|])], ref -1)
            for value in stateToVirtex.Value do
                printf "%d " value.Key
                if parserSource.AccStates.[value.Key] then
                    res := value.Value.outEdges.[0].label
                           |> addTreeTop |> Success |> Some
            printfn ""
            match !res with
            | None -> Error (tokensCount, "There is no accepting state")
            | Some res -> res
