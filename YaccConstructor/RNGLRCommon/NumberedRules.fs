//  NumberedRules.fs contains methods to transform initial grammar into arrays of numbers
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

namespace Yard.Generators.RNGLR

open Yard.Core.IL.Production;
open Yard.Core.IL;
open Yard.Generators.RNGLR;

type NumberedRules (ruleList : Rule.t<Source.t,Source.t> list, indexator : Indexator) =
    let rules = ruleList |> Array.ofList
    let start =
        rules
        |> Array.findIndex (fun rule -> rule._public)
    let left = rules |> Array.map (fun x -> x.name |> indexator.nonTermToIndex)
    let right =
        let rec transformBody acc (*body*) = function
            | PRef (nTerm,_) -> (*printfn "N %s" <| fst nTerm;*) (indexator.nonTermToIndex <| fst nTerm)::acc
            | PToken token -> (*printfn "T %s" <| fst token;*) (indexator.termToIndex <| fst token)::acc
            | PLiteral lit -> (*printfn "L %s" <| fst lit;*) (indexator.literalToIndex <| fst lit)::acc
            | PSeq (s,_,_) -> List.foldBack (fun x acc -> transformBody acc x.rule) s acc
            | _ -> failwith "Unexpected construction in grammar"
        rules
        |> Array.map
            (fun x ->
                transformBody [] x.body 
                //|> List.rev
                |> Array.ofList )
        //|> (fun x -> printfn "======="; x)
    let rulesWithLeft =
        let result : int list array = Array.create indexator.fullCount []
        for i in 0..rules.Length-1 do
            result.[left.[i]] <- i::result.[left.[i]]
        result
        |> Array.map (List.rev >> Array.ofList)
        
    member this.rulesCount = rules.Length
    member this.startRule = start
    member this.startSymbol = left.[start]
    member this.leftSide num = left.[num]
    member this.rightSide num = right.[num]
    member this.length num = right.[num].Length
    member this.symbol rule pos = right.[rule].[pos]
    member this.rulesWithLeftSide symbol = rulesWithLeft.[symbol]
