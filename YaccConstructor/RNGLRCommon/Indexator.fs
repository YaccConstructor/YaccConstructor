//  Indexator.fs contains methods to transform non-terminal, terminals and literals to numbers
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

open Yard.Core.IL
open Yard.Core.IL.Production

type Indexator (ruleList : Rule.t<Source.t,Source.t> list) =
    let ppp = 
        ruleList
        |> List.iteri
            (fun i rule ->
                let args =
                    rule.args
                    |> List.map (fun x -> "[" + Source.toString x + "]")
                    |> String.concat ""
                printfn "%4d: %s%s = %s" i rule.name args <| rule.body.ToString())
    let unique s = s |> Set.ofSeq |> Array.ofSeq
    let connect x = 
        let dict = x |> Array.mapi (fun i n -> n, i) |> dict
        (fun nTerm -> dict.Item nTerm)
        , (fun i -> x.[i])
        , x.Length

    let rules = ruleList |> Array.ofList

    let terms, literals =
        let rec collectTermsAndLits (accTerms, accLiterals) (*body*) = function (*body*)
            | PRef _ | PMetaRef _ -> (accTerms, accLiterals)
            | PSeq (s, _, _) ->
                s |> List.map (fun e -> e.rule)
                |> List.fold collectTermsAndLits (accTerms, accLiterals)
            | PLiteral lit -> (accTerms, (fst lit)::accLiterals)
            | PToken token -> ((fst token)::accTerms, accLiterals)
            | x -> failwithf "Unexpected construction %A in grammar" x
                    
        rules
        |> Array.map (fun rule -> rule.body)
        |> Array.fold collectTermsAndLits (["EOF"], [])
        |> (fun (x, y) -> unique x, unique y)

    let nonTermsConnect = 
        rules
        |> Array.map (fun x -> x.name)
        //|> Array.append ([|"error"|])
        |> unique
        |> connect
    let termsConnect = connect terms
    let literalsConnect = connect literals
    
    let fst = fun (x,_,_) -> x
    let snd = fun (_,x,_) -> x
    let trd = fun (_,_,x) -> x

    let nonTermsShift = 0
    let termsShift = trd nonTermsConnect
    let literalsShift = termsShift + trd termsConnect

    let add value f = fun x -> (f x) + value
    let sub value f = fun x -> f (x - value)

    member this.nonTermToIndex = add nonTermsShift <| fst nonTermsConnect
    member this.indexToNonTerm = sub nonTermsShift <| snd nonTermsConnect
    member this.nonTermCount = trd nonTermsConnect

    member this.termToIndex = add termsShift <| fst termsConnect
    member this.indexToTerm = sub termsShift <| snd termsConnect
    member this.termCount = trd termsConnect
    member this.termsStart = termsShift
    member this.termsEnd = termsShift + this.termCount - 1

    member this.literalToIndex = add literalsShift <| fst literalsConnect
    member this.indexToLiteral = sub literalsShift <| snd literalsConnect
    member this.literalsCount = trd literalsConnect
    member this.literalsStart = literalsShift
    member this.literalsEnd = literalsShift + this.literalsCount - 1

    member this.fullCount = (trd literalsConnect) + (trd termsConnect) + (trd nonTermsConnect)
    member this.eofIndex = this.termToIndex "EOF"