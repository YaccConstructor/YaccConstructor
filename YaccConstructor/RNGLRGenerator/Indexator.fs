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
            | PLiteral lit -> accTerms, lit.text::accLiterals
            | PToken token -> token.text::accTerms, accLiterals
            | x -> failwithf "Unexpected construction %A in grammar" x
                    
        rules
        |> Array.map (fun rule -> rule.body)
        |> Array.fold collectTermsAndLits (["EOF"], [])
        |> (fun (x, y) -> unique x, unique y)

    let nonTermsConnect = 
        rules
        |> Array.map (fun x -> x.name.text)
        //|> Array.append ([|"error"|])
        |> unique
        |> connect
    let termsConnect = connect terms
    let literalsConnect = connect literals
    
    let nonTermsShift = 0
    let (_,_,termsShift) = nonTermsConnect
    let literalsShift = termsShift + (let (_,_,x) = termsConnect in x)
    let _eofIndex = (let (x,_,_) = termsConnect in x "EOF") + termsShift

    static member inline private fst (x,_,_) = x
    static member inline private snd (_,x,_) = x
    static member inline private trd (_,_,x) = x

    static member inline private add value f x = (f x) + value
    static member inline private sub value f x = f (x - value)

    member this.nonTermToIndex nt = Indexator.add nonTermsShift (Indexator.fst nonTermsConnect) nt
    member this.indexToNonTerm i = Indexator.sub nonTermsShift (Indexator.snd nonTermsConnect) i
    member this.nonTermCount = Indexator.trd nonTermsConnect

    member this.termToIndex t = Indexator.add termsShift (Indexator.fst termsConnect) t
    member this.indexToTerm i = Indexator.sub termsShift (Indexator.snd termsConnect) i
    member this.termCount = Indexator.trd termsConnect
    member this.termsStart = termsShift
    member this.termsEnd = termsShift + this.termCount - 1

    member this.literalToIndex lit = Indexator.add literalsShift (Indexator.fst literalsConnect) lit
    member this.indexToLiteral i = Indexator.sub literalsShift (Indexator.snd literalsConnect) i
    member this.literalsCount = Indexator.trd literalsConnect
    member this.literalsStart = literalsShift
    member this.literalsEnd = literalsShift + this.literalsCount - 1

    member this.fullCount = (Indexator.trd literalsConnect) + (Indexator.trd termsConnect) + (Indexator.trd nonTermsConnect)
    member this.eofIndex = _eofIndex
