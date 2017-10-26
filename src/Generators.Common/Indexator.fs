//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

namespace Yard.Generators.Common

open Yard.Core.IL

type Indexator (ruleList : Rule<Source,Source> list, caseSensitive) =
    let unique s = s |> Set.ofSeq |> Array.ofSeq
    let connect x = 
        let dict = x |> Array.mapi (fun i n -> n, i) |> dict
        (fun nTerm -> dict.[nTerm])
        , (fun i -> x.[i])
        , x.Length

    let rules = ruleList |> Array.ofList

    let terms, literals =
        let rec collectTermsAndLits (accTerms, accLiterals) (*body*) = function
            | PRef _ | PMetaRef _ -> (accTerms, accLiterals)
            | PSeq (s, _, _) ->
                s
                |> List.map (fun e -> e.rule)
                |> List.fold collectTermsAndLits (accTerms, accLiterals)
            | PLiteral lit -> accTerms, (Indexator.transformLiteral caseSensitive lit.text)::accLiterals
            | PToken token -> token.text::accTerms, accLiterals
            | x -> failwithf "Unexpected construction %A in grammar" x
                    
        rules
        |> Array.map (fun rule -> rule.body)
        |> Array.fold collectTermsAndLits (["RNGLR_EOF"], [])
        |> (fun (x, y) -> unique x, unique y)

    let nonTermsConnect = 
        rules
        |> Array.map (fun x -> x.name.text)
        |> Array.append ([|"error"|])
        |> unique
        |> connect

    let termsConnect = connect terms
    let literalsConnect = connect literals
    
    let nonTermsShift = 0
    let (_,_,termsShift) = nonTermsConnect
    let literalsShift = termsShift + (let (_,_,x) = termsConnect in x)
    let _eofIndex = (let (x,_,_) = termsConnect in x "RNGLR_EOF") + termsShift
    
    let _errorIndex = let (x,_,_) = nonTermsConnect in x "error"
    let _getLiteralName i =
        let (lit:string) = Indexator.sub literalsShift (Indexator.snd literalsConnect) i
        let replacementDict =
            [
                '.', "dot"
                ',', "comma"
                ':', "semi"
                ';', "colon"
                '+', "plus"
                '-', "minus"
                '*', "star"
                '<', "less"
                '>', "more"
                '=', "equal"
                '/', "slash"
                '&', "and"
                '|', "or"
                '?', "question"
                '$', "dollar"
                '[', "left_square_bracket"
                ']', "right_square_bracket"
                '(', "left_bracket"
                ')', "right_bracket"
                '!', "not"
                '~', "tilda"
                '#', "sharp"
                '%', "percent"
                '^', "hat"
                '{', "left_figure_bracket"
                '}', "right_figure_bracket"
                '\\', "reverse_slash"
                '`', "reverse_quate"
                ''', "quate"
                '?', "number"
            ]
            |> dict

        lit
        |> Seq.mapi  
            (fun i ch ->
                let exist,v = replacementDict.TryGetValue(ch)
                if exist
                then
                    if i = 0 
                    then v + "_"
                    elif i = lit.Length - 1
                    then "_" + v
                    else "_" + v + "_"
                else string ch
            )
        |> String.concat ""
    let _getLiteralName i =
        let (lit:string) = Indexator.sub literalsShift (Indexator.snd literalsConnect) i
        let replacementDict =
            [
                '.', "dot"
                ',', "comma"
                ':', "semi"
                ';', "colon"
                '+', "plus"
                '-', "minus"
                '*', "star"
                '<', "less"
                '>', "more"
                '=', "equal"
                '/', "slash"
                '&', "and"
                '|', "or"
                '?', "question"
                '$', "dollar"
                '[', "left_square_bracket"
                ']', "right_square_bracket"
                '(', "left_bracket"
                ')', "right_bracket"
                '!', "not"
                '~', "tilda"
                '#', "sharp"
                '%', "percent"
                '^', "hat"
                '{', "left_figure_bracket"
                '}', "right_figure_bracket"
                '\\', "reverse_slash"
                '`', "reverse_quate"
                ''', "quate"
                '?', "number"
            ]
            |> dict

        lit
        |> Seq.mapi  
            (fun i ch ->
                let exist,v = replacementDict.TryGetValue(ch)
                if exist
                then
                    if i = 0 
                    then v + "_"
                    elif i = lit.Length - 1
                    then "_" + v
                    else "_" + v + "_"
                else string ch
            )
        |> String.concat ""

    static member inline private fst (x,_,_) = x
    static member inline private snd (_,x,_) = x
    static member inline private trd (_,_,x) = x

    static member inline private add value f x = (f x) + value
    static member inline private sub value f x = f (x - value)

    static member transformLiteral caseSensitive (lit : string) =
        if caseSensitive then lit
        else lit.ToLower()

    member this.nonTermToIndex nt = Indexator.add nonTermsShift (Indexator.fst nonTermsConnect) nt
    member this.indexToNonTerm i = Indexator.sub nonTermsShift (Indexator.snd nonTermsConnect) i
    member this.nonTermCount = Indexator.trd nonTermsConnect
    member this.isNonTerm index = index >= 0 && index < this.nonTermCount

    member this.termToIndex t = Indexator.add termsShift (Indexator.fst termsConnect) t
    member this.indexToTerm i = Indexator.sub termsShift (Indexator.snd termsConnect) i
    member this.termCount = Indexator.trd termsConnect
    member this.termsStart = termsShift
    member this.termsEnd = termsShift + this.termCount - 1

    member this.literalToIndex lit = Indexator.add literalsShift (Indexator.fst literalsConnect) lit
    member this.indexToLiteral i = Indexator.sub literalsShift (Indexator.snd literalsConnect) i
    member this.literalsCount = Indexator.trd literalsConnect
    member this.getLiteralName i = _getLiteralName i
    member this.literalsStart = literalsShift
    member this.literalsEnd = literalsShift + this.literalsCount - 1

    member this.fullCount = (Indexator.trd literalsConnect) + (Indexator.trd termsConnect) + (Indexator.trd nonTermsConnect)
    member this.eofIndex = _eofIndex
    member this.errorIndex = _errorIndex