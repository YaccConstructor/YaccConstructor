namespace Yard.Generators.Common.EBNF

open Yard.Core.IL
open Yard.Core.IL.Production

exception IndexOutOfRange

type GrammarSymbol =
    | Term of string
    | Literal of string
    | NonTerm of string
    | Epsilon

type IndexatorEBNF (ruleList : Rule.t<Source.t,Source.t> list, caseSensitive) =
    let unique s = s |> Set.ofSeq |> Array.ofSeq
    let connect x = 
        let dict = x |> Array.mapi (fun i n -> n, i) |> dict
        (fun nTerm -> dict.[nTerm])
        , (fun i -> x.[i])
        , x.Length

    let rules = ruleList |> Array.ofList

    let terms, literals =
        let rec collectTermsAndLits (accTerms, accLiterals) (*body*) = function (*body*)
            | PRef _ | PMetaRef _ -> (accTerms, accLiterals)
            | PSeq (s, _, _) ->
                s |> List.map (fun e -> e.rule)
                |> List.fold collectTermsAndLits (accTerms, accLiterals)
            | PLiteral lit -> accTerms, (IndexatorEBNF.transformLiteral caseSensitive lit.text)::accLiterals
            | PToken token -> token.text::accTerms, accLiterals
            | PMany x -> collectTermsAndLits (accTerms, accLiterals) x
            | PAlt (x, y) -> 
                let terms, lits = collectTermsAndLits (accTerms, accLiterals) x
                collectTermsAndLits (terms, lits) y
            | POpt x -> collectTermsAndLits (accTerms, accLiterals) x
            | PSome x -> collectTermsAndLits (accTerms, accLiterals) x
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

    let _epsilonIndex = literalsShift + (let (_,_,x) = literalsConnect in x)

    let _getLiteralName i =
        let (lit:string) = IndexatorEBNF.sub literalsShift (IndexatorEBNF.snd literalsConnect) i
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

    member this.nonTermToIndex nt = IndexatorEBNF.add nonTermsShift (IndexatorEBNF.fst nonTermsConnect) nt
    member this.indexToNonTerm i = IndexatorEBNF.sub nonTermsShift (IndexatorEBNF.snd nonTermsConnect) i
    member this.nonTermCount = IndexatorEBNF.trd nonTermsConnect
    member this.isNonTerm index = index >= 0 && index < this.nonTermCount

    member this.termToIndex t = IndexatorEBNF.add termsShift (IndexatorEBNF.fst termsConnect) t
    member this.indexToTerm i = IndexatorEBNF.sub termsShift (IndexatorEBNF.snd termsConnect) i
    member this.termCount = IndexatorEBNF.trd termsConnect
    member this.termsStart = termsShift
    member this.termsEnd = termsShift + this.termCount - 1

    member this.literalToIndex lit = IndexatorEBNF.add literalsShift (IndexatorEBNF.fst literalsConnect) lit
    member this.indexToLiteral i = IndexatorEBNF.sub literalsShift (IndexatorEBNF.snd literalsConnect) i
    member this.literalsCount = IndexatorEBNF.trd literalsConnect
    member this.getLiteralName i = _getLiteralName i
    member this.literalsStart = literalsShift
    member this.literalsEnd = literalsShift + this.literalsCount - 1

    member this.indexToSymbol i =
        if i >= nonTermsShift && i < nonTermsShift + IndexatorEBNF.trd nonTermsConnect then
            NonTerm(this.indexToNonTerm i)
        elif i >= termsShift && i < termsShift + IndexatorEBNF.trd termsConnect then
            Term(this.indexToTerm i)
        elif i >= literalsShift && i < literalsShift + IndexatorEBNF.trd literalsConnect then
            Literal(this.indexToLiteral i)
        elif i = _epsilonIndex then
            Epsilon
        else raise IndexOutOfRange
    
    member this.fullCount = (IndexatorEBNF.trd literalsConnect) + (IndexatorEBNF.trd termsConnect) + (IndexatorEBNF.trd nonTermsConnect)
    member this.eofIndex = _eofIndex
    member this.errorIndex = _errorIndex
    member this.epsilonIndex = _epsilonIndex