module Yard.Generators.GLL.Printer

open System.Collections.Generic
open Yard.Generators.Common.FSA
open Yard.Generators.Common.FSA.Common

let printGLL (fsa : FSA)
             (outFileName : string)
             (tokenType : Map<string,string option>)
             moduleName
             light 
             isAbstract = 
    let dummyPos = char 0
    let res = new System.Text.StringBuilder()
    let nextInt = ref fsa.States.Length
    //let eps = ref -1
    let stateTokenNewState = new ResizeArray<_>()
    let termToInt = new Dictionary<string,int>()
    
    let inline pack state token =
        if (int state < 65536) && (int token - fsa.States.Length < 65536)
        then int( (int state <<< 16) ||| (token - fsa.States.Length) )
        else failwith "State or token is greater then 65535!!"

    let println (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append "\n" |> ignore) x
    let print (x : 'a) =
        Printf.kprintf (res.Append >> ignore) x
    
    let printHeaders () =
        let fileName = outFileName.Substring(0, outFileName.IndexOf("."))
        //println "%s" <| getPosFromSource fullPath dummyPos (defaultSource output)
        println "module %s"
        <|  match moduleName with
            | "" -> "GLL.Parser" + fileName
            | s -> s
        if not light then
            println "#light \"off\""
        println "#nowarn \"64\";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type"

        println "open Yard.Generators.GLL"
        println "open Yard.Generators.GLL.ParserCommon"
        println "open AbstractAnalysis.Common"

    let printToken () = 
        let defaultType = tokenType.TryFind "_"
        println "type Token ="
        for token in termToInt.Keys do
            let type' =
                match tokenType.TryFind token with
                | Some t -> t
                | None ->
                    match defaultType with
                    | Some t -> t
                    | None -> failwithf "Type of token %s in not defined" token
            println "    | %s%s" token
            <|  match type' with
                | None -> ""
                | Some s -> " of (" + s + ")"

    let printTokenToNumber () = 
        println "let tokenToNumber = function"
        for tokenNumber in termToInt do
            println "    | %s() -> %i" tokenNumber.Key tokenNumber.Value

    let printStateToNontermName (sortedStateToNontermName : seq<KeyValuePair<'a, string>>) () = 
        println "let stateToNontermName = function"
        for tokenNumber in termToInt do
            println "    | %i -> \"%s\"" tokenNumber.Value tokenNumber.Key
        
        for numberNonterm in sortedStateToNontermName do
            println "    | %i -> \"%s\"" numberNonterm.Key numberNonterm.Value
        println "    | _ -> \"\""
    
    let printNumOfAnyState numOfAnyState () =
        println "let private numOfAnyState = %i<positionInGrammar>" numOfAnyState

    let printNumIsTerminal () =
        println "let private numIsTerminal = function"
        for i in termToInt.Values do
            println "    | %i -> true" i
        println "    | _ -> false"

    let printStateAndTokenToNewState () =
        println "let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<positionInGrammar>>()"
        for state, token, newState in stateTokenNewState do
            println "stateAndTokenToNewState.Add(%i, %i<positionInGrammar>)" (pack state token) newState

    let printState (state:(string * int<positionInGrammar>) []) isFirst isLast =
        let prefix = if isFirst then "  [|" else "    "
        let postfix = if isLast then " |]" else ";"

        let printEdge (str,state) isFirst isLast = 
            let prefix = if isFirst then "[|" else ""
            let postfix = if isLast then "|]" else ";"
            print "%s%s<positionInGrammar>,%s<positionInGrammar>%s" prefix str (state.ToString()) postfix
                    
        print "%s" prefix

        if state.Length = 0
        then
            print "[||]"
        else
            state
            |> Array.iteri (fun i edge -> printEdge edge (i = 0) (i = state.Length-1))

        println "%s" postfix

    let printOutNonterms fsaStatesOutNonterms () =
        println "let private outNonterms ="
        fsaStatesOutNonterms    
        |> List.iteri (fun i state -> printState state (i = 0) (i = fsaStatesOutNonterms.Length-1))

    let printStartState () = 
        println "let private startState = %i<positionInGrammar>" fsa.StartState

    let printFinalStates () =
        println "let private finalStates ="
        let printState state isFirst isLast = 
            let prefix = if isFirst then "  new System.Collections.Generic.HashSet<int<positionInGrammar>>(\n     [|" else "       "
            let postfix = if isLast then "|])" else ";"

            println "%s%i<positionInGrammar>%s" prefix state postfix

        let i = ref 0
        for state in fsa.FinalStates do
            printState state (!i = 0) (!i = fsa.FinalStates.Count - 1)
            incr i
        
    let printNontermCount () =
        println "let private nontermCount = %i" fsa.NontermCount
    
    let printParser () =
        println "let parserSource = new FSAParserSourceGLL (outNonterms, startState, finalStates, nontermCount, numIsTerminal, stateToNontermName, numOfAnyState, stateAndTokenToNewState)"

    let printFun isAbstract () =
        if isAbstract
        then
            println "let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) ="
            println "    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource"
        else
            println "let buildAST (input : seq<int>) ="
            println "    Yard.Generators.GLL.ParserFSA.buildAST parserSource input"
             
    let fsaStatesOutNonterms = 
        fsa.States
        |> Array.mapi (fun i x ->
            x
            |> Array.collect (fun (symbol,state) -> 
                match symbol with
                    | Nonterm s -> [|s.ToString(), state|]
                    | Term s -> 
                        let cond, value = termToInt.TryGetValue s
                        if cond then
                            stateTokenNewState.Add(i, value, state)
                        else
                            termToInt.Add(s,!nextInt)
                            stateTokenNewState.Add(i, !nextInt, state)
                            incr nextInt
                        [||]
                    | Epsilon() -> failwith "Found epsilon edge while print fsa."
                            ))
        |> List.ofSeq

    let sortedStateToNontermName = 
        fsa.StateToNontermName
        |> Seq.sortBy (fun x -> x.Key)

    let numOfAnyState = 
        sortedStateToNontermName
        |> Seq.tryFind (fun x -> x.Value.Equals("any"))
        |> (fun x -> 
            match x with
            | Some i -> i.Key
            | _ -> -1<positionInGrammar>)
    
    let printItem printer = 
        printer ()
        println ""

    printItem printHeaders
    printItem printToken
    printItem printTokenToNumber
    printItem (printStateToNontermName sortedStateToNontermName)
    printItem (printNumOfAnyState numOfAnyState)
    printItem printNumIsTerminal
    printItem printStateAndTokenToNewState
    printItem (printOutNonterms fsaStatesOutNonterms)
    printItem printStartState
    printItem printFinalStates
    printItem printNontermCount
    printItem printParser
    //printItem (printFun isAbstract)

    res