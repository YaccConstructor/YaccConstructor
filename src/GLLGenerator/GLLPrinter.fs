module Yard.Generators.GLL.Printer

open System.Collections.Generic
open Yard.Generators.Common.FSA
open Yard.Generators.Common.FSA.Common
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon

let getGLLparserSource (fsa : FSA)
             (outFileName : string)
             (tokenType : Map<string,string option>)
             moduleName
             light 
             generateToFile =
             //isAbstract = 
    let dummyPos = char 0
    let res = new System.Text.StringBuilder()
    let nextInt = ref fsa.States.Length
    let stateTokenNewState = new ResizeArray<int * int<token>* int<positionInGrammar>>()
    let stringToToken = new Dictionary<string,int<token>>()
    

    let inline pack state (token : int<token>) =
        if (int state < 65536) && (int token - fsa.States.Length < 65536)
        then int( (int state <<< 16) ||| (int token - fsa.States.Length) )
        else failwith "State or token is greater then 65535!!"

    let println (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append "\n" |> ignore) x
    let print (x : 'a) =
        Printf.kprintf (res.Append >> ignore) x

    let printHeaders () =
        let fileName = outFileName.Substring(0, outFileName.IndexOf("."))
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
        for str in stringToToken.Keys do
            let type' =
                match tokenType.TryFind str with
                | Some t -> t
                | None ->
                    match defaultType with
                    | Some t -> t
                    | None -> failwithf "Type of token %s in not defined" str
            println "    | %s%s" str
            <|  match type' with
                | None -> ""
                | Some s -> " of (" + s + ")"
    
    let printStringToToken () = 
        println "let stringToToken = new System.Collections.Generic.Dictionary<_,_>()"
        for tokenNumber in stringToToken do
            println "stringToToken.Add(\"%s\",%i<token>)" tokenNumber.Key tokenNumber.Value

//    let printTokenToNumber () = 
//        if generateToFile then
//            println "let tokenToNumber = new System.Collections.Generic.Dictionary<_,_>()"
//            for tokenNumber in termToInt do
//                println "tokenToNumber.Add(%s _ -> %i" tokenNumber.Key tokenNumber.Value

    let printIntToString (sortedStateToNontermName : seq<KeyValuePair<int<positionInGrammar>, string>>) () = 
        if generateToFile then
            println "let intToString = new System.Collections.Generic.Dictionary<_,_>()"
            for tokenNumber in stringToToken do
                println "intToString.Add(%i,\"%s\")" tokenNumber.Value tokenNumber.Key
        
            for numberNonterm in sortedStateToNontermName do
                println "intToString.Add(%i,\"%s\")" numberNonterm.Key numberNonterm.Value


    
    let printAnyNonterm anyNonterm () =
        println "let private anyNonterm = %i<positionInGrammar>" anyNonterm

    let printTerminalNums () =
        println "let private terminalNums = new System.Collections.Generic.HashSet<_>()"
        for i in stringToToken.Values do
            println "terminalNums.Add(%i<token>) |> ignore" i

    let printStateAndTokenToNewState () =
        println "let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<positionInGrammar>>()"
        for state, token, newState in stateTokenNewState do
            println "stateAndTokenToNewState.Add(%i, %i<positionInGrammar>)" (pack state token) newState

    let printState (state:(int<positionInGrammar> * int<positionInGrammar>) []) isFirst isLast =
        let prefix = if isFirst then "  [|" else "    "
        let postfix = if isLast then " |]" else ";"

        let printEdge (edge,state) isFirst isLast = 
            let prefix = if isFirst then "[|" else ""
            let postfix = if isLast then "|]" else ";"
            print "%s%s<positionInGrammar>,%s<positionInGrammar>%s" prefix (edge.ToString()) (state.ToString()) postfix
                    
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
        |> Array.iteri (fun i state -> printState state (i = 0) (i = fsaStatesOutNonterms.Length-1))

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
    
    let printMultipleInEdges (multipleInEdges : bool []) () =
        println "let private multipleInEdges = "
        print "    [|"
        multipleInEdges
        |> Array.iteri (fun i st -> 
            if st then print "true"
            else print "false"
            if i <> multipleInEdges.Length - 1
            then print "; ")
        println "|]"
     

    let printParser () =
        println "let parserSource = new ParserSourceGLL (outNonterms, startState, finalStates, nontermCount, terminalNums, intToString, anyNonterm, stateAndTokenToNewState,stringToToken, multipleInEdges)"

    let printFun isAbstract () =
        if isAbstract
        then
            println "let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) ="
            println "    Yard.Generators.GLL.AbstractParserWithoutTree.buildAbstract parserSource"
        else
            println "let buildAST (input : seq<int>) ="
            println "    Yard.Generators.GLL.ParserFSA.buildAST parserSource input"
             
    let fsaStatesOutNonterms = 
        fsa.States
        |> Array.mapi (fun i x ->
            x
            |> Array.collect (fun (symbol,state) -> 
                match symbol with
                    | EdgeSymbol.Nonterm s -> [|(int s) * 1<positionInGrammar>, state|]
                    | EdgeSymbol.Term s -> 
                        let cond, value = stringToToken.TryGetValue s
                        if cond then
                            stateTokenNewState.Add(i, value, state)
                        else
                            stringToToken.Add(s,(!nextInt)*1<token>)
                            stateTokenNewState.Add(i, (!nextInt)*1<token>, state)
                            incr nextInt
                        [||]
                    | EdgeSymbol.Epsilon() -> failwith "Found epsilon edge while print fsa."
                            ))

    let sortedStateToNontermName = 
        fsa.StateToNontermName
        |> Seq.sortBy (fun x -> x.Key)

    let anyNonterm = 
        sortedStateToNontermName
        |> Seq.tryFind (fun x -> x.Value.Equals("any"))
        |> (fun x -> 
            match x with
            | Some i -> i.Key
            | _ -> -1<positionInGrammar>)

    let multipleInEdges = 
        let inEdgesCount = Array.zeroCreate fsa.States.Length

        fsa.States
        |> Array.iter (fun edges -> 
            edges
            |> Array.iter (fun (_,nextState) -> 
                inEdgesCount.[int nextState] <- inEdgesCount.[int nextState] + 1))

        inEdgesCount
        |> Array.map (fun x -> x > 1)

    
    let printItem printer = 
        printer ()
        println ""

    if generateToFile
    then
        printItem printHeaders
        printItem printToken
        printItem printStringToToken
//        printItem printTokenToNumber
        printItem (printIntToString sortedStateToNontermName)
        printItem (printAnyNonterm anyNonterm)
        printItem printTerminalNums
        printItem printStateAndTokenToNewState
        printItem (printOutNonterms fsaStatesOutNonterms)
        printItem printStartState
        printItem printFinalStates
        printItem printNontermCount
        printItem (printMultipleInEdges multipleInEdges)
        printItem printParser
    //printItem (printFun isAbstract)

    let terminalNums = new HashSet<_>(stringToToken.Values)
    let stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<positionInGrammar>>()
    for state, token, newState in stateTokenNewState do
        stateAndTokenToNewState.Add((pack state token), newState)

    let intToString = new Dictionary<int, string>()
    
    for tokenNumber in stringToToken do
        intToString.Add(int tokenNumber.Value, tokenNumber.Key)
        
    for numberNonterm in sortedStateToNontermName do
        intToString.Add(int numberNonterm.Key, numberNonterm.Value)

    let parserSource = new ParserSourceGLL(fsaStatesOutNonterms
                                         , fsa.StartState
                                         , fsa.FinalStates
                                         , fsa.NontermCount
                                         , terminalNums
                                         , intToString
                                         , (int anyNonterm)* 1<positionInGrammar>
                                         , stateAndTokenToNewState
                                         , stringToToken
                                         , multipleInEdges)


    res, parserSource