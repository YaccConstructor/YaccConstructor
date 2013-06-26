//#nowarn "47" // recursive initialization of LexBuffer


namespace AbstractLexer.Core

open System.Collections.Generic
open AbstractLexer.Common
open QuickGraph.Algorithms
open Microsoft.FSharp.Collections

[<Struct>]
type StateInfo<'a> =
    val StartV: int
    val AccumulatedString: 'a
    new (startV, str) = {StartV = startV; AccumulatedString = str}

[<Struct>]
type State<'a> =
    val StateID: int
    val AcceptAction: int
    val Info: ResizeArray<StateInfo<'a>>
    new (stateId:int, acceptAction, info) =  {StateID = stateId; AcceptAction = acceptAction; Info = info}


// REVIEW: This type showed up on a parsing-intensive performance measurement. Consider whether it can be a struct-record later when we have this feature. -jomo
//type Position = 
//    { pos_fname : string;
//        pos_lnum : int;
//        pos_bol : int;
//        pos_cnum : int; }
  //  member x.FileName = x.pos_fname
  //  member x.Line = x.pos_lnum
  //  member x.Char = x.pos_cnum
  //  member x.AbsoluteOffset = x.pos_cnum
  //  member x.StartOfLine = x.pos_bol
  //  member x.StartOfLineAbsoluteOffset = x.pos_bol
  //  member x.Column = x.pos_cnum - x.pos_bol
    
//and [<Sealed>]
//    LexBuffer<'char>(filler: LexBufferFiller<'char>) as this = 
//    let context = new Dictionary<string,obj>(1) in 
//    let mutable buffer=[||];
//    /// number of valid charactes beyond bufferScanStart 
//    let mutable bufferMaxScanLength=0;
//    /// count into the buffer when scanning 
//    let mutable bufferScanStart=0;
//    /// number of characters scanned so far 
//    let mutable bufferScanLength=0;
//    /// length of the scan at the last accepting state 
//    let mutable lexemeLength=0;
//    /// action related to the last accepting state 
//    let mutable bufferAcceptAction=0;
//    let mutable eof = false;
//    
//
//    // Throw away all the input besides the lexeme 
//              
//    let discardInput () = 
//        let keep = Array.sub buffer bufferScanStart bufferScanLength
//        let nkeep = keep.Length 
//        Array.blit keep 0 buffer 0 nkeep;
//        bufferScanStart <- 0;
//        bufferMaxScanLength <- nkeep
//                 
//              
////    member lexbuf.EndOfScan () : int =
////        // Printf.eprintf "endOfScan, lexBuffer.lexemeLength = %d\n" lexBuffer.lexemeLength;
////        if bufferAcceptAction < 0 then 
////            failwith "unrecognized input"
////
////        //  Printf.printf "endOfScan %d state %d on unconsumed input '%c' (%d)\n" a s (Char.chr inp) inp;
////        //   Printf.eprintf "accept, lexeme = %s\n" (lexeme lexBuffer); 
////        lexbuf.StartPos <- endPos;
////        lexbuf.EndPos <- endPos.EndOfToken(lexbuf.LexemeLength);
////        bufferAcceptAction
////
//
//
//    member lexbuf.Lexeme         = Array.sub buffer bufferScanStart lexemeLength
//    member lexbuf.LexemeChar(n)  = buffer.[n+bufferScanStart]
//        
//    member lexbuf.BufferLocalStore = (context :> IDictionary<_,_>)
//    member lexbuf.LexemeLength        with get() : int = lexemeLength    and set v = lexemeLength <- v
//    member internal lexbuf.Buffer              with get() : 'char[] = buffer              and set v = buffer <- v
//    member internal lexbuf.BufferMaxScanLength with get() = bufferMaxScanLength and set v = bufferMaxScanLength <- v
//    member internal lexbuf.BufferScanLength    with get() = bufferScanLength    and set v = bufferScanLength <- v
//    member internal lexbuf.BufferScanStart     with get() : int = bufferScanStart     and set v = bufferScanStart <- v
//    member internal lexbuf.BufferAcceptAction  with get() = bufferAcceptAction  and set v = bufferAcceptAction <- v
//
//
//    static member LexemeString(lexbuf:LexBuffer<char>) = 
//        new System.String(lexbuf.Buffer,lexbuf.BufferScanStart,lexbuf.LexemeLength)
//
//    member lexbuf.IsPastEndOfStream 
//        with get() = eof
//        and  set(b) =  eof <- b
//
//    member lexbuf.DiscardInput() = discardInput ()
//
//    member x.BufferScanPos = bufferScanStart + bufferScanLength
//
//    member lexbuf.EnsureBufferSize n = 
//        if lexbuf.BufferScanPos + n >= buffer.Length then 
//            let repl = Array.zeroCreate (lexbuf.BufferScanPos + n) 
//            Array.blit buffer bufferScanStart repl bufferScanStart bufferScanLength;
//            buffer <- repl
        
module GenericImplFragments = 
    let startInterpret lexBuffer = ()
//        lexBuffer.BufferScanStart <- lexBuffer.BufferScanStart + lexBuffer.LexemeLength;
//        lexBuffer.BufferMaxScanLength <- lexBuffer.BufferMaxScanLength - lexBuffer.LexemeLength;
//        lexBuffer.BufferScanLength <- 0;
//        lexBuffer.LexemeLength <- 0;
//        lexBuffer.BufferAcceptAction <- -1;

//    let afterRefill (trans: uint16[] array,sentinel,lexBuffer:LexBuffer<_>,scanUntilSentinel,endOfScan,state,eofPos) = 
//        // end of file occurs if we couldn't extend the buffer 
//        if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then  
//            let snew = int trans.[state].[eofPos] // == EOF 
//            if snew = sentinel then 
//                endOfScan()
//            else 
//                if lexBuffer.IsPastEndOfStream then failwith "End of file on lexing stream";
//                lexBuffer.IsPastEndOfStream <- true;
//                // Printf.printf "state %d --> %d on eof\n" state snew;
//                scanUntilSentinel(lexBuffer,snew)
//        else 
//            scanUntilSentinel(lexBuffer, state)

//    let onAccept (lexBuffer:LexBuffer<_>,a) = 
//        lexBuffer.LexemeLength <- lexBuffer.BufferScanLength;
//        lexBuffer.BufferAcceptAction <- a;

open GenericImplFragments

[<Sealed>]
type AsciiTables(trans: uint16[] array, accept: uint16[]) =
    let rec scanUntilSentinel(lexBuffer, state) =
        let sentinel = 255 * 256 + 255 
        // Return an endOfScan after consuming the input 
        let a = int accept.[state] 
        if a <> sentinel then () 
            //onAccept (lexBuffer,a)
        
        // read a character - end the scan if there are no further transitions 
        let inp = 1//int(lexBuffer.Buffer.[lexBuffer.BufferScanPos])
        let snew = int trans.[state].[inp] 
        if snew = sentinel then ()
            //lexBuffer.EndOfScan()
        else 
            //lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
            scanUntilSentinel(lexBuffer, snew)
            
    /// Interpret tables for an ascii lexer generated by fslex. 
    member tables.Interpret(initialState,lexBuffer) = 
        startInterpret(lexBuffer)
        scanUntilSentinel(lexBuffer, initialState)

    static member Create(trans,accept) = new AsciiTables(trans,accept)

[<Sealed>]
type UnicodeTables(trans: uint16[] array, accept: uint16[]) = 
    let sentinel = 255 * 256 + 255 
    let numUnicodeCategories = 30 
    let numLowUnicodeChars = 128 
    let numSpecificUnicodeChars = (trans.[0].Length - 1 - numLowUnicodeChars - numUnicodeCategories)/2
    let lookupUnicodeCharacters (state,inp) = 
        let inpAsInt = int inp
        // Is it a fast ASCII character?
        if inpAsInt < numLowUnicodeChars then 
            int trans.[state].[inpAsInt]
        else 
            // Search for a specific unicode character
            let baseForSpecificUnicodeChars = numLowUnicodeChars
            let rec loop i = 
                if i >= numSpecificUnicodeChars then 
                    // OK, if we failed then read the 'others' entry in the alphabet,
                    // which covers all Unicode characters not covered in other
                    // ways
                    let baseForUnicodeCategories = numLowUnicodeChars+numSpecificUnicodeChars*2
                    let unicodeCategory = System.Char.GetUnicodeCategory(inp)
                    //System.Console.WriteLine("inp = {0}, unicodeCategory = {1}", [| box inp; box unicodeCategory |]);
                    int trans.[state].[baseForUnicodeCategories + int32 unicodeCategory]
                else 
                    // This is the specific unicode character
                    let c = char (int trans.[state].[baseForSpecificUnicodeChars+i*2])
                    //System.Console.WriteLine("c = {0}, inp = {1}, i = {2}", [| box c; box inp; box i |]);
                    // OK, have we found the entry for a specific unicode character?
                    if c = inp
                    then int trans.[state].[baseForSpecificUnicodeChars+i*2+1]
                    else loop(i+1)
                
            loop 0
    let eofPos    = numLowUnicodeChars + 2*numSpecificUnicodeChars + numUnicodeCategories 
        
    let rec scanUntilSentinel inp (state:State<_>) =
        // Return an endOfScan after consuming the input 
        let a = int accept.[state.StateID]
        let onAccept = if a <> sentinel then a else state.AcceptAction
                
        // Find the new state
        let snew = lookupUnicodeCharacters (state.StateID,inp)                
        snew = sentinel, onAccept, snew

    let tokenize actions (inG:LexerInputGraph<_>) =
        let g = new LexerInnerGraph<_>(inG)
        let newEdges = new ResizeArray<_>()
        let states = new Dictionary<_,_>(g.VertexCount)
        let startState = new State<_>(0,-1,new ResizeArray<_>([|new StateInfo<_>(0,new ResizeArray<_>())|]))
        states.Add(g.StartVertex, new ResizeArray<_>([startState]))
        let sorted = g.TopologicalSort() |> Array.ofSeq
        let filterStates v =
            states.[v]
            |> Seq.groupBy (fun x -> x.AcceptAction,x.StateID)
            |> Seq.map 
                (fun (gn,gv) -> 
                    let a = gv |> Array.ofSeq
                    a.[1..] |> Array.iter (fun x -> a.[0].Info.AddRange x.Info)
                    a.[0])
            |> fun s -> states.[v] <- ResizeArray.ofSeq s

        for v in sorted do
           let reduced = ref false
           for e in g.OutEdges v do
            printfn "%A" e.Label
            let ch = e.Label
            let add (newStt:State<_>) =
                if states.ContainsKey e.Target
                then
                    match states.[e.Target] 
                          |> ResizeArray.tryFind(fun x -> x.AcceptAction = newStt.AcceptAction && x.StateID = newStt.StateID)
                      with
                    | Some x -> x.Info.AddRange newStt.Info
                    | None -> states.[e.Target].Add newStt
                else states.Add(e.Target,new ResizeArray<_>([newStt]))            
            for stt in states.[v] do                
                match ch with
                | Some x ->
                    let rec go stt =
                        let reduce, onAccept, news = scanUntilSentinel x stt
                        if reduce
                        then
                            stt.Info
                            |> ResizeArray.iter
                                (fun (i:StateInfo<_>) -> 
                                    new string(i.AccumulatedString |> Array.ofSeq)
                                    |> actions onAccept  
                                    |> printfn "%A")

                            if not !reduced
                            then
                                let newStt = new State<_>(0,-1,new ResizeArray<_>())                            
                                go newStt
                            reduced := true
                        else 
                            let acc = 
                                if stt.Info.Count > 0
                                then
                                    stt.Info
                                    |> ResizeArray.map (fun i -> new StateInfo<_>(i.StartV, ResizeArray.concat [i.AccumulatedString; new ResizeArray<_>([ch.Value])]))
                                else new ResizeArray<_>([new StateInfo<_>(v, new ResizeArray<_>([ch.Value]))])
                            (*if not !reduced
                            then*)
                            let newStt = new State<_>(news,onAccept,acc)
                            add newStt
                    go stt
                | None -> ()

        let ac = states.[sorted.[sorted.Length-1]] 
        ac
        |> ResizeArray.iter(
            fun x ->
                printfn "%A" x.AcceptAction
                printfn "%A" x.StateID
                x.Info
                |> ResizeArray.iter
                    (fun (i:StateInfo<_>) ->                        
                        new string(i.AccumulatedString |> Array.ofSeq)
                        |> actions (if x.AcceptAction > -1 then x.AcceptAction else int accept.[x.StateID])
                        |> printfn "%A"))        
                          
    // Each row for the Unicode table has format 
    //      128 entries for ASCII characters
    //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
    //      30 entries, one for each UnicodeCategory
    //      1 entry for EOF
        

    member tables.Tokenize actions g =
        tokenize actions g

    static member Create(trans,accept) = new UnicodeTables(trans,accept)

