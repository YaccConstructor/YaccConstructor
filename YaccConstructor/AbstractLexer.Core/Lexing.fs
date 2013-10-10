namespace AbstractLexer.Core

open System.Collections.Generic
open AbstractLexer.Common
open QuickGraph.Algorithms
open Microsoft.FSharp.Collections
open QuickGraph.Graphviz
open AbstractParsing.Common


type Position<'br> =
        { pos_fname : string;
          pos_lnum : int;
          pos_bol : int;
          pos_cnum : int;
          back_ref: 'br }
        member x.FileName = x.pos_fname
        member x.Line = x.pos_lnum
        member x.Char = x.pos_cnum
        member x.AbsoluteOffset = x.pos_cnum
        member x.StartOfLine = x.pos_bol
        member x.StartOfLineAbsoluteOffset = x.pos_bol
        member x.Column = x.pos_cnum - x.pos_bol
        member pos.NextLine =
            { pos with
                    pos_lnum = pos.Line+1;
                    pos_bol = pos.AbsoluteOffset }
        member pos.EndOfToken(n) = {pos with pos_cnum=pos.pos_cnum + n }
        member pos.AsNewLinePos() = pos.NextLine
        member pos.ShiftColumnBy(by) = {pos with pos_cnum = pos.pos_cnum + by}
        static member Empty =
            { pos_fname="";
              pos_lnum= 0;
              pos_bol= 0;
              pos_cnum=0 
              back_ref = Unchecked.defaultof<'br>}
        static member FirstLine(filename,?br) =
            { pos_fname=filename;
              pos_lnum= 1;
              pos_bol= 0;
              pos_cnum=0;
              back_ref = br}

[<Struct>]
type StateInfo<'a, 'br> =
    val StartV: int
    val AccumulatedString: ResizeArray<'a>
    val Positions: ResizeArray<Position<'br>>
    new (startV, str, positions) = {StartV = startV; AccumulatedString = str; Positions = positions}

[<Struct>]
type State<'a, 'br> =
    val StateID: int
    val AcceptAction: int
    val PreviousV: Option<int>
    val Info: ResizeArray<StateInfo<'a, 'br>>
    new (stateId, acceptAction, info) = {StateID = stateId; AcceptAction = acceptAction; Info = info ; PreviousV = None}
    new (stateId, acceptAction, info, previousV) = 
        {StateID = stateId; AcceptAction = acceptAction; Info = info ; PreviousV = previousV}

type LexBuffer<'char,'br>(inGraph:LexerInputGraph<'br>) =
        let context = new Dictionary<string,obj>(1) in
        let mutable buffer=[||];
        /// number of valid charactes beyond bufferScanStart
        let mutable bufferMaxScanLength=0;
        /// count into the buffer when scanning
        let mutable bufferScanStart=0;
        /// number of characters scanned so far
        let mutable bufferScanLength=0;
        /// length of the scan at the last accepting state
        let mutable lexemeLength=0;
        /// action related to the last accepting state
        let mutable bufferAcceptAction=0;
        let mutable eof = false
        let mutable startPos = Position.Empty
        let mutable endPos = Position.Empty
        let count_pos_line = ref 0
        let g = new LexerInnerGraph<'br>(inGraph)
        let sorted = g.TopologicalSort() |> Array.ofSeq
        let states = Array.init ((Array.max sorted)+1) (fun _ -> new ResizeArray<_>())
        let startState = new State<_,'br>(0,-1, ResizeArray.singleton (new StateInfo<_,'br>(0, new ResizeArray<_>(), new ResizeArray<_>())))
        do states.[g.StartVertex] <- ResizeArray.singleton startState
        let edgesSeq = seq{ for v in sorted do
                                yield g.OutEdges v |> Array.ofSeq
                                  
                            }
                        |> Seq.filter (fun x -> x.Length > 0)

        let discardInput () =
            let keep = Array.sub buffer bufferScanStart bufferScanLength
            let nkeep = keep.Length
            Array.blit keep 0 buffer 0 nkeep;
            bufferScanStart <- 0;
            bufferMaxScanLength <- nkeep
                 
              
        member lexbuf.EndOfScan () : int =
            // Printf.eprintf "endOfScan, lexBuffer.lexemeLength = %d\n" lexBuffer.lexemeLength;
            if bufferAcceptAction < 0 then
                failwith "unrecognized input"

            // Printf.printf "endOfScan %d state %d on unconsumed input '%c' (%d)\n" a s (Char.chr inp) inp;
            // Printf.eprintf "accept, lexeme = %s\n" (lexeme lexBuffer);
            lexbuf.StartPos <- endPos;
            lexbuf.EndPos <- endPos.EndOfToken(lexbuf.LexemeLength);
            bufferAcceptAction

        member lexbuf.StartPos
           with get() = startPos
           and set(b) = startPos <- b
           
        member lexbuf.EndPos
           with get() = endPos
           and set(b) = endPos <- b

        member lexbuf.Lexeme = Array.sub buffer bufferScanStart lexemeLength
        member lexbuf.LexemeChar(n) = buffer.[n+bufferScanStart]
        
        member lexbuf.BufferLocalStore = (context :> IDictionary<_,_>)
        member lexbuf.LexemeLength with get() : int = lexemeLength and set v = lexemeLength <- v
        member x.CountPosLine = count_pos_line
        member x.States = states
        member x.Edges = edgesSeq
        member x.LastVId = sorted.[sorted.Length-1]
        member internal lexbuf.Buffer with get() : 'char[] = buffer and set v = buffer <- v
        member internal lexbuf.BufferMaxScanLength with get() = bufferMaxScanLength and set v = bufferMaxScanLength <- v
        member internal lexbuf.BufferScanLength with get() = bufferScanLength and set v = bufferScanLength <- v
        member internal lexbuf.BufferScanStart with get() : int = bufferScanStart and set v = bufferScanStart <- v
        member internal lexbuf.BufferAcceptAction with get() = bufferAcceptAction and set v = bufferAcceptAction <- v

//        static member LexemeString(lexbuf:LexBuffer<char>) =
//            new System.String(lexbuf.Buffer,lexbuf.BufferScanStart,lexbuf.LexemeLength)

        member lexbuf.IsPastEndOfStream
           with get() = eof
           and set(b) = eof <- b

        member lexbuf.DiscardInput() = discardInput ()

        member x.BufferScanPos = bufferScanStart + bufferScanLength

        member lexbuf.EnsureBufferSize n =
            if lexbuf.BufferScanPos + n >= buffer.Length then
                let repl = Array.zeroCreate (lexbuf.BufferScanPos + n)
                Array.blit buffer bufferScanStart repl bufferScanStart bufferScanLength;
                buffer <- repl

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
        //startInterpret(lexBuffer)
        //scanUntilSentinel(lexBuffer, initialState)
        1

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
        
    let scanUntilSentinel inp (state:State<_,_>) =
        // Return an endOfScan after consuming the input 
        let a = int accept.[state.StateID]
        let onAccept = if a <> sentinel then a else state.AcceptAction
        // Find the new state
        let snew = lookupUnicodeCharacters (state.StateID,inp)
        snew = sentinel, onAccept, snew

    let tokenize actions (lexbuf:LexBuffer<_,_>) printG =
        //let edgesSeq = edgesSeq |> Array.ofSeq
        let add (edg:LexerEdge<_,_>) (newStt:State<_,_>) =
            match lexbuf.States.[edg.Target]
                  |> ResizeArray.tryFind(fun (x:State<_,_>) -> x.AcceptAction = newStt.AcceptAction && x.StateID = newStt.StateID)
                with
            | Some x ->
                newStt.Info
                |> ResizeArray.iter(
                    fun i -> 
                        if x.Info.Exists(fun j -> j.StartV = i.StartV
                                                  && i.AccumulatedString.Count = j.AccumulatedString.Count
                                                  && ResizeArray.forall2 (=) i.AccumulatedString j.AccumulatedString) 
                           |> not
                        then x.Info.Add i)
            | None -> lexbuf.States.[edg.Target].Add newStt
 
        let mkNewString (edg:LexerEdge<_,'br>) (stt:State<_,_>) = 
            let ch = edg.Label.Value
            let newPos (p:Option<Position<_>>) =
                match p with
                | Some x when x.back_ref = edg.BackRef.Value->
                    {
                            pos_fname = ""
                            pos_lnum = 0
                            pos_bol = 0
                            pos_cnum = x.pos_cnum + 1
                            back_ref = edg.BackRef.Value
                    }

                | _ ->
                    {
                        pos_fname = ""
                        pos_lnum = 0
                        pos_bol = 0
                        pos_cnum = 0
                        back_ref = edg.BackRef.Value
                    }
            

            if stt.Info.Count > 0
            then
                stt.Info
                |> ResizeArray.map 
                    (
                        fun i ->
                            let pos = 
                                if i.Positions.Count = 0 then None else Some i.Positions.[0] 
                                |> newPos
                            new StateInfo<_,'br>((match stt.PreviousV with | Some x -> x | None -> i.StartV)
                            , ResizeArray.concat [i.AccumulatedString; ResizeArray.singleton ch]
                            , ResizeArray.concat [ResizeArray.singleton pos; i.Positions])
                    )
            else 
                new StateInfo<_,'br>((match stt.PreviousV with | Some x -> x | None -> edg.Source), ResizeArray.singleton ch, ResizeArray.singleton (newPos None))
                |> ResizeArray.singleton

        let processToken onAccept (p: StateInfo<_,_>) =
            let s = (new string(p.AccumulatedString |> Array.ofSeq))
            actions onAccept (new string(p.AccumulatedString |> Array.ofSeq)) (p.Positions |> Array.ofSeq |> Array.rev)                          

        let processEdg (edg:LexerEdge<_,_>) stt reduced =
            let acc = new ResizeArray<_>(10) 
            match edg.Label with
            | Some x ->
                let rec go stt =
                    let reduce, onAccept, news = scanUntilSentinel x stt
                    if reduce
                    then
                        for i in stt.Info do
                            if not !reduced then acc.Add(new ParserEdge<_>(i.StartV, edg.Source, processToken onAccept i))
                        reduced := true
                        let newStt = new State<_,_>(0,-1,new ResizeArray<_>())
                        go newStt
                    else 
                        let acc = mkNewString edg stt
                        let newStt = new State<_,_>(news,onAccept,acc,stt.PreviousV)
                        add edg newStt
                go stt
            | None -> //add edg stt
                      let acc = mkNewString edg stt
                      let newStt = new State<_,_>(0,-1,acc,stt.PreviousV)
                      add edg newStt
            acc 

        let res_edg_seq = 
            seq{
                for (edgs:array<LexerEdge<_,_>>) in lexbuf.Edges do
                    for stt in lexbuf.States.[edgs.[0].Source] do
                        let reduced = ref false
                        for edg in edgs do
                            yield! processEdg edg stt reduced
                }
        seq{
            yield! res_edg_seq
            for x in lexbuf.States.[lexbuf.LastVId] do
                for i in x.Info do                        
                    match processToken (int accept.[x.StateID]) i with
                    | Some x -> yield (new ParserEdge<_>(i.StartV,lexbuf.LastVId, Some x))
                    | None -> yield (new ParserEdge<_>(i.StartV,lexbuf.LastVId, None)) 
                    //()
        }
        

    let inputGraph actions (inG:LexerInputGraph<'br>) printG =
        let lexbuf = new LexBuffer<_,'br>(inG) 
        let newEdgs = 
            tokenize actions lexbuf printG
            |> Array.ofSeq
        let res = new ParserInputGraph<_>()
        let r = newEdgs
        res.AddVerticesAndEdgeRange r
        |> ignore
        //EpsClosure.NfaToDfa res
        //|> ignore
        let eps_res = EpsClosure.NfaToDfa res
        eps_res
                          
    // Each row for the Unicode table has format 
    //      128 entries for ASCII characters
    //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
    //      30 entries, one for each UnicodeCategory
    //      1 entry for EOF

    member tables.Interpret stt (lexbuf:LexBuffer<_,_>) =
        0
    member tables.Tokenize(actions,g, ?printG) =
        inputGraph actions g (match printG with Some f -> f | _ -> fun x y -> ())
    static member Create(trans,accept) = new UnicodeTables(trans,accept)

//    let query = "select \" f , " + (if x < 2 then "x" else "y") + "from z"
//    let DB = new MS_DB("")
//    let res = DB.exec query
//    res |> Seq.iter (printfn "%A")