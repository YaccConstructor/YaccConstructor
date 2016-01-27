// (c) Microsoft Corporation 2005-2009.  

module internal FSharp.PowerPack.FsLex.Driver 

open FSharp.PowerPack.FsLex
open FSharp.PowerPack.FsLex.AST
open FSharp.PowerPack.FsLex.Parser
open Printf
open Internal.Utilities
open Internal.Utilities.Text.Lexing
open System
open System.Collections.Generic
open System.IO
open QuickGraph.FST.GraphBasedFst
open Microsoft.FSharp.Collections 
open QuickGraph
open QuickGraph.FSA.GraphBasedFsa

//------------------------------------------------------------------
// This code is duplicated from Microsoft.FSharp.Compiler.UnicodeLexing

type Lexbuf =  LexBuffer<char>

/// Standard utility to create a Unicode LexBuffer
///
/// One small annoyance is that LexBuffers and not IDisposable. This means 
/// we can't just return the LexBuffer object, since the file it wraps wouldn't
/// get closed when we're finished with the LexBuffer. Hence we return the stream,
/// the reader and the LexBuffer. The caller should dispose the first two when done.
let UnicodeFileAsLexbuf (filename,codePage : int option) : FileStream * StreamReader * Lexbuf =
    // Use the .NET functionality to auto-detect the unicode encoding
    // It also presents the bytes read to the lexer in UTF8 decoded form
    let stream  = new FileStream(filename,FileMode.Open,FileAccess.Read,FileShare.Read) 
    let reader = 
        match codePage with 
        | None -> new  StreamReader(stream,true)
        | Some n -> new  StreamReader(stream,System.Text.Encoding.GetEncoding(n)) 
    let lexbuf = LexBuffer.FromFunction(reader.Read) 
    lexbuf.EndPos <- Position.FirstLine(filename);
    stream, reader, lexbuf
    
//------------------------------------------------------------------
// This is the program proper

let input = ref None
let out = ref None
let inputCodePage = ref None
let light = ref None
let abstractLexer = ref false
let caseInsensitive = ref false

let mutable lexlib = "Microsoft.FSharp.Text.Lexing"

let usage =
  [ ArgInfo ("-o", ArgType.String (fun s -> out := Some s), "Name the output file."); 
    ArgInfo ("--codepage", ArgType.Int (fun i -> inputCodePage := Some i), "Assume input lexer specification file is encoded with the given codepage."); 
    ArgInfo ("--light", ArgType.Unit (fun () ->  light := Some true), "(ignored)");
    ArgInfo ("--light-off", ArgType.Unit (fun () ->  light := Some false), "Add #light \"off\" to the top of the generated file");
    ArgInfo ("--lexlib", ArgType.String (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer table interperter (default Microsoft.FSharp.Text.Lexing)");
    ArgInfo ("--unicode", ArgType.Set unicode, "Produce a lexer for use with 16-bit unicode characters.");
    ArgInfo ("--abstract", ArgType.Unit (fun () -> abstractLexer := true), "Lexer is based on FST.");  
    ArgInfo ("--case-insensitive", ArgType.Unit (fun () -> caseInsensitive := true), "For case insensitive grammar.");
  ]

let _ = ArgParser.Parse(usage, (fun x -> match !input with Some _ -> failwith "more than one input given" | None -> input := Some x), "fslex <filename>")

let outputInt (os: TextWriter) (n:int) = os.Write(string n)

let outputCodedUInt16 (os: #TextWriter)  (n:int) = 
  os.Write n;
  os.Write "us; ";

let sentinel = 255 * 256 + 255 

let lineCount = ref 0
let cfprintfn (os: #TextWriter) fmt = Printf.kfprintf (fun () -> incr lineCount; os.WriteLine()) os fmt

let FileParse (filename:string) =
    let streamReader = new StreamReader(filename)
    let keywords = new Collections.Generic.List<string>()
    let mutable readrules = false
    let strings = new Collections.Generic.List<string>()
    let rules = new Collections.Generic.List<string>()
    while not streamReader.EndOfStream do
        let line = streamReader.ReadLine()
        if line.Contains("rule") then
            readrules <- true
        if not readrules then
            strings.Add(line)
        else
            if line.Contains("\'") || line.Contains("\"") then
                let line = line.Replace("[", "")
                let line = line.Replace("]", "")
                let x = line.Split('\'', '\"')
                let mutable genline = ""
                for i in 0..x.Length-1 do
                    if i%2 = 0 then
                        genline <- String.Concat(genline, x.[i])
                    else
                        let mutable word = x.[i]
                        word <- word.ToLower()
                        if (System.Text.RegularExpressions.Regex.IsMatch(word, "[a-z]+")) then
                            if keywords.Contains(word) then
                                genline <- String.Concat(genline, "(" + word + ")")
                            else
                                genline <- String.Concat(genline, "(" + word + ")")
                                keywords.Add(word)
                                let mutable genregexp = "let " + word + " = "
                                
                                for j = 0 to word.Length-1 do
                                    genregexp <- genregexp + "['" + word.[j].ToString() + "'" + "'" + Char.ToUpper(word.[j]).ToString() + "']"
                                strings.Add(genregexp)
                        else
                            if word.Length = 1 then
                                genline <- String.Concat(genline, "[\'"+  word + "\']")
                            else
                                genline <- String.Concat(genline, "\""+  word + "\"")
                rules.Add(genline)
            else
                rules.Add(line)
    System.IO.File.WriteAllLines("ci" + filename, strings)
    System.IO.File.AppendAllLines("ci" + filename, rules)
    "ci" + filename
        

let main() = 
  try 
    let mutable filename = (match !input with Some x -> x | None -> failwith "no input given") 
    if (!caseInsensitive) then
        filename <- FileParse(filename)
    let domain = if !unicode then "Unicode" else "Ascii" 
    let spec = 
      let stream,reader,lexbuf = UnicodeFileAsLexbuf(filename, !inputCodePage) 
      use stream = stream
      use reader = reader
      try 
          Parser.spec Lexer.token lexbuf 
      with e -> 
          printf "%s(%d,%d): error: %s" filename lexbuf.StartPos.Line lexbuf.StartPos.Column 
              (match e with 
               | Failure s -> s 
               | _ -> e.Message);
          exit 1
    printfn "compiling to dfas (can take a while...)";
    let perRuleData, dfaNodes = AST.Compile spec
    let dfaNodes = dfaNodes |> List.sortBy (fun n -> n.Id) 

    printfn "%d states" dfaNodes.Length;
    printfn "writing output"; 
    
    let output = 
        match !out with 
        | Some x -> x 
        | _ -> 
            Path.Combine (Path.GetDirectoryName filename,Path.GetFileNameWithoutExtension(filename)) + ".fs"
    use os = System.IO.File.CreateText output

    if (!light = Some(false)) || (!light = None && (Path.HasExtension(output) && Path.GetExtension(output) = ".ml")) then
        cfprintfn os "#light \"off\"";
    
    let printLinesIfCodeDefined (code,pos:Position) =
        if pos <> Position.Empty  // If bottom code is unspecified, then position is empty.        
        then 
            cfprintfn os "# %d \"%s\"" pos.Line pos.FileName;
            cfprintfn os "%s" code;

    printLinesIfCodeDefined spec.TopCode
    let code = fst spec.TopCode
    lineCount := !lineCount + code.Replace("\r","").Split([| '\n' |]).Length;
    cfprintfn os "# %d \"%s\"" !lineCount output;
    
    let sizeTable = 
        if !unicode 
        then 
            let countSpecificUnicodeChars = GetSpecificUnicodeChars() |> Array.ofSeq |> Array.length
            numLowUnicodeChars + NumUnicodeCategories + 2 * countSpecificUnicodeChars + 1           
        else 256 + 1

    let tableTransitions = ResizeArray.init dfaNodes.Length (fun _ -> ResizeArray.init sizeTable (fun _ -> -1))

    cfprintfn os "let trans : uint16[] array = ";
    cfprintfn os "    [| ";
    if !unicode then 
        let specificUnicodeChars = GetSpecificUnicodeChars()
        // This emits a (numLowUnicodeChars+NumUnicodeCategories+(2*#specificUnicodeChars)+1) * #states array of encoded UInt16 values
        
        // Each row for the Unicode table has format 
        //      128 entries for ASCII characters
        //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
        //      30 entries, one for each UnicodeCategory
        //      1 entry for EOF
        //
        // Each entry is an encoded UInt16 value indicating the next state to transition to for this input.
        //
        // For the SpecificUnicodeChars the entries are char/next-state pairs.
        let k = ref 0
        for state in dfaNodes do
            cfprintfn os "    (* State %d *)" state.Id;
            fprintf os "     [| ";
            let trans = 
                let dict = new Dictionary<_,_>()
                state.Transitions |> List.iter dict.Add
                dict
            let emit n = 
                if trans.ContainsKey(n) then 
                  outputCodedUInt16 os trans.[n].Id 
                  trans.[n].Id 
                else
                  outputCodedUInt16 os sentinel
                  sentinel
            for i = 0 to numLowUnicodeChars-1 do 
                let c = char i
                tableTransitions.[state.Id].[i] <- emit (EncodeChar c);
            k := numLowUnicodeChars - 1
            for c in specificUnicodeChars do 
                outputCodedUInt16 os (int c); 
                tableTransitions.[state.Id].[(incr k; !k)] <- emit (EncodeChar c);
            for i = 0 to NumUnicodeCategories-1 do 
                tableTransitions.[state.Id].[(incr k; !k)] <- emit (EncodeUnicodeCategoryIndex i);
            tableTransitions.[state.Id].[(sizeTable - 1)] <- emit Eof;
            cfprintfn os "|];"
        done;
    
    else
        // Each row for the ASCII table has format 
        //      256 entries for ASCII characters
        //      1 entry for EOF
        //
        // Each entry is an encoded UInt16 value indicating the next state to transition to for this input.

        // This emits a (256+1) * #states array of encoded UInt16 values
        for state in dfaNodes do
            cfprintfn os "   (* State %d *)" state.Id;
            fprintf os " [|";
            let trans = 
                let dict = new Dictionary<_,_>()
                state.Transitions |> List.iter dict.Add
                dict
            let emit n = 
                if trans.ContainsKey(n) then 
                  outputCodedUInt16 os trans.[n].Id
                  trans.[n].Id 
                else
                  outputCodedUInt16 os sentinel
                  sentinel
            for i = 0 to 255 do 
                let c = char i                
                tableTransitions.[state.Id].[i] <- emit (EncodeChar c);
            tableTransitions.[state.Id].[256] <- emit Eof;
            cfprintfn os "|];"
        done;
    
    cfprintfn os "    |] ";
    
    let actionFunc = ResizeArray.init dfaNodes.Length (fun _ -> new ResizeArray<_>())
    let count = ref 0
    //let actionFunc = ResizeArray.init dfaNodes.Length (fun _ -> -1)
    fprintf os "let actions : uint16[] = [|";
    for state in dfaNodes do
        if state.Accepted.Length > 0 then 
          outputCodedUInt16 os (snd state.Accepted.Head)
          //actionFunc.Add(snd state.Accepted.Head)
          actionFunc.[!count].Add(snd state.Accepted.Head)
        else
          outputCodedUInt16 os sentinel
          //actionFunc.Add(sentinel)
          //actionFunc.[!count].Add(sentinel)
        count := !count + 1
    done;

    cfprintfn os "|]";
    cfprintfn os "let _fslex_tables = %s.%sTables.Create(trans,actions)" lexlib domain;
    
    cfprintfn os "let rec _fslex_dummy () = _fslex_dummy() ";

    // These actions push the additional start state and come first, because they are then typically inlined into later
    // rules. This means more tailcalls are taken as direct branches, increasing efficiency and 
    // improving stack usage on platforms that do not take tailcalls.
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        cfprintfn os "(* Rule %s *)" ident;
        cfprintfn os "and %s %s (lexbuf : %s.LexBuffer<_>) = _fslex_%s %s %d lexbuf" ident (String.Join(" ",Array.ofList args)) lexlib ident (String.Join(" ",Array.ofList args)) startNode.Id;
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        cfprintfn os "(* Rule %s *)" ident;
        cfprintfn os "and _fslex_%s %s _fslex_state lexbuf =" ident (String.Join(" ",Array.ofList args));
        cfprintfn os "  match _fslex_tables.Interpret(_fslex_state,lexbuf) with" ;
        actions |> Seq.iteri (fun i (code,pos) -> 
            cfprintfn os "  | %d -> ( " i;
            cfprintfn os "# %d \"%s\"" pos.Line pos.FileName;
            let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            for line in lines do
                cfprintfn os "               %s" line;
            cfprintfn os "# %d \"%s\"" !lineCount output;
            cfprintfn os "          )")
        cfprintfn os "  | _ -> failwith \"%s\"" ident
    

    cfprintfn os "";
        
    printLinesIfCodeDefined spec.BottomCode
    cfprintfn os "# 3000000 \"%s\"" output;
  
    if !abstractLexer then 
        let ToGraphBasedFst  =
            let resFST = new FST<_,_>()
            resFST.InitState.Add((fst perRuleData.[0]).Id) //we SUGGEST that we have one init state
        
            let stEOF = tableTransitions.[0].[(sizeTable - 1)] 
            new EdgeFST<_,_>(0, stEOF, ( Smbl 65535, Eps)) |> resFST.AddVerticesAndEdge |> ignore
            resFST.FinalState.Add(stEOF)
                         
            for state in dfaNodes do
                for i in 0..(sizeTable - 1) do
                    let st = tableTransitions.[state.Id].[i]
                    if st <> sentinel 
                    then
                        if st <> - 1 
                        then new EdgeFST<_,_>(state.Id, st, (Smbl (if i = sizeTable - 1 then 65535 else i), Eps)) |> resFST.AddVerticesAndEdge |> ignore                       
                    else 
                        if state.Id <> 0
                        then 
                            let action = if actionFunc.[state.Id].Count > 0 then Smbl (actionFunc.[state.Id].[0]) else Eps
                            if tableTransitions.[0].[i] <> sentinel || i = sizeTable - 1
                            then  
                                new EdgeFST<_,_>(state.Id, tableTransitions.[0].[i], (Smbl (if i = sizeTable - 1 then 65535 else i), action)) |> resFST.AddVerticesAndEdge |> ignore
                            
            let getVal printV s = 
                match s with
                | Smbl y -> "Smbl " + printV y
                | Eps -> "Eps"

            let filePathFst = 
                match !out with 
                | Some x -> Path.Combine (Path.GetDirectoryName x,Path.GetFileNameWithoutExtension(x)) + "_Abstract.fs" 
                | _ -> Path.Combine (Path.GetDirectoryName filename,Path.GetFileNameWithoutExtension(filename)) + "_Abstract.fs"

            let fstStream = new StreamWriter(filePathFst)

            let printIfCodeDefined (code,pos:Position) =
                if pos <> Position.Empty  // If bottom code is unspecified, then position is empty.        
                then 
                     fstStream.WriteLine(sprintf "%s" code);

            printIfCodeDefined spec.TopCode

            fstStream.WriteLine("let fstLexer () = ")
            fstStream.WriteLine(sprintf "   let startState = ResizeArray.singleton %i" resFST.InitState.[0]) // one init state...
            fstStream.WriteLine(sprintf "   let finishState = ResizeArray.singleton %i" resFST.FinalState.[0]) //one final state...
            fstStream.WriteLine("   let transitions = new ResizeArray<_>()")
            let alphabet = new HashSet<_>()

            let split length (xs: seq<'T>) =
                let rec loop xs =
                    [
                        yield Seq.truncate length xs |> Seq.toList
                        match Seq.length xs <= length with
                        | false -> yield! loop (Seq.skip length xs)
                        | true -> ()
                    ]
                loop xs

            let printEdg (edge:EdgeFST<_,_>) =
                alphabet.Add(match fst edge.Tag with |Smbl 65535 -> "Smbl 65535" |Smbl y -> ("Smbl "+ (int y |> string))| Eps -> "Eps")  |> ignore
                sprintf  
                    "(%i, (%s, %s), %i)"
                    edge.Source
                    (match fst edge.Tag with |Smbl 65535 -> "Smbl 65535" |Smbl y -> ("Smbl " + (int y |> string))  |Eps -> "Eps") 
                    (getVal (string) (snd edge.Tag)) edge.Target

            split 1000 resFST.Edges
            |> Seq.iter (fun lst -> fstStream.WriteLine("   transitions.AddRange([|" + (lst |> List.map printEdg |> String.concat "; ") + "|])"))

            fstStream.WriteLine("   new FST<_,_>(startState, finishState, transitions)")

            fstStream.WriteLine("\nlet actions () =")
            fstStream.WriteLine("   [|")
        
            let strs = ref ""
            for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
                    actions |> Seq.iteri (fun i (code,pos) -> 
                        strs := !strs + "\n      (fun (gr : FSA<_>) ->\n"  

                        let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
                        for line in lines do
                            strs := !strs +  (sprintf "               %s" line);
                        strs := !strs + ");")
    
            fstStream.WriteLine(!strs)
            fstStream.WriteLine()

            fstStream.WriteLine("   |]\n")
        

            fstStream.WriteLine("\nlet alphabet () = ")            
            let alp = ref ""
            for i in alphabet do
                alp := !alp + (sprintf " %s;" i)
            fstStream.WriteLine(" new HashSet<_>([|" + !alp + "|])\n")
            fstStream.WriteLine("let tokenize eof approximation = Tokenize (fstLexer()) (actions()) (alphabet()) eof approximation")
            fstStream.Close()

        ToGraphBasedFst    
    
  with e -> 
    printf "FSLEX: error FSL000: %s" (match e with Failure s -> s | e -> e.ToString());
    exit 1


let _ = main()
