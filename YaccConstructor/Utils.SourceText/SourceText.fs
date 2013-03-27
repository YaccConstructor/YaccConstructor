module Yard.Utils.SourceText

open Microsoft.FSharp.Text.Lexing

type Trinity = {Id : int; Column : int; Line : int}



let private constId = 17
let private constColumn = 17
let private constLine = 64 - constId - constColumn



let Pack id column line = ((uint64 id) <<< constColumn + constLine) ||| 
                          ((uint64 column) <<< constLine) ||| 
                          (uint64 line)

let RePackId (trinity : uint64) = int (trinity >>> constColumn + constLine)
let RePackColumn (trinity : uint64) = int ((trinity <<< constId) >>> constId + constLine)
let RePackLine (trinity : uint64) = int ((trinity <<< constId + constColumn) >>> constId + constColumn)

let RePack (trinity : uint64) = 
    let id = RePackId trinity
    let column = RePackColumn trinity
    let line = RePackLine trinity
    let (res : Trinity) = {Id = id; Column = column; Line = line}
    res
    

[<Struct>]
type SourceRange =
    val start : uint64
    val end_ : uint64
    new (start,end_) = { start = start; end_ = end_ }
    member inline x.Start = x.start
    member inline x.End = x.end_
    //static member inline Empty = new SourceRange(Position.Empty, Position.Empty)
    static member inline ofTuple(startPos: Position, endPos: Position) = 
        new SourceRange(Pack 0 startPos.Column startPos.Line, Pack 0 endPos.Column endPos.Line)
    override this.ToString() =
        sprintf "(%s) - (%s)"
            (this.Start.ToString())            
            (this.End.ToString())           

[<Struct>]
type SourceText =
    val text:string
    val range:SourceRange
    new (text, range) = { text = text; range = range }
    static member inline ofTuple(text, range) = new SourceText(text,SourceRange.ofTuple range)
    //static member inline ofText(text) = new SourceText(text, SourceRange.Empty)
    member inline x.Text = x.text
    member inline x.Range = x.range