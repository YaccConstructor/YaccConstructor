module LexerHelper
          
type FsLexPosition = Microsoft.FSharp.Text.Lexing.Position

[<Struct>]
type Position =
   val line: int
   val column: int
  
   new (lin:int,col:int) = { line = lin; column = col}     
   static member inline ofLexingPosition (pos:FsLexPosition) = new Position(pos.Line, pos.Column)
   static member inline Empty = new Position (-1,-1)
   member x.toLexingPosition () =
       {  Microsoft.FSharp.Text.Lexing.pos_lnum = x.line
          Microsoft.FSharp.Text.Lexing.pos_cnum = x.column
          Microsoft.FSharp.Text.Lexing.pos_fname = ""
          Microsoft.FSharp.Text.Lexing.pos_bol =  x.AbsoluteOffset
       }
   member x.IsEmpty = x = Position.Empty
   member inline x.Line = x.line
   member inline x.Column = x.column
   member x.AbsoluteOffset = 0
   override x.ToString () = sprintf "%d,%d" x.Line x.Column
  

[<Struct>]
type SourceRange =
    val start:Position
    val end_:Position
    new (start,end_) = { start = start; end_ = end_ }
    member inline x.Start = x.start
    member inline x.End = x.end_
    static member inline Empty = new SourceRange(Position.Empty, Position.Empty)
    static member inline ofTuple(startPos:FsLexPosition, endPos:FsLexPosition) = new SourceRange(Position.ofLexingPosition startPos, Position.ofLexingPosition endPos )
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
    static member inline ofText(text) = new SourceText(text, SourceRange.Empty)
    member inline x.Text = x.text
    member inline x.Range = x.range

