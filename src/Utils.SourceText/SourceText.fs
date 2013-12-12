//   Copyright 2013 YaccConstructor Software Foundation
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


module Yard.Utils.SourceText

open Yard.Utils.StructClass

(*
  For Trinity:
    amount of files limited: 131071 files
    line's length limited: 131071 symbols
    file's length limited: 1073741823 lines (1 billion lines)

  For Pair:
    amount of files limited: 131071 files
    absolute offset limited: 140736414482432 symbols (140 trillion 736 billon)  
*)


let private constId = 17
let private constColumn = 17
let private constLine = 64 - constId - constColumn
let private constOffset = 64 - constId

let Pack (id : int<id>) (line : int<line>) (column : int<symbol>) 
    = ((uint64 id) <<< constColumn + constLine) ||| ((uint64 column) <<< constLine) ||| (uint64 line)
let RePackId (trinity : uint64) = int(trinity >>> constColumn + constLine)
let RePackColumn (trinity : uint64) = int((trinity <<< constId) >>> constId + constLine)
let RePackLine (trinity : uint64) = int((trinity <<< constId + constColumn) >>> constId + constColumn)
let RePack (trinity : uint64) = 
    let id = RePackId trinity
    let column = RePackColumn trinity
    let line = RePackLine trinity
    let res = new Trinity(id * _id, column * _symbol, line * _line)
    res
 
let PackPair (id : int<id>) (offset : int64<symbol>) 
    = ((uint64 id) <<< constOffset) ||| (uint64 offset)
let RePackIdPair (pair : uint64) = int (pair >>> constOffset)
let RePackOffset (pair : uint64) = int64 ((pair <<< constId) >>> constId)
let RePackPair (pair : uint64) =
    let id = RePackIdPair pair
    let offset = RePackOffset pair
    let res = new Pair(id * _id, offset * _symbolL)
    res


[<Struct>]
type SourceRange =
    val start : uint64
    val end_ : uint64
    new (start,end_) = { start = start; end_ = end_ }
    member inline x.Start = x.start
    member inline x.End = x.end_
    //static member inline Empty = new SourceRange(Position.Empty, Position.Empty)
    static member inline ofTuple(start : Pair, end_ : Pair) = 
        new SourceRange(PackPair start.Id start.AbsoluteOffset, PackPair end_.Id end_.AbsoluteOffset)
        //new SourceRange(Pack id startPos.Column startPos.Line, Pack 0 endPos.Column endPos.Line)
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