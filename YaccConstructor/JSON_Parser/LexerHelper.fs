module LexerHelper

open AbstractLexer.Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open JSON.Parser
open Yard.Utils.SourceText
open Yard.Utils.StructClass

open System

//let appendBuf (str:string) = str_buf.Append(str) |> ignore

let getLiteral value =
    let id = 0<id>
    let range = 
        SourceRange.ofTuple(new Pair (id,int64 0 * _symbolL)
                               , new Pair(id, int64 0 * _symbolL))
    genLiteral value range.start range.End
        