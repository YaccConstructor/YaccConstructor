module LexerHelper

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Yard.Examples.MSParser
open Yard.Utils.SourceText
open Yard.Utils.StructClass

open System

let appendBuf (str:string) = str_buf.Append(str) |> ignore

let getLiteral id (lexbuf : LexBuffer<_>) value =
    let range = 
        SourceRange.ofTuple(new Pair (id,int64 lexbuf.StartPos.AbsoluteOffset * _symbolL)
                               , new Pair(id, int64 lexbuf.EndPos.AbsoluteOffset * _symbolL))
    genLiteral value range.start range.End
        