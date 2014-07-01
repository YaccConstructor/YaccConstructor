module LexerHelper

open AbstractLexer.Core
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open JSON.Parser
open Yard.Utils.SourceText
open Yard.Utils.StructClass

open System

//let appendBuf (str:string) = str_buf.Append(str) |> ignore

let getLiteral brs value =
    genLiteral value (value,brs)
        