//  Reports.fs 
//  contains stuff tor reports generation.
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Reports

type Row = List<string>

[<Struct>]
type Table = 
    val mutable Rows: ResizeArray<Row>
    member x.AsWiki =
        let row r = "|" + r + "|"
        x.Rows|> Seq.map (fun r -> r |> String.concat "|" |> row)
        |> String.concat "\n"
    member x.FromSeq (s:#seq<#seq<_>>) = x.Rows <- new ResizeArray<_>(s |> Seq.map (Seq.map string >> List.ofSeq))
    new (s:seq<seq<string>>) = {Rows = new ResizeArray<_>(s |> Seq.map (Seq.map string >> List.ofSeq))}

type Block =
    | RStr of string
    | RTable of Table
    | RList  of List<string>

type Report (outFile) =
    let mutable body = []
    let asWiki () = 
        body
        |> List.map
            (function
             | RStr  s -> s
             | RTable t -> t.AsWiki
             | RList l -> "")
    let write (text:#seq<_>) = 
        System.IO.File.WriteAllLines(outFile, text)
    member x.AsWiki() = asWiki() |> write
    member x.Body 
        with get () = body
        and set v = body <- v 

