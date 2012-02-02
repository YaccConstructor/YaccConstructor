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

