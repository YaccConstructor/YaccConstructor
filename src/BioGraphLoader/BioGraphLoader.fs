module YC.BIO.BioGraphLoader

open System.IO

module Seq =

    /// Returns a sequence that yields chunks of length n.
    /// Each chunk is returned as a list.
    let split length (xs: seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs

let lodGraph fileWithoutExt =
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let lbls = 
        File.ReadAllLines(fileWithoutExt + lblsExt) 
        |> Seq.split 2
        |> Seq.map (fun a -> a.[0].Trim().TrimStart('>') |> int,a.[1].Trim()) 
    0

