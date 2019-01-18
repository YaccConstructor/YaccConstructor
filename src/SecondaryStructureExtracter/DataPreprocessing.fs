module DataPreprocessing

open System.Text

let getData path = 
    let d = new System.Text.StringBuilder()
    let meta = ref ""
    let lst = new ResizeArray<_>()
    for s in System.IO.File.ReadAllLines(path) do
        if s.[0] = '>'
        then
            if !meta <> ""
            then 
                lst.Add(!meta, d.ToString())
                d.Clear() |> ignore
            meta := s
        else
            d.Append s |> ignore
    lst.Add(!meta, d.ToString())
    lst.ToArray()

let getDataFrom16sBase fastaFile sortNum =
    getData fastaFile 
    //|> Array.filter (fun (meta, _) -> meta.Contains("Bacteria;"))
    |> Array.map (fun (meta, gen) -> (meta,gen))//([for i in 0..sortNum - 1 -> (meta,gen)]))
//                                                                if i = 0 then meta.Split().[0].[1 ..]
//                                                                else meta.Split([|' '; ';'|]).[i]] |> String.concat " ", gen))//(meta.Split().[0].[1 ..], gen))


let getCompleteGenomeData fastaFile =
    let data = (getData fastaFile).[0]
    let metaParts = (fst data).Split(',') |> Array.map (fun s -> s.Trim())
    let id = metaParts.[0].[1 ..] 
    let intervals16s = 
        metaParts.[3].Split()
        |> Array.map (fun s -> let p = s.Split(':') in (int p.[0], int p.[1]))
    (id, snd data, intervals16s)

let removeIntervals (input: string) toRemove =
    if Array.isEmpty toRemove
    then input
    else
        let builder = new StringBuilder()
        let cur = ref 0
        toRemove
        |> Array.iter 
               (fun (i, j) -> builder.Append input.[!cur .. i] |> ignore; cur := j)
        builder.Append(input.[!cur ..]).ToString()