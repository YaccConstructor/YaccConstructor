module TblReader

open Microsoft.FSharp.Collections
open System.IO
open System
open System.Collections.Generic

let getTbl path = 
    (*
    ///returns lengths without last column
    let getLengths (line:string) =
        let lengths = ResizeArray<_>()
        let counter = ref 0

        line.ToCharArray()
        |> Array.iter (fun c -> match c with
                                | '-' | '#' -> incr counter
                                | ' ' | '\n' -> lengths.Add !counter
                                                counter := 0
                                | _ -> failwithf "Error in tbl parce. Found %A as a delimeter." c
                                 )
        //if !counter <> 0 then lengths.Add !counter
        lengths |> ResizeArray.toList
    
    let getCells lengths (line:string) =
        
        lengths
        |> List.fold (fun (cells,(line:string)) l -> let subline = line.Substring (0, l)
                                                     if line.Length >= l+1 
                                                     then cells@[subline], line.Substring (l+1)
                                                     else cells@[subline], "" )
            ([],line)
        |> (fun (x,_) -> x)
    *) 
    let lines = File.ReadAllLines(path)
    
    //let lengths = getLengths lines.[1]

    let dataLines = 
        lines
        |> Array.filter (fun (x:string) -> x.StartsWith("#") |> not)

    let data = 
        dataLines
        |> Array.map (fun (line:string) -> line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))

    let filteredData =
        //let filter = [|2;7;8|]
        data
        |> Array.map (fun line -> let res = ResizeArray<_>()
                                  line.[2], (line.[7] |> int) - 1, (line.[8] |> int) - 1)
    filteredData


