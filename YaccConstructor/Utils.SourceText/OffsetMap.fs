module Yard.Utils.OffsetMap

open System
open System.IO

type Coord = {Line : int; Column : int}

let getMap path = 
    try
        let lines = System.IO.File.ReadLines path
        let sum = ref 0
        let res = seq { yield 0
                        yield! (lines |> Seq.map(fun str -> 
                                                        let r = sum.Value + str.Length + 1
                                                        sum := r
                                                        r))                                                   
                      }
        res |> Seq.toArray
    with
    | :? DriveNotFoundException
    | :? DirectoryNotFoundException
        -> printfn "Unhandled Drive or Directory not found exception"
           [||]
    | :? FileNotFoundException as ex
        -> printfn "Unhandled FileNotFoundException: %s" ex.Message
           [||]
    | _ as ex
        -> printfn "Unhandled Exception: %s" ex.Message
           [||]

let getAbsoluteOffset (map : array<_>) line column =
    if line > map.Length - 1  || line < 1 then
        failwith ("Value of Line is out of Map's range")
    if column > map.[line] - map.[line - 1] - 1 || column < 1 then
        failwith ("Column is out of Map's range")
    let res = Some(map.[line - 1] + column - 1)
    if res |> Option.isSome
    then
        res |> Option.get
    else
        failwith ("There is no such line or column")       
        
               
let rec getLine (map : array<_>) offset left right = 
    if offset > map.[right] - 1 then
        failwith ("Offset is too big")
    let center = (left + right) / 2
    if offset >= map.[center] && offset < map.[center + 1] 
    then
        center
    else
        if offset < map.[center]
        then
            getLine map offset left center
        else
            getLine map offset center right


let getCoordinates (map : array<_>) offset = 
    let inLine = getLine map offset 0 (map.Length - 1) + 1
    let inColumn = offset - map.[inLine - 1] + 1
    let res = Some({Line = inLine; Column = inColumn})
    if res |> Option.isSome 
    then
        res |> Option.get
    else
        failwith ("Offset is out of Map's range") 
