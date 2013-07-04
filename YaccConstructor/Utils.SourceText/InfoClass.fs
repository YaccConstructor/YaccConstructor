module Yard.Utils.InfoClass

open System.Collections.Generic
open System.IO
open Yard.Utils.StructClass
open Yard.Utils.SourceText


let rec getLine (map : array<_>) id offset left right = //BinarySearch         
    let center = (left + right) / 2
    if offset >= map.[center] && offset < map.[center + 1] 
    then center        
    elif offset < map.[center]
    then getLine map id offset left center
    else getLine map id offset center right

type ProjInfo () =
    let FilesMap : Dictionary<_,_>  = new Dictionary<_,_> () //Dictionary<int/*id файла*/, string/*имя файла*/>
    let LinesMap : Dictionary<int<id>, array<int64<symbol>>> = new Dictionary<_,_>() //Dictionary<int /*id*/, /*array<_>*/
    member this.AddLine id lineMap = LinesMap.Add(id, lineMap)    
    member public this.GetMap (streamElement : StreamReader) = 
        try
            let mutable sum = 0L<symbol>
            let mutable beg = false
            let list = new ResizeArray<_>()
            if (streamElement.Peek() >= 0)
            then list.Add(sum)
            while streamElement.Peek() >= 0 do
                let num = streamElement.Read()
                sum <- sum + 1L<symbol>
                if num = 10
                then list.Add(sum)
            list.Add(sum)    
            list.ToArray()
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

    member this.GetAbsoluteOffset t =
        let repack = RePack t
        let id = repack.Id
        let line = repack.Line
        let column = repack.Column   
        let map = LinesMap.[id]
        if line > (map.Length - 1) * _line  || line < 1<line>
        then failwith ("Value of Line is out of Map's range")    
        if (int64 column) * _symbolL > map.[int line] - map.[int line - 1] - 1L * _symbolL || column < 1<symbol> 
        then failwith ("Column is out of Map's range")       
        let res = Some(map.[int line - 1] + (int64 column)*_symbolL - 1L<symbol>)
        if res |> Option.isSome
        then res |> Option.get        
        else failwith ("There is no such line or column")

    member this.GetCoordinates pair = 
        let repack = RePackPair pair
        let id = repack.Id
        let offset = repack.AbsoluteOffset
        let map = LinesMap.[id]
        if offset > map.[map.Length] - 1L<symbol>
        then failwith ("Offset is too big")
        let inLine = (getLine map id offset 0 (map.Length - 1)) + 1
        let inColumn = offset - map.[inLine - 1] + 1L<symbol>
        new Trinity(id, (int inColumn) * _symbol, inLine * _line)