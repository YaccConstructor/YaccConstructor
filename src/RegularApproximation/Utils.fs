/// General purpose utilities
module Utils

open System.IO

/// Extension methods for .net Dictionary class
module Dictionary =
    open System.Collections.Generic

    let dictFromSeq (vals: seq<'a * 'b>) =
        let dict = Dictionary()
        vals |> Seq.iter (fun (a, b) -> dict.[a] <- b)
        dict
    let addToSetInDict key elemForSet (dict: Dictionary<_, HashSet<_>>) =
        match dict.TryGetValue key with 
        | true, hs -> do hs.Add elemForSet |> ignore
        | false, _ -> 
            let hs = HashSet()
            do hs.Add elemForSet |> ignore
            do dict.[key] <- hs
    let mergeDicts (dict1: Dictionary<'a, 'b>) (dict2: Dictionary<'a, 'c>) =
        let resDict = Dictionary()
        do dict1
        |> Seq.iter 
            (
                fun (KeyValue(a, b)) -> 
                    match dict2.TryGetValue a with
                    | true, c -> resDict.[b] <- c
                    | false, _ -> failwith "dict1 contains elements that dict2 doesn't"
            )
        resDict
    /// Gets the set corresponding to passed key and checks it's size.
    /// If set contains the only element this element is returned,
    /// otherwise exception is thrown
    let getMappingToOne key (dict: Dictionary<'k, HashSet<'v>>) =
        let oneToOneMsg = "one to one mapping expected"
        match dict.TryGetValue key with
        | true, hSet ->
            if hSet.Count <> 1
            then failwith oneToOneMsg
            else hSet |> Seq.head
        | _ -> failwith oneToOneMsg

module List =
    let cons x xs = x :: xs
        
module Seq =
    let unfoldS generator state =
        Seq.unfold (generator >> Option.map (fun (u, s) -> (u, s), s)) state

/// Converts passed function to one with parameters in reversed order
let flip f = fun x y -> f y x

/// Utility function for custom equality and comparison implementation
let applyToMappedTypedArgs f mapper (arg1: 'a) (arg2: obj) typingFailedFunc =
    match arg2 with
    | :? 'a as arg2Typed-> f (mapper arg1) (mapper arg2Typed)
    | _ -> typingFailedFunc ()

let (===) = LanguagePrimitives.PhysicalEquality

let myDebugFolderPath = "E:\\Diploma\\Debug"
let myDebugFilePath fileName = Path.Combine (myDebugFolderPath, fileName)