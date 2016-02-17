namespace SeqExtension

open System.Collections.Generic

[<RequireQualifiedAccessAttribute>]
module Seq = 
    let filterAndMap filter map s = 
        
        let inputList = s |> List.ofSeq
        
        let fFoldBack elem acc = 
            if filter elem 
            then (map elem) :: acc
            else acc
        
        List.foldBack fFoldBack inputList []

[<RequireQualifiedAccessAttribute>]
module List = 
    let intersect (one : 'a list) (two : 'a list) = 
        one 
        |> List.filter (fun elem1 -> List.exists ((=) elem1) two)