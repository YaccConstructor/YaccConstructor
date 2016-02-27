namespace SeqExtension

[<RequireQualifiedAccessAttribute>]
module List = 
    let intersect (one : 'a list) (two : 'a list) = 
        one 
        |> List.filter (fun elem1 -> List.exists ((=) elem1) two)