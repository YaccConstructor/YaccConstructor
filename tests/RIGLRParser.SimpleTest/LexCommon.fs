module LexCommon

open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection

let tokens<'lexType>(path) = 
    let toLexerTag = 
        let targetUCIs = 
            FSharpType.GetUnionCases(typeof<'lexType>) 
            |> Array.map (fun uci -> (uci.Name,  FSharpValue.PreComputeUnionConstructor(uci)) ) 
            |> dict

//        printfn "%A" targetUCIs
        let curI = ref 0
        fun (name:string) ->
//            printf "%s " name
            let caseCtor = targetUCIs.[name]
            incr curI
            (caseCtor [|!curI|]) :?> 'lexType

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.filter ((<>) "")
    |> Array.map toLexerTag
    |> (fun x -> printf "\n"; x)