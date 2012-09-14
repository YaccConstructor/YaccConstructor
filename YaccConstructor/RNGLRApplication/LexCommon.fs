module LexCommon

open Microsoft.FSharp.Text
open Yard.Generators.RNGLR.Parser
open Microsoft.FSharp.Reflection

let tokens<'lexType>(path) = 
    let toLexerTag = 
        let targetUCIs = 
            FSharpType.GetUnionCases(typeof<'lexType>) 
            |> Array.map (fun uci -> (uci.Name,  FSharpValue.PreComputeUnionConstructor(uci)) ) 
            |> dict

        printfn "%A" targetUCIs

        fun (name:string) ->
            printfn "%s" name
            let caseCtor = targetUCIs.[name]
            (caseCtor [|2|]) :?> 'lexType

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.map toLexerTag