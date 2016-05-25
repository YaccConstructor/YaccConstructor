module LexCommon

open Microsoft.FSharp.Text
//open Yard.Generators.RNGLR.EBNF.Parser
open Microsoft.FSharp.Reflection

let tokens<'lexType>(path, genLiteral) = 
    
    let toLexerTag = 
        let targetUCIs = 
            FSharpType.GetUnionCases(typeof<'lexType>) 
            |> Array.map (fun uci -> (uci.Name,  FSharpValue.PreComputeUnionConstructor(uci)) ) 
            |> dict

        let getNum =
            let exists, numCtor = targetUCIs.TryGetValue "NUM"
            if not exists then
                fun _ -> None
            else System.Int32.TryParse >> function
                | (true, x) -> 
                    (numCtor [|x|]) :?> 'lexType
                    |> Some
                | (false, _) -> None
        
        let curI = ref 0

        //there are string->data->, and we swap it
        let genLiteral x y = genLiteral y x
    
        let (|Literal|_|) = genLiteral !curI
        let (|NumberToken|_|) = getNum
        
        printfn "%A" targetUCIs
        fun (name:string) ->
            printf "%s " name
            incr curI
            match name with
            | Literal x | NumberToken x -> x
            | _ ->
                let caseCtor = targetUCIs.[name]
                (caseCtor [|!curI|]) :?> 'lexType

    System.IO.File.ReadAllText(path)
        .Split([|' '|])
    |> Array.filter ((<>) "")
    |> Array.map toLexerTag
    |> (fun x -> printf "\n"; x)