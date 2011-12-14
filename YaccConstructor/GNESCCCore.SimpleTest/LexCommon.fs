module LexCommon

open Microsoft.FSharp.Text
open Yard.Generators.GNESCCGenerator
open Microsoft.FSharp.Reflection

type MyLexeme (tag,_value) =
    member self.MValue = _value
    interface ILexeme with    
       member self.tag = tag
       member self.CompareTo x =  compare (hash self) (hash x)
    end

type Lexer<'lexType>(path, getTag) = 
    let toLexerTag = 
        let targetUCIs = 
            FSharpType.GetUnionCases(typeof<'lexType>) 
            |> Array.map (fun uci -> (uci.Name,  FSharpValue.PreComputeUnionConstructor(uci)) ) 
            |> dict

        //printfn "%A" targetUCIs

        fun (name:string) ->
            //printfn "%s" name
            let caseCtor = targetUCIs.["T_" + name]
            (caseCtor [||]):?> 'lexType
            |> (fun x -> MyLexeme(getTag x, name) :> ILexeme)

    let locBuf =
        System.IO.File.ReadAllText(path)
            .Split([|' '|])
        |> Array.map toLexerTag

    interface ILexer with
        member self.Get pos = 
            if (pos <> Array.length locBuf + 1) then
                locBuf.[pos-1]
            else
                MyLexeme(Constants.gnesccEndStreamTag, "") :> ILexeme
    end
