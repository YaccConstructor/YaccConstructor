namespace Yard.Frontends.CompressionFrontend

open Yard.Core
open Yard.Frontends.CompressionFrontend.Compression

type CompressionFrontend() = 
    inherit Frontend()
        override this.Name = "CompressionFrontend"
        override this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> compress s '&'
            | (:?((string * char)) as t) -> compress (fst t) (snd t)
            | (:? (string list) as lst) -> compressList lst
            | _ -> IL.emptyGrammarDefinition
        override this.ProductionTypes =
            Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>
            |> List.ofArray
            |> List.map (fun unionCase -> unionCase.Name)
        override this.ParseGrammarFromStr str = 
            compress str '&'
    
