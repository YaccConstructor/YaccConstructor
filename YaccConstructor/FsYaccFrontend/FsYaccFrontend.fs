namespace Yard.Frontends.FsYaccFrontend

open Yard.Core

type FsYaccFrontend() = 
    interface IFrontend with
        member this.Name = "FsYaccFrontend"
        member this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end

// For testing switch to Console App and then switch back to Class Library
// Delete action code from yard_option_8
// Add %token HEAD
// Add <string> to some terminals
// Remove %prec TOKEN

module Run = 

    open Microsoft.FSharp.Text.Lexing

    FrontendsManager.Register(new FsYaccFrontend()) // Not register itself automatically
    
//    let filename = @"..\..\..\AntlrToYard\Parser.fsy" 
    let filename = @"..\..\..\..\Tests\FsYacc\5.fsy" 
//    let content = System.IO.File.ReadAllText(filename)
//    Lexer.source := content
//    let reader = new System.IO.StringReader(content)
//    let lexbuf = LexBuffer<_>.FromTextReader reader//LexBuffer<_>.FromChars  ("abc/* def */foo".ToCharArray())
//    let lexems = seq { 
//                       while not lexbuf.IsPastEndOfStream do
//                             yield Lexer.token lexbuf  
//                      }
//    lexems |> Seq.iter (fun x -> printf "%A ; " x)
    printf "%A\n" <| Yard.Frontends.FsYaccFrontend.Main.ParseFile filename
    ()