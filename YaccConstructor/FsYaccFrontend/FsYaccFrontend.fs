namespace Yard.Frontends.FsYaccFrontend

open Yard.Core

type FsYaccFrontend() = 
    interface IFrontend with
        member this.Name = "FsYaccFrontend"
        member this.ParseGrammar t = 
            match t with
//            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.Definition.empty
        member this.ProductionTypes = List.ofArray(Reflection.FSharpType.GetUnionCases typeof<IL.Production.t<string,string>>) |> List.map (fun unionCase -> unionCase.Name)
    end

// For testing switch to Console App and then switch back to Class Library
module Run = 

    open Microsoft.FSharp.Text.Lexing

    FrontendsManager.Register(new FsYaccFrontend()) // Not register itself automatically
    
    let content = System.IO.File.ReadAllText(@"..\..\..\AntlrToYard\Parser.fsy")
    Lexer.source := content
    let reader = new System.IO.StringReader(content)
    let lexbuf = LexBuffer<_>.FromTextReader reader//LexBuffer<_>.FromChars  ("abc/* def */foo".ToCharArray())
    let lexems = seq { 
                       while not lexbuf.IsPastEndOfStream do
                             yield Lexer.token lexbuf  
                      }
    lexems |> Seq.iter (fun x -> printf "%A ; " x)
    //let filename = @"..\..\..\..\Tests\Basic\test_include\test_include_main.yrd" 
    let filename = @"..\..\..\..\Tests\RACC\test_arithm_glr\test_arithm_glr.yrd" 
//    printf "%A\n" <| Yard.Frontends.FsYaccFrontend.Main.ParseFile filename
    ()