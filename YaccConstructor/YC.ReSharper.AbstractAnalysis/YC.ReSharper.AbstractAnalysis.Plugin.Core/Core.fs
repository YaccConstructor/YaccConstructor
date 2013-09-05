namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Daemon.CSharp.Stages
open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Feature.Services.CSharp.Bulbs
open JetBrains.ReSharper.Feature.Services.LinqTools
open JetBrains.ReSharper.Intentions.Extensibility
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages

type SupportedLangs =
    | Calc
    | TSQL

type Processor(file) =
    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let porcessLang graph tokenize parse addLError addPError = 
        let tokenize g =
            try 
               tokenize g
               |> Some 
            with
            | Calc.Lexer.LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
                |> addLError
                None
        tokenize graph |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success(_,_) -> ()
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> addPError tok
            )
            
//(provider: ICSharpContextActionDataProvider) = 
    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        //let sourceFile = provider.SourceFile
        //let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate defLang
        let addError tok =
            let e t l (br:array<AbstractLexer.Core.Position<#ITreeNode>>) = parserErrors.Add <| ((sprintf "%A(%A)" t l), br.[0].back_ref.GetDocumentRange())
            match tok with
            | Calc.AbstractParser.MINUS (l,br) -> e "MINUS" l br
            | Calc.AbstractParser.DIV (l,br) -> e "DIV" l br
            | Calc.AbstractParser.PLUS (l,br) -> e "PLUS" l br
            | Calc.AbstractParser.NUMBER (l,br) -> e "NUMBER" l br
            | Calc.AbstractParser.LBRACE (l,br) -> e "LBRACE" l br
            | Calc.AbstractParser.RBRACE (l,br) -> e "RBRACE" l br
            | Calc.AbstractParser.POW (l,br) -> e "POW" l br
            | Calc.AbstractParser.RNGLR_EOF (l,br) -> e "EOF" l br
            | Calc.AbstractParser.ERROR (l,br) -> e "ERROR" l br
            | Calc.AbstractParser.MULT (l,br) -> e "MULT" l br
        
        let addErrorTSQL tok =  ()
            //let data = Yard.Examples.MSParser.tokenData tok
            //data.
        graphs
        |> ResizeArray.iter 
            (fun (l,g) ->
                match l with
                | Calc -> porcessLang g Calc.tokenize Calc.parse lexerErrors.Add  addError
                | TSQL -> porcessLang g TSQL.tokenize TSQL.parse lexerErrors.Add  addErrorTSQL  )

        lexerErrors,parserErrors