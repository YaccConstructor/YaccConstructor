namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
//open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser

type SupportedLangs =
    | Calc
    | TSQL
    | JSON

type Processor(file) =
    let mutable xmlPath = ""
    let mutable forest = []
    let mutable language = Calc
    
    ///Needs for tree generation for highlighting
    let mutable unprocessed = []
    let mutable count = 0

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" ->JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError translate printer = 
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
             | Yard.Generators.RNGLR.Parser.Success (ast, errors) -> 
                    forest <- [ast, errors] @ forest
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )
        
    let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
        let ranges = 
            brs |> Seq.groupBy (fun x -> x.back_ref)
            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
            |> Seq.map(fun brs ->
                try
                    let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                    let lengthTok = pos.Length
                    let beginPosTok = pos.[0] + 1
                    let endPosTok = pos.[lengthTok-1] + 2 
                    let endPos = 
                        brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                        - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                    brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                with
                | e -> 
                    brs.[0].back_ref.GetDocumentRange())
        ranges    

//(provider: ICSharpContextActionDataProvider) = 
    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
            res.ToArray()
        //let sourceFile = provider.SourceFile
        //let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate defLang

        let addError tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
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
        
        let addErrorJSON tok = 
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            match tok with
            | JSON.Parser.NUMBER (l,br) -> e "NUMBER" l br
            | JSON.Parser.STRING1 (l,br) -> e "STRING1" l br
            | _ -> failwith "error in addErrorJSON function"

        
        let addErrorTSQL tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
                calculatePos brs 
                |> Seq.iter (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            match tok with
            | DEC_NUMBER (sourceText,brs)   -> e "DEC_NUMBER" sourceText.text brs
            | DOUBLE_COLON (sourceText,brs) -> e "DOUBLE_COLON" sourceText.text brs
            | GLOBALVAR (sourceText,brs)    -> e "GLOBALVAR" sourceText.text brs
            | IDENT (sourceText,brs)        -> e "IDENT" sourceText.text brs
            | LOCALVAR (sourceText,brs)     -> e "LOCALVAR" sourceText.text brs
            | RNGLR_EOF (sourceText,brs) -> e "EOF" sourceText.text brs
            | STOREDPROCEDURE (sourceText,brs) -> e "STOREDPROCEDURE" sourceText.text brs
            | STRING_CONST (sourceText,brs) -> e "STRING_CONST" sourceText.text brs
            | WEIGHT (sourceText,brs) -> e "WEIGHT" sourceText.text brs
            | _ -> failwith "error in addErrorTSQL function"

        graphs
        |> ResizeArray.iter 
            (fun (lang, graph) ->
                match lang with
                | Calc -> 
                    xmlPath <- Calc.xmlPath
                    language <- Calc
                    processLang graph Calc.tokenize Calc.parse lexerErrors.Add  addError Calc.translate Calc.printAstToDot
                | JSON -> 
                    xmlPath <- JSON.xmlPath
                    language <- JSON
                    processLang graph JSON.tokenize JSON.parse lexerErrors.Add  addErrorJSON JSON.translate JSON.printAstToDot
                | TSQL -> 
                    xmlPath <- TSQL.xmlPath
                    language <- TSQL
                    processLang graph TSQL.tokenize TSQL.parse lexerErrors.Add  addErrorTSQL TSQL.translate TSQL.printAstToDot
            )

        lexerErrors, parserErrors

    member this.XmlPath = xmlPath
    
    member this.GetNextTree() = 
        let translate = 
            match language with
            | Calc -> Calc.translate
            | JSON -> JSON.translate
            | TSQL -> TSQL.translate

        if forest.Length <= count 
        then null
        else 
            let mutable curAst, errors = List.nth forest count
            let mutable unproc = 
                if unprocessed.Length = 0
                then Array.init curAst.TokensCount (fun i -> i) |> List.ofArray
                else unprocessed
            
            let nextTree, unproc = curAst.GetNextTree unproc (fun _ -> true)
            if unproc.Length = 0
            then 
                count <- count + 1
            unprocessed <- unproc
            
            let treeNodeList = translate nextTree errors :> seq<ITreeNode>
            treeNodeList.ToTreeNodeCollection().First()


    member this.GetForestWithToken range = 
        let translate = 
            match language with
            | Calc -> Calc.translate
            | JSON -> JSON.translate
            | TSQL -> TSQL.translate

        let calcPos token  = 
            let tokenData = 
                match language with
                | Calc -> Calc.AbstractParser.tokenData
                | JSON -> JSON.Parser.tokenData
                | TSQL -> tokenData
            
            let (_ : string), (position : AbstractLexer.Core.Position<ICSharpLiteralExpression>[]) 
                = unbox <| tokenData token
                
            calculatePos position

        let res = new ResizeArray<_>()
        for ast, errors in forest do
            let trees = ast.GetForestWithToken range calcPos
            for tree in trees do
                let treeNodeList = translate tree errors :> seq<ITreeNode>
                res.Add <| treeNodeList.ToTreeNodeCollection().First()
        res