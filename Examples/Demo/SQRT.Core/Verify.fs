module SQRT.Core.Verify

open Yard.Examples
open SQRT.Core.Context
open Yard.Utils.SourceText
open Yard.Utils.StructClass
open Yard.Utils.InfoClass
open LexerHelper

let Verify srcFilePath (dataContext:DataContext) =
    let parseRes = MSSqlParser.justParse srcFilePath
    match parseRes with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
        let print = 
            tokenPos 
            >> (fun(x,y) -> 
                let x = RePack x
                let y = RePack y
                sprintf "(%A,%A) - (%A,%A)" (x.Line + 1<line>) x.Column (y.Line + 1<line>) y.Column)
        new ResizeArray<_>([sprintf "Error in file %s on position %s on Token %A: %s" srcFilePath (print tok) (tok.GetType()) msg])
    | Yard.Generators.RNGLR.Parser.Success ast ->
        //ast.collectWarnings (fun x -> 0,0)
        //TODO Do something useful
        if dataContext.AstPerFile.ContainsKey(srcFilePath)
        then dataContext.AstPerFile.[srcFilePath] <-ast
        else dataContext.AstPerFile.Add(srcFilePath,ast)
        new ResizeArray<_>([])

