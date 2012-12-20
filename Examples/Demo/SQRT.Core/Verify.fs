module SQRT.Core.Verify

open Yard.Examples
open SQRT.Core.Context

let Verify srcFilePath (dataContext:DataContext) =
    let parseRes = MSSqlParser.justParse srcFilePath
    match parseRes with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
        new ResizeArray<_>([sprintf "Error in file %s on position %d on Token %A: %s" srcFilePath num tok msg])
    | Yard.Generators.RNGLR.Parser.Success ast ->
        //ast.collectWarnings (fun x -> 0,0)
        //TODO Do something useful
        if dataContext.AstPerFile.ContainsKey(srcFilePath)
        then dataContext.AstPerFile.[srcFilePath] <-ast
        else dataContext.AstPerFile.Add(srcFilePath,ast)
        new ResizeArray<_>([])

