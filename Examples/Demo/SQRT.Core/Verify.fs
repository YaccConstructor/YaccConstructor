module SQRT.Core.Verify

open Yard.Examples

let Verify srcFilePath =
    let parseRes = MSSqlParser.justParse srcFilePath
    match parseRes with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
        new ResizeArray<_>([sprintf "Error in file %s on position %d on Token %A: %s" srcFilePath num tok msg])
    | Yard.Generators.RNGLR.Parser.Success ast ->
        //ast.collectWarnings (fun x -> 0,0)
        //TODO Do something useful
        new ResizeArray<_>([])

