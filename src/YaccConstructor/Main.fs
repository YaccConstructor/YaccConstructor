module Main

open YaccConstructor.Common
open YaccConstructor.API

open Yard.Frontends.FsYaccFrontend
open Yard.Frontends.YardFrontend

open Yard.Generators.RNGLR
open Yard.Generators.GLL

open Yard.Core
open Yard.Core.Conversions

open System.Collections.Generic

open Argu

[<EntryPoint>]
let main args = 
    let frontends = ([new FsYaccFrontend(); new YardFrontend()]: Frontend list)
                    |> List.map (fun f -> (f.Name, f))
                    |> fun feList -> ("", new YardFrontend() :> Frontend) :: feList
                    |> dict

    let generators = ([new RNGLR(); new GLL()]: Generator list)
                     |> List.map (fun g -> (g.Name, g))
                     |> fun gList -> ("", new RNGLR() :> Generator) :: gList
                     |> dict

    let conversions =  ([new AddDefaultAC.AddDefaultAC(); new AddEOF.AddEOF(); new BuildAST.BuildAST(); new BuildAstSimple.BuildAstSimple(); new CNFandBNF.CNF();
                         new CNFandBNF.BNFconj(); new CNFandBNF.BNFbool(); new EliminateLeftRecursion.EliminateLeftRecursion(); new RegularApproximation.RegularApproximation();
                         new ExpandTopLevelAlt.ExpandTopLevelAlt(); new ExpandBrackets.ExpandBrackets(); new ExpandEbnfStrict.ExpandEbnf(); new ExpandInnerAlt.ExpandInnerAlt();
                         new ExpandMeta.ExpandMeta(); new LeaveLast.LeaveLast(); new MergeAlter.MergeAlter(); new RemoveAST.RemoveAC(); new ExpandInline.ReplaceInline();
                         new ReplaceLiterals.ReplaceLiterals(); new Linearize.Linearize(); new ExpandRepet.ExpandExpand(); new ExpandConjunction.ExpandConjunction()]: Conversion list)
                       |> List.map (fun c -> (c.Name, c))
                       |> dict 

    let feName = ref "YardFrontend"
    let generatorName = ref "RNGLRGenerator"
    let generatorParams = ref None
    let conversionNames = new HashSet<string>()
    let sourceFile = ref "grammar.yrd"
    let sourceDirectory = ref "./"

    let argv = System.Environment.GetCommandLineArgs()
    let parser = ArgumentParser.Create<CLIArguments>(errorHandler = ProcessExiter())
    let args = parser.Parse argv.[1..]
    for res in args.GetAllResults() do
        match res with 
        | Frontend fe -> 
            feName := fe
        | AvailableFrontends -> 
            printfn "Available frontends %s" (frontends.Keys |> String.concat ", ")
        | Generator g -> 
            match Array.toList (g.Split ' ') with
            | name::[] -> 
               generatorName := name
            | name::parameters -> 
               generatorName := name
               generatorParams := Some (String.concat " " parameters)
            | _ -> failwith "You need to specify generator name"
        | AvailableGenerators -> 
            printfn "Available generators %s" (generators.Keys |> String.concat ", ")
        | Conversion c -> 
            conversionNames.Add c |> ignore
        | AvailableConversions -> 
            printfn "Available conversions %s" (conversions.Keys |> String.concat ", ")
        | DefConstant d -> 
            printfn "-d is not supported"
        | UndefConstant u -> 
            printfn "-u is not supported"
        | Input i -> 
            sourceFile := System.IO.Path.GetFileName i
            sourceDirectory := System.IO.Path.GetDirectoryName i
    
    generateToFile (System.IO.Path.Combine(!sourceDirectory, !sourceFile))
                   (frontends.[!feName])
                   (generators.[!generatorName])
                   (!generatorParams)
                   (conversionNames |> Seq.map (fun c -> conversions.[c]))
                   [||]
                   [||]
    0
