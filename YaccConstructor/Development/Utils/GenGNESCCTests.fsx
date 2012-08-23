#I "../../Main/bin/Release/"
#r "Common.dll"
#r "YardFrontend.dll"
#r "GNESCCGenerator.dll"

open Yard.Core
open System.IO

let fe = Yard.Frontends.YardFrontend.YardFrontend()
let be = Yard.Generators.GNESCCGenerator.GNESCCGenerator()

#load "Test.fsx"
open Test

#load "Tests.fsx"
open Tests

let testPath = @"../../../Tests/GNESCC/"
let gtPath = "../../GNESCCGeneratedTests"
let targetPath = "../../GNESCCCore.Test"
let ARf = "GNESCC.Actions"
let TRf = "GNESCCGenerator.Tables"
let RRf = "GNESCC.Regexp"


let generate test = 
    printfn "geterate test: %A\n" test.FullGrammarPath
    (fe:>Frontend).ParseGrammar test.FullGrammarPath |> (be:>Generator).Generate |> ignore

let move () =
    try     
        Directory.Delete(gtPath,true)
    with
    | x -> printfn "Cannot remove directory: %A" x.Message

    Directory.CreateDirectory(gtPath)
    |> ignore
    let filesPaths = Directory.GetFiles(testPath,"*.fs", SearchOption.AllDirectories) 
    let lexFiles = Directory.GetFiles(testPath,"*.fsl", SearchOption.AllDirectories)
    printfn "move files: \%An" filesPaths    
    Seq.iter (fun x -> printfn "path : %s" (Path.Combine (gtPath,Path.GetFileName(x))); File.Copy(x,Path.Combine (gtPath,Path.GetFileName(x)), true)) lexFiles
    Seq.iter (fun x -> printfn "path : %s" (Path.Combine (gtPath,Path.GetFileName(x))); File.Move(x,Path.Combine (gtPath,Path.GetFileName(x)))) filesPaths

let replace () =
    Directory.GetFiles(gtPath,"*.fs", SearchOption.AllDirectories)
    |> Seq.iter
        (fun path ->
            printfn "replace path: %A\n" path
            let content = File.ReadAllText(path)
            let info = (Path.GetFileName path).Split('.')
            let test = 
                List.find 
                    (fun test ->                        
                        (Path.GetFileNameWithoutExtension test.FullGrammarPath) = info.[0]
                    )
                    tests
            let newContent = 
                if info.[2] = "actions" 
                then content.Replace (ARf, snd test.ActionReplacement) 
                elif info.[2] = "regexp" 
                   then content.Replace (RRf, snd test.RegexpReplacement) 
                   else content.Replace (TRf, snd test.TablesReplacement)
            let outStrieam =         
                try
                    let t = new FileInfo(Path.Combine (targetPath, Path.GetFileName path))
                    let writer = t.CreateText()             
                    writer     
                with e -> failwith ("Writer Exception:" + e.ToString())
                         
            let write (str:string) = outStrieam.Write(str)                        
            write(newContent)
            outStrieam.Close())

    Directory.GetFiles(gtPath, "*.fsl", SearchOption.AllDirectories)
    |> Seq.iter (fun x -> printfn "path : %s" (Path.Combine (gtPath,Path.GetFileName(x))); File.Copy(x,Path.Combine (targetPath,Path.GetFileName(x)),true)) 

do Seq.iter generate tests
   move()
   replace() 