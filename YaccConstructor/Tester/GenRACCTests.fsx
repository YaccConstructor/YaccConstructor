#I "../Main/bin/Release/"
#r "Common.dll"
#r "YardFrontend.dll"
#r "RACCGenerator.dll"

open Yard.Core
open System.IO

let fe = Yard.Frontends.YardFrontend.YardFrontend()
let be = Yard.Generators.RACCGenerator.RACCGenerator()

type test = 
    {
        FullPath          : string
        ActionReplacement : string*string
        TablesReplacement : string*string
    }

let testPath = @"../../Tests/RACC/"
let gtPath = "../RACCGeneratedTests"
let targetPath = "../RACCCore.Test"
let ARf = "RACC.Actions"
let TRf = "RACCGenerator.Tables"

let tests = 
    [
        {
            FullPath = Path.Combine(testPath,@"test_seq\test_seq.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Seq")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Seq")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_alt\test_alt.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Alt")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Alt")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_cls\test_cls.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Cls")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Cls")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_alt_in_cls\test_alt_in_cls.yrd")
            ActionReplacement = (ARf,"RACC.Actions_alt_in_cls")
            TablesReplacement = (TRf,"RACCGenerator.Tables_alt_in_cls")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_cls_with_tail\test_cls_with_tail.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Cls_tail")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Cls_tail")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_cls_with_head\test_cls_with_head.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Cls_head")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Cls_head")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_arithm_glr\test_arithm_glr.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Arithm_glr")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Arithm_glr")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_l_attr\test_l_attr.yrd")
            ActionReplacement = (ARf,"RACC.Actions_L_attr")
            TablesReplacement = (TRf,"RACCGenerator.Tables_L_attr")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_simple_checker\test_simple_checker.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Simple_checker")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Simple_checker")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_checker_on_glr\test_checker_on_glr.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Checker_on_glr")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Checker_on_glr")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_summator_1\test_summator_1.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Summator_1")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Summator_1")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_opt\test_opt.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Opt")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Opt")
        }
        ;{
            FullPath = Path.Combine(testPath,@"test_reduce_reduce\test_reduce_reduce.yrd")
            ActionReplacement = (ARf,"RACC.Actions_Rdc_Rdc")
            TablesReplacement = (TRf,"RACCGenerator.Tables_Rdc_Rdc")
        }
        ;{
            FullPath = Path.Combine(testPath,@"claret\braces_1\test_simple_braces.yrd")
            ActionReplacement = (ARf,"RACC.Actions_claret_1")
            TablesReplacement = (TRf,"RACCGenerator.Tables_claret_1")
        }
        ;{
            FullPath = Path.Combine(testPath,@"claret\braces_2\test_simple_braces_2.yrd")
            ActionReplacement = (ARf,"RACC.Actions_claret_2")
            TablesReplacement = (TRf,"RACCGenerator.Tables_claret_2")
        }
    ]

let generate test = 
    printfn "geterate test: %A\n" test.FullPath
    (fe:>IFrontend).ParseGrammar test.FullPath |> (be:>IGenerator).Generate |> ignore

let move () = 
    Directory.Delete(gtPath,true)
    Directory.CreateDirectory(gtPath)
    |> ignore
    let filesPaths = Directory.GetFiles(testPath,"*.fs", SearchOption.AllDirectories)
    printfn "move files: \%An" filesPaths    
    Seq.iter (fun x -> File.Move(x,Path.Combine (gtPath,Path.GetFileName(x)))) filesPaths

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
                        (Path.GetFileNameWithoutExtension test.FullPath) = info.[0]
                    )
                    tests
            let newContent = 
                if info.[2] = "actions" 
                then content.Replace (ARf, snd test.ActionReplacement) 
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

do Seq.iter generate tests
   move()
   replace() 