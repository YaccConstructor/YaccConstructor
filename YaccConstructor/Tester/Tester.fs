//let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
//let s = tester.RunTest

// Maybe we should use fsx?
// http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx
// Right-click it in Windows Explorer and choose "Run with F# Interactive.."

open System.IO
open System.Diagnostics
open System.Text.RegularExpressions
open System.Configuration
open Yard.Core
open Microsoft.FSharp.Text

let () = 
    let command = ref "gi"
    let baseString = ref ""
    let increment = ref ""
    let outFolder = ref "PerformanceTests"
    let outFile = ref "in"
    let counter = ref 0

    let commandLineSpecs =
        ["-cmd",  ArgType.String (fun s -> command := s), "Command"    
         "-base", ArgType.String (fun s -> baseString := s), "Base string"
         "-is", ArgType.String (fun s -> increment := s), "Increment"
         "-of", ArgType.String (fun s -> outFolder := s), "Name of output folder"
         "-o", ArgType.String (fun s -> outFile := s), "Name prefix of output file"
         "-cnt", ArgType.Int (fun i -> counter := i), "Repeat counter"
        ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    let commandLineArgs = System.Environment.GetCommandLineArgs()
    ArgParser.Parse commandLineSpecs

    let (|Gi|Pt|Other|) (input:string) =
        match input.ToLower() with
        | "gi" -> Gi
        | "pt" -> Pt
        | _    -> Other

    let generateTests () = 
        for i in [0..!counter-1] do
            let str = !baseString + String.replicate i !increment
            let fileName = !outFile + "_" + i.ToString() + ".in"
            System.IO.Directory.CreateDirectory(!outFolder) |> ignore
            let outPath = !outFolder + "/" + fileName            
            try
                let t = new FileInfo(outPath)
                use writer = t.CreateText()
                writer.Write(str)
                writer.Close()
            with e -> failwith ("Writer Exception:" + e.ToString())            

    match !command with
    | Gi    -> generateTests ()
    | Pt    -> PerformanceTests.Run()
    | Other -> printf "Incorrect command" 